(in-package boot)

(defvar *builtins* (make-hash-table))

(defmacro defbuiltin (name lambda-list &body body)
  (let ((args-sym (gensym)))
    `(setf (gethash ',name *builtins*)
           (lambda (,args-sym)
             (destructuring-bind ,lambda-list ,args-sym
               ,@body)))))

(defun nuc-val<-int (nuc-val)
  (llvm::build-shl *builder*
                   nuc-val
                   (llvm-val<-int *lowtag-bits*)
                   "nuc-val<-int"))

(defun int<-nuc-val (int)
  ; TODO: type checking
  (llvm::build-l-shr *builder*
                     int
                     (llvm-val<-int *lowtag-bits*)
                     "int<-nuc-val"))

(defun nuc-val<-cons (cons)
  (llvm:build-or
    *builder*
    (llvm:build-pointer-to-int *builder* cons *nuc-val* "nuc-val<-cons")
    (llvm-val<-int #b010)
    "nuc-val<-cons"))

(defun cons<-nuc-val (nuc-val)
  ; TODO: type checking
  (llvm:build-int-to-pointer
    *builder*
    (llvm:build-and *builder*
                    nuc-val
                    (llvm-val<-int (lognot (1- (ash 1 *lowtag-bits*))))
                    "cons<-nuc-val")
    *cons-cell-ptr*
    "cons<-nuc-val"))

(defmacro define-binary-op (name instruction)
  `(defbuiltin ,name (&rest operands)
     (let ((operands (mapcar (lambda (expr)
                               (int<-nuc-val (compile-expr expr)))
                             operands)))
       (nuc-val<-int
         (reduce (lambda (lhs rhs)
                   (,instruction *builder* lhs rhs (string ',name)))
                 operands)))))

(define-binary-op + llvm:build-add)
(define-binary-op - llvm:build-sub)
(define-binary-op * llvm:build-mul)
(define-binary-op >> llvm::build-l-shr)

(defmacro define-comparison (name)
  (let ((name-str (string name)))
    `(defbuiltin ,name (lhs rhs)
       (let ((lhs (compile-expr lhs))
             (rhs (compile-expr rhs))
             (true-block (llvm:append-basic-block *current-func*
                                                  (format nil "~S-true" ',name)))
             (false-block (llvm:append-basic-block *current-func*
                                                   (format nil "~S-false" ',name)))
             (after-block (llvm:append-basic-block *current-func*
                                                   (format nil "~S-after" ',name))))
         (llvm:build-cond-br
           *builder*
           ;; We don't bother shifting off the lowtag, as it doesn't effect ordering
           (llvm:build-i-cmp *builder* ,(intern name-str "KEYWORD")
                             lhs rhs ,name-str)
           true-block
           false-block)
         (llvm:position-builder-at-end *builder* true-block)
         (llvm:build-br *builder* after-block)
         (llvm:position-builder-at-end *builder* false-block)
         (llvm:build-br *builder* after-block)
         (llvm:position-builder-at-end *builder* after-block)
         (let ((phi (llvm:build-phi *builder* *nuc-val* (format nil "~S?" ',name))))
           (llvm:add-incoming phi
                              (list *true* *false*)
                              (list true-block false-block))
           phi)))))

(define-comparison <)
(define-comparison <=)
(define-comparison >)
(define-comparison >=)

(defbuiltin |if| (condition then-form else-form)
  ;; TODO: use phi
  (let ((then-block (llvm:append-basic-block *current-func* "if-then"))
        (else-block (llvm:append-basic-block *current-func* "if-else"))
        (after-block (llvm:append-basic-block *current-func* "if-after"))
        (if-value (llvm:build-alloca *builder* *nuc-val* "if-value")))
    (flet ((build-branch (form block)
             (llvm:position-builder *builder* block)
             (llvm:build-store *builder* (compile-expr form) if-value)
             (llvm:build-br *builder* after-block)))
      (llvm:build-cond-br
        *builder*
        (llvm:build-i-cmp *builder* :=
                          (compile-expr condition)
                          *true*
                          "condition-true?")
        then-block else-block)
      (build-branch then-form then-block)
      (build-branch else-form else-block)
      (llvm:position-builder *builder* after-block)
      (llvm:build-load *builder* if-value "if-value"))))

(defbuiltin |set| (name value)
  (let ((var (lookup-lvalue name))
        (compiled-expr (compile-expr value)))
    (llvm:build-store *builder* compiled-expr var)
    compiled-expr))

(defbuiltin |car| (cons)
  (let ((cons-ptr (cons<-nuc-val (compile-expr cons))))
    (llvm:build-load *builder*
                     (llvm:build-struct-gep *builder* cons-ptr 0 "car-ptr")
                     "car")))

(defbuiltin |cdr| (cons)
  (let ((cons-ptr (cons<-nuc-val (compile-expr cons))))
    (llvm:build-load *builder*
                     (llvm:build-struct-gep *builder* cons-ptr 1 "cdr-ptr")
                     "cdr")))

(defbuiltin |let| (clauses &body body)
  (let ((*env* (append
                 (mapcar
                   (lambda (clause)
                     (let* ((name (first clause))
                            (expr (second clause))
                            (var-on-stack (llvm:build-alloca *builder*
                                                             *nuc-val*
                                                             (string name))))
                       (llvm:build-store
                         *builder* (compile-expr expr) var-on-stack)
                       (cons name var-on-stack)))
                   clauses)
                 *env*)))
    (loop for cons on body
          for compiled-expr = (compile-expr (car cons))
          when (null (cdr cons))
            return compiled-expr)))

(defbuiltin |eq?| (a b)
  ;; TODO: use phi
  (let ((bool (llvm:build-alloca *builder* *nuc-val* "eq?-result"))
        (true-block (llvm:append-basic-block *current-func* "eq?-true"))
        (false-block (llvm:append-basic-block *current-func* "eq?-false"))
        (after-block (llvm:append-basic-block *current-func* "eq?-after")))
    (flet ((build-branch (block ret-val)
             (llvm:position-builder *builder* block)
             (llvm:build-store *builder* ret-val bool)
             (llvm:build-br *builder* after-block)))
      (llvm:build-cond-br
        *builder* (llvm:build-i-cmp
                    *builder* := (compile-expr a) (compile-expr b) "eq?-cmp")
        true-block false-block)
      (build-branch true-block *true*)
      (build-branch false-block *false*)
      (llvm:position-builder *builder* after-block)
      (llvm:build-load *builder* bool "eq?"))))

(defparameter *lambda-counter* 0)

(defbuiltin |lambda| (simple-lambda-list &body body)
  (let* ((name (intern (format nil "lambda_~D" (1- (incf *lambda-counter*)))))
         (current-block (llvm:insertion-block *builder*))
         (func (prog1
                 (declare-function (mangle-name name) (length simple-lambda-list))
                 (compile-defun name simple-lambda-list body))))
    ;; COMPILE-DEFUN will change the builders position.
    (llvm:position-builder-at-end *builder* current-block)
    (llvm:build-call *builder*
                     (extern-func "rt_make_lambda" *nuc-val* (llvm:int-type 64))
                     (list (llvm:build-pointer-to-int *builder* func
                                                      (llvm:int-type 64)
                                                      "func-pointer-to-int"))
                     "make-lambda")))

(defbuiltin |%raw-call| (name &rest args)
  (llvm:build-call
    *builder*
    (declare-function (string name) (length args)) ; deliberately DON'T mangle
    (mapcar #'compile-expr args)
    "%raw-call"))


;;; The following should definitely be macros in the standard library, but
;;; for the bootstrap compiler it's easier to just define them in the compiler
;;; than it is to add a macro system

(defbuiltin |list| (&rest args)
  (compile-expr (reduce (lambda (a b) `(|cons| ,a ,b)) args
                        :from-end t
                        :initial-value '|nil|)))

(defbuiltin |cond| (&rest clauses)
  (compile-expr
    (if (null clauses)
      '|nil|
      `(|if| ,(caar clauses)
         ; TODO: progn
         ,(cadar clauses)
         (|cond| ,@(cdr clauses))))))

(defbuiltin |case| (expr &rest clauses)
  (let ((case-sym (gensym "case")))
    (compile-expr
      `(|let| ((,case-sym ,expr))
         (|cond|
           ,@(mapcar (lambda (clause)
                       ;; TODO: change once cond support multiple exprs
                       (list `(|eq?| ,case-sym ,(first clause)) (second clause)))
                     clauses))))))
