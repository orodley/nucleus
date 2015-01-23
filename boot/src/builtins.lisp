(in-package boot)

(declaim (optimize debug))

(defvar *builtins* (make-hash-table))

(defmacro defbuiltin (name lambda-list &body body)
  (let ((args-sym (gensym)))
    `(setf (gethash ',name *builtins*)
           (lambda (,args-sym)
             (destructuring-bind ,lambda-list ,args-sym
               ,@body)))))

(defun nuc-val<-int (nuc-val)
  (llvm::build-shl
    *builder*
    nuc-val
    (llvm-val<-int *lowtag-bits*)
    "nuc-val<-int"))

(defun int<-nuc-val (int)
  ; TODO: type checking
  (llvm::build-l-shr
    *builder*
    int
    (llvm-val<-int *lowtag-bits*)
    "int<-nuc-val"))

(defun nuc-val<-cons (cons)
  (add-lowtag cons #b010))

(defun add-lowtag (nuc-val tag)
  (llvm:build-or
    *builder*
    (llvm:build-pointer-to-int *builder* nuc-val *nuc-val* "ptr-to-int")
    (llvm-val<-int tag)
    "add-lowtag"))

(defun cons<-nuc-val (nuc-val)
  ; TODO: type checking
  (llvm:build-int-to-pointer
    *builder*
    (remove-lowtag nuc-val)
    *cons-cell-ptr*
    "cons<-nuc-val"))

(defun remove-lowtag (nuc-val)
  (llvm:build-and
    *builder*
    nuc-val
    (llvm-val<-int (lognot (1- (ash 1 *lowtag-bits*))))
    "remove-lowtag"))

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
(define-binary-op / llvm:build-s-div)
(define-binary-op % llvm::build-s-rem)
(define-binary-op >> llvm::build-l-shr)

(defmacro define-comparison (name)
  (let ((name-str (string name)))
    `(defbuiltin ,(intern (format nil "binary-~A" name)) (lhs rhs)
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

(macrolet ((def (cmp)
             `(defbuiltin ,cmp (&rest args)
                (compile-expr
                  `(|and|
                     ,@(loop for cons on args
                             until (null (cdr cons))
                             collecting
                               (list ',(intern (format nil "binary-~A" cmp))
                                     (first cons) (second cons))))))))
  (def <)
  (def <=)
  (def >)
  (def >=))

(defbuiltin |if| (condition then-form &optional (else-form '|nil|))
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
         (captures (find-captured-vars body *env*))
         (func (compile-lambda name simple-lambda-list body (mapcar #'car captures))))
    ;; COMPILE-DEFUN will change the builders position.
    (llvm:position-builder-at-end *builder* current-block)
    (let ((captures-array (llvm:build-alloca
                            *builder*
                            (llvm:array-type
                              (llvm:pointer-type *nuc-val*)
                              (length captures))
                            "make-captures-array")))
      (loop for capture in captures
            for i = 0 then (1+ i)
            ;; TODO: insert-value
            do (llvm:build-store
                 *builder*
                 (cdr capture)
                 (llvm:build-gep *builder* captures-array
                                 (map 'vector #'llvm-val<-int (list 0 i))
                                 "array-elt")))
      (llvm:build-call *builder*
                       (extern-func "rt_make_lambda" *nuc-val*
                                    *uintptr* (llvm:int-type 8)
                                    (llvm:int-type 32) *uintptr*)
                       (list (llvm:build-pointer-to-int
                               *builder* func *uintptr* "func-pointer-to-int")
                             (llvm-val<-int (length simple-lambda-list) 8)
                             (llvm-val<-int (length captures) 32)
                             (llvm:build-pointer-to-int
                               *builder* captures-array *uintptr* "array-to-int"))
                       "make-lambda"))))

(defun find-captured-vars (body vars-alist)
  (labels
    ((%find-captured-vars (body vars-alist)
       (cond
         ((atom body)
          (let ((binding (assoc body vars-alist)))
            (if binding
              (list binding)
              nil)))
         ((eq (car body) 'let)
          (%find-captured-vars
            (third body)
            ;; let can shadow variables, producing spurious captures
            (remove-if (lambda (var-cell)
                         (member (car var-cell) (second body) :key #'car))
                       vars-alist)))
         (t (mappend (lambda (body) (%find-captured-vars body vars-alist))
                     body)))))
    (remove-duplicates (%find-captured-vars body vars-alist))))

(defbuiltin |progn| (&body body)
  (loop for cons on body
        for compiled = (compile-expr (car cons))
        when (null (cdr cons))
          return compiled))

(defbuiltin |quote| (thing)
  (etypecase thing
    (integer (compile-expr thing))
    (null (compile-expr '|nil|))
    (cons (compile-expr `(|cons| (|quote| ,(car thing))
                                 (|quote| ,(cdr thing)))))
    (symbol (llvm:build-call
              *builder*
              (extern-func "rt_intern_symbol" *nuc-val*
                           (llvm:pointer-type (llvm:int-type 8)))
              (list (llvm:build-gep
                      *builder*
                      (llvm:build-global-string *builder* (string thing)
                                                "intern-const")
                      (make-array (list 2) :initial-element (llvm-val<-int 0))
                      "str-to-ptr"))
              "intern-const"))))

(defbuiltin |%raw-call| (name &rest args)
  (llvm:build-call
    *builder*
    ;; deliberately DON'T mangle 
    (declare-function (string name) (length args))
    (mapcar #'compile-expr args)
    "%raw-call"))

(defbuiltin |%extern-call| (name args-alist return-type)
  (nuc-val<-c-val
    return-type
    (llvm:build-call
      *builder*
      (apply #'extern-func
             name
             (llvm-type<-type-spec return-type)
             (mapcar (lambda (entry)
                       (llvm-type<-type-spec (car entry)))
                     args-alist))
      (mapcar (lambda (entry) (c-val<-nuc-val (car entry)
                                              (compile-expr (cdr entry))))
              args-alist)
      (if (eq return-type '|void|)
        ""
        "%extern-call"))))

(defun llvm-type<-type-spec (type)
  (ecase type
    (* *foreign-pointer*)
    (|string| (llvm:pointer-type (llvm:int-type 8)))
    (|void| (llvm:void-type))
    ((|int| |uint| |bool|) (llvm:int-type 32))
    (|array| (llvm:pointer-type *foreign-pointer*))))

(defun c-val<-nuc-val (type val)
  (ecase type
    (* (llvm:build-int-to-pointer *builder* (remove-lowtag val)
                                  *foreign-pointer* "foreign-pointer<-nuc-val"))
    (|string| ; char *
      (llvm:build-call
        *builder*
        (extern-func "rt_nuc_str_to_c_str"
                     (llvm:pointer-type (llvm:int-type 8))
                     *nuc-val*)
        (list val)
        "c-val<-nuc-val"))
    ((|int| |uint|) ; platform dependent int type
      (llvm::build-int-cast
        *builder*
        (llvm::build-l-shr
          *builder*
          val
          (llvm-val<-int *lowtag-bits*)
          "shift-off-lowtag")
        (llvm-type<-type-spec type)
        "downcast-nuc-val-to-int"))
    (|bool|
      (llvm::build-int-cast
        *builder*
        (llvm:build-i-cmp *builder* := val *true* "int<-bool")
        (llvm-type<-type-spec '|int|)
        "cast-cmp-result-to-int"))
    (|array|
      (llvm:build-call
        *builder*
        (extern-func "rt_list_to_array"
                     (llvm:pointer-type *foreign-pointer*) *nuc-val*)
        (list val)
        "array<-list"))))

(defun nuc-val<-c-val (type val)
  (ecase type
    (* ; opaque pointer
      (add-lowtag (llvm:build-pointer-to-int *builder* val *nuc-val*
                    "nuc-val<-foreign-pointer")
                  #b110))
    (|void|
      (compile-expr '|nil|))
    ((|int| |uint|)
      (add-lowtag
        (llvm::build-int-cast *builder* val *nuc-val* "upcast-int-to-nuc-val")
        #b000))))

;;; The following should definitely be in the standard library, but for the
;;; bootstrap compiler it's easier to just define them in the compiler than it
;;; is to add a macro system

(defmacro define-nuc-macro (name ll &body body)
  `(defbuiltin ,name ,ll
     (compile-expr (progn ,@body))))

(define-nuc-macro |list| (&rest args)
  (reduce (lambda (a b) `(|cons| ,a ,b)) args
          :from-end t
          :initial-value '|nil|))

(define-nuc-macro |cond| (&rest clauses)
  (if (null clauses)
    '|nil|
    `(|if| ,(caar clauses)
       (|progn| ,@(cdar clauses))
       (|cond| ,@(cdr clauses)))))

;;; TODO: Support 'otherwise' as a default case.
(define-nuc-macro |case| (expr &rest clauses)
  (let ((case-sym (gensym "case")))
    `(|let| ((,case-sym ,expr))
       (|cond|
         ,@(mapcar (lambda (clause)
                     `((|eq?| ,case-sym ,(first clause)) ,@(rest clause)))
                   clauses)))))

(define-nuc-macro |let*| (clauses &body body)
  (if (null (cdr clauses))
    `(|let| ,clauses ,@body)
    `(|let| ,(list (first clauses)) (|let*| ,(rest clauses) ,@body))))

(define-nuc-macro |and| (&rest operands)
  (if (null operands)
    '|true|
    (let ((operand-sym (gensym)))
      `(|let| ((,operand-sym ,(first operands)))
         (|if| ,operand-sym
           (|and| ,@(rest operands))
           |false|)))))

(define-nuc-macro |or| (&rest operands)
  (if (null operands)
    '|false|
    (let ((operand-sym (gensym)))
      `(|let| ((,operand-sym ,(first operands)))
         (|if| ,operand-sym
           |true|
           (|or| ,@(rest operands)))))))
