(in-package boot)

(defvar *builtins* (make-hash-table))

(defmacro defbuiltin (name lambda-list &body body)
  (let ((args-sym (gensym)))
    `(setf (gethash ',name *builtins*)
           (lambda (,args-sym)
             (destructuring-bind ,lambda-list ,args-sym
               ,@body)))))

(defun lisp-val<-int (lisp-val)
  ; TODO: type checking
  (llvm::build-shl *builder*
                   lisp-val
                   (llvm-val<-int *lowtag-bits*)
                   "lisp-val<-int"))

(defun int<-lisp-val (int)
  ; TODO: type checking
  (llvm::build-l-shr *builder*
                     int
                     (llvm-val<-int *lowtag-bits*)
                     "int<-lisp-val"))

(defmacro define-binary-op (name instruction)
  `(defbuiltin ,name (&rest operands)
     (let ((operands (mapcar (lambda (expr)
                               (int<-lisp-val (compile-expr expr)))
                             operands)))
       (lisp-val<-int
         (reduce (lambda (lhs rhs)
                   (,instruction *builder* lhs rhs (string ',name)))
                 operands)))))

(define-binary-op + llvm:build-add)
(define-binary-op - llvm:build-sub)
(define-binary-op * llvm:build-mul)
(define-binary-op >> llvm::build-l-shr)

(defbuiltin |if| (condition then-form else-form)
  (let ((then-block (llvm:append-basic-block *current-func* "then"))
        (else-block (llvm:append-basic-block *current-func* "else"))
        (after-block (llvm:append-basic-block *current-func* "after"))
        (if-value (llvm:build-alloca *builder* *lisp-value* "if-value")))
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

(defbuiltin |cons| (car cdr)
  (let* ((cons-ptr (llvm::build-malloc *builder* *cons-cell* "cons"))
         (car-ptr (llvm:build-struct-gep *builder* cons-ptr 0 "car-ptr"))
         (cdr-ptr (llvm:build-struct-gep *builder* cons-ptr 1 "cdr-ptr")))
    (llvm:build-store *builder* (compile-expr car) car-ptr)
    (llvm:build-store *builder* (compile-expr cdr) cdr-ptr)
    (llvm:build-pointer-to-int *builder* cons-ptr *lisp-value* "cast-cons")))

(defbuiltin |car| (cons)
  (let ((cons-ptr (llvm:build-int-to-pointer
                    *builder* (compile-expr cons) *cons-cell-ptr* "cast-cons")))
    (llvm:build-load *builder* 
                     (llvm:build-struct-gep *builder* cons-ptr 0 "car-ptr")
                     "car")))

(defbuiltin |cdr| (cons)
  (let ((cons-ptr (llvm:build-int-to-pointer
                    *builder* (compile-expr cons) *cons-cell-ptr* "cast-cons")))
    (llvm:build-load *builder* 
                     (llvm:build-struct-gep *builder* cons-ptr 1 "cdr-ptr")
                     "cdr")))

(defbuiltin |let| (clauses &body body)
  (let ((*env* (append
                 (mapcar
                   (lambda (clause)
                     (let* ((name (car clause))
                            (expr (cadr clause)) 
                            (var-on-stack (llvm:build-alloca *builder*
                                                             *lisp-value*
                                                             (string name))))
                       (llvm:build-store
                         *builder* (compile-expr expr) var-on-stack)
                       (cons name var-on-stack)))
                   clauses))))
    (loop for cons on body
          for compiled-expr = (compile-expr (car cons))
          when (null (cdr cons))
            return compiled-expr)))
