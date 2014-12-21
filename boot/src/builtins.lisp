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

(defbuiltin |set| (name value)
  (let ((var (lookup-var name)))
    (llvm:build-store *builder* (compile-expr value) var)))

(defbuiltin |cons| (car cdr)
  (let* ((cons (llvm::build-malloc *builder* *cons-cell* "cons"))
         (car-ptr (llvm:build-struct-gep *builder* cons 0 "car-ptr"))
         (cdr-ptr (llvm:build-struct-gep *builder* cons 1 "cdr-ptr")))
    (llvm:build-store *builder* (compile-expr car) car-ptr)
    (llvm:build-store *builder* (compile-expr cdr) cdr-ptr)
    cons))

(defbuiltin |car| (cons)
  (let ((cons-ptr (compile-expr cons)))
    (llvm:build-load *builder* 
                     (llvm:build-struct-gep *builder* cons-ptr 0 "car-ptr")
                     "car")))

(defbuiltin |cdr| (cons)
  (let ((cons-ptr (compile-expr cons)))
    (llvm:build-load *builder* 
                     (llvm:build-struct-gep *builder* cons-ptr 1 "cdr-ptr")
                     "cdr")))

(defbuiltin |let| (clauses &body body)
  (print clauses)
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
