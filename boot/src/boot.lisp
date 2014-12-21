(in-package boot)

(declaim (optimize debug))

(defun nuc-compile-file (input-filename output-filename &optional dump-module-p)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module output-filename))
    (let ((*env* nil))
      (compile-prelude)
      (dolist (form (read-file input-filename))
        (compile-toplevel-form form))
      (when dump-module-p
        (llvm:dump-module *module*))
      (llvm:write-bitcode-to-file *module* output-filename))))

(defun compile-prelude ()
  (dolist (form '((|defvar| |$status-code|)))
    (compile-toplevel-form form)))

(defun compile-toplevel-form (form)
  (ecase (car form)
    (|defun|
      (compile-defun (cadr form) (caddr form) (cdddr form)))
    (|defvar|
      (when (> (length form) 2)
        (error "defvar doesn't yet support initializers"))
      (compile-defvar (cadr form)))))

(defun compile-defun (name args body)
  (let* ((func-type (llvm:function-type
                     *lisp-value*
                     (make-array (length args) :initial-element *lisp-value*)))
         (func (llvm:add-function *module* (string name) func-type)))
    (map nil
         (lambda (param name)
           (setf (llvm:value-name param) (string name)))
         (llvm:params func)
         args)
    (llvm:position-builder-at-end *builder* (llvm:append-basic-block func "entry"))
    (let ((*env* (append
                   (mapcar (lambda (arg-name llvm-param)
                             (let ((arg-on-stack
                                     (llvm:build-alloca *builder* *lisp-value*
                                                        (string arg-name))))
                               (llvm:build-store *builder*
                                                 llvm-param arg-on-stack)
                               (cons arg-name arg-on-stack)))
                           args
                           (llvm:params func))
                   *env*)))
      (loop for cons on body
            for compiled-expr = (compile-expr (car cons))
            when (null (cdr cons))
              do (if (eq name '|main|)
                   (llvm:build-ret
                     *builder*
                     (llvm::build-l-shr *builder*
                                        (compile-expr '|$status-code|)
                                        (llvm-val<-int *lowtag-bits*)
                                        "$status-code"))
                   (llvm:build-ret *builder* compiled-expr))))
    (unless (llvm:verify-function func)
      (llvm:dump-value func)
      (error "ICE compiling function ~S (failed llvm:verify-function)~%~
              Function has been dumped" name))))

(defun compile-defvar (name)
  (unless (symbolp name)
    (error "~S is not a valid variable name" name))
  (llvm:set-initializer
    (llvm:add-global *module* *lisp-value* (string name))
    ; All globals are zeroed
    (llvm-val<-int 0)))

(defun compile-expr (expr)
  (etypecase expr
    ;; TODO: limit on size
    (integer (llvm-val<-int (ash expr *lowtag-bits*)))
    (symbol (llvm:build-load *builder* (lookup-var expr) (string expr)))
    (list (compile-form expr))))

(defun lookup-var (name)
  (let ((lexical-binding (cdr (assoc name *env*)))
        (global-binding (llvm:named-global *module* (string name))))
    (cond
      (lexical-binding lexical-binding)
      ((not (cffi:null-pointer-p global-binding)) global-binding)
      (t (error "Undefined variable ~S" name)))))

(defun llvm-val<-int (int)
  (llvm:const-int *lisp-value* (format nil "~D" int) 10))

(defun compile-form (form)
  (let* ((name (car form))
         (args (cdr form))
         (builtin (gethash name *builtins*))
         (func (llvm:named-function *module* (string name))))
    (cond
      (builtin (funcall builtin args))
      ((not (cffi:null-pointer-p func))
       (llvm:build-call *builder* func
                        (mapcar #'compile-expr args)
                        (string name)))
      (t (error "Don't know how to compile form ~S" form)))))

(defun read-file (filename)
  (let ((eof-value (gensym))
        (*readtable* (copy-readtable *readtable*))
        ;; Make sure all symbols get read in the BOOT package, as all of the
        ;; literal symbols in this file are in it.
        (*package* (find-package 'boot)))
    (setf (readtable-case *readtable*) :preserve)
    (with-open-file (file filename)
      (loop for form = (read file nil eof-value)
            until (eq form eof-value)
              collect form))))
