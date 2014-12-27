(in-package boot)

(declaim (optimize debug))

(defun nuc-compile-file (input-filename output-filename &optional dump-module-p)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module output-filename))
    (let ((*env* nil)
          (*current-file* input-filename)
          (*lambda-counter* 0))
      (compile-prelude)
      (dolist (form (mappend #'process-toplevel-form
                             (read-file input-filename)))
        (compile-toplevel-form form))
      (when dump-module-p
        (llvm:dump-module *module*))
      (llvm:verify-module *module*)
      (llvm:write-bitcode-to-file *module* output-filename))))

(defun compile-prelude ()
  (map nil #'compile-toplevel-form
       (mappend #'process-toplevel-form
                '((|defvar| |$status-code|)
                  ;; TODO: this should locate the standard library rather than
                  ;; assuming we're sitting next to it
                  (|include| "../stdlib/lib")))))

(defun process-toplevel-form (form)
  (ecase (car form)
    (|defun|
      (declare-function (string (cadr form)) (length (caddr form)))
      (list form))
    (|extern|
      (when (/= (length (cdr form)) 2)
        (error "Invalid number of arguments to 'extern' (got ~D, expected 2)"
               (length (cdr form))))
      (declare-function (string (cadr form)) (caddr form))
      nil)
    (|defvar|
      (when (/= (length form) 2)
        (error "defvar doesn't yet support initializers"))
      (unless (symbolp (cadr form))
        (error "~S is not a valid variable name" (cadr form)))
      (llvm:set-initializer
        (llvm:add-global *module* *nuc-val* (string (cadr form)))
        ; All globals are zeroed
        (llvm-val<-int 0))
      nil)
    (|include|
      (when (/= (length (cdr form)) 1)
        (error "Invalid number of arguments to 'include' (got ~D, expected 1)"
               (length (cdr form))))
      (let* ((includee (lookup-included-filename (cadr form)))
             (*current-file* includee))
        (mappend #'process-toplevel-form (read-file includee))))))

(defun mappend (func list)
  (apply #'append (mapcar func list)))

(defun declare-function (name arity)
  (apply #'extern-func name *nuc-val* (loop repeat arity collecting *nuc-val*)))

(defun extern-func (name return-type &rest arg-types)
  (let ((func (llvm:named-function *module* name)))
    (if (not (cffi:null-pointer-p func))
      func
      (llvm:add-function
        *module* name
        (llvm:function-type return-type (map 'vector #'identity arg-types))))))

(defun compile-toplevel-form (form)
  (ecase (car form)
    (|defun|
      (compile-defun (cadr form) (caddr form) (cdddr form)))))

(defun compile-defun (name args body)
  (let* ((func (llvm:named-function *module* (string name))))
    (map nil
         (lambda (param name)
           (setf (llvm:value-name param) (string name)))
         (llvm:params func)
         args)
    (llvm:position-builder-at-end *builder* (llvm:append-basic-block func "entry"))
    (let ((*env* (append
                   (mapcar (lambda (arg-name llvm-param)
                             (let ((arg-on-stack
                                     (llvm:build-alloca *builder* *nuc-val*
                                                        (string arg-name))))
                               (llvm:build-store *builder*
                                                 llvm-param arg-on-stack)
                               (cons arg-name arg-on-stack)))
                           args
                           (llvm:params func))
                   *env*))
          (*current-func* func))
      (when (null body)
        (setf body (list '|nil|)))
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

(defun lookup-included-filename (includee)
  ;; Later we could have some predefined include paths or something.
  ;; For now it's just relative to the including file
  (let* ((path (namestring
                 (merge-pathnames
                   includee (make-pathname
                              :directory (pathname-directory *current-file*)))))
         (extension (search ".nuc" path)))
    (if extension
      path
      (concatenate 'string path ".nuc"))))

(defun compile-expr (expr)
  (etypecase expr
    ;; TODO: limit on size
    (integer (llvm-val<-int (ash expr *lowtag-bits*)))
    (symbol (let ((const (cdr (assoc expr *constants*))))
              (if const
                const
                (multiple-value-bind (binding bindingp) (lookup-lvalue expr)
                  (if bindingp
                    (llvm:build-load *builder* binding (string expr))
                    (error "Undefined variable '~S'~%" expr))))))
    (list (compile-form expr))
    (string (llvm:build-call
              *builder*
              (extern-func "rt_make_string" *nuc-val* *size-t*
                           (llvm:pointer-type (llvm:int-type 8)))
              (list (llvm-val<-int (length expr))
                    (llvm:build-gep
                      *builder*
                      (llvm:build-global-string *builder* expr "string-lit")
                      (make-array (list 2) :initial-element (llvm-val<-int 0))
                      "str-to-ptr"))
              "make-literal-string"))))

(defun lookup-lvalue (name)
  (let ((lexical-binding (cdr (assoc name *env*)))
        (global-binding (llvm:named-global *module* (string name))))
    (cond
      (lexical-binding (values lexical-binding t))
      ((not (cffi:null-pointer-p global-binding)) (values global-binding t))
      (t (values nil nil)))))

(defun compile-form (form)
  (let* ((name (car form))
         (args (cdr form))
         (builtin (gethash name *builtins*))
         (func (llvm:named-function *module* (string name))))
    (multiple-value-bind (binding bindingp) (lookup-lvalue name)
      (cond
        (builtin (funcall builtin args))
        ((not (cffi:null-pointer-p func))
         (llvm:build-call *builder* func
                          (mapcar #'compile-expr args)
                          (string name)))
        (bindingp
          (llvm:build-call
            *builder*
            (llvm:build-int-to-pointer
              *builder*
              (llvm:build-and *builder*
                              (llvm:build-load *builder* binding (string name))
                              (llvm:build-not
                                *builder*
                                (llvm-val<-int
                                  (loop for n from 0 below *lowtag-bits*
                                        summing (ash 1 n)))
                                "lowtag-remover")
                              "remove-lowtag")
              (llvm:pointer-type 
                (llvm:function-type
                  *nuc-val*
                  (loop repeat (length args) collecting *nuc-val*)))
              "nuc-val-to-func-pointer")
            (mapcar #'compile-expr args)
            "call-func-pointer"))
        (t (error "Don't know how to compile form ~S" form))))))

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
