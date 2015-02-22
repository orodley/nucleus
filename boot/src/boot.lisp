(in-package boot)

(declaim (optimize debug))

(defun nuc-compile-file (input-filename output-filename &optional dump-module-p)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module output-filename))
    (let ((*env* nil)
          (*current-file*
            (pathname (pathname
                        (if (char= (elt input-filename 0) #\/)
                          input-filename
                          (concatenate 'string
                                       (namestring *default-pathname-defaults*)
                                       input-filename)))))
          (*lambda-counter* 0)
          (*initialisers* ()))
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
                `((|defvar| |$status-code|)
                  (|include| ,(find-stdlib))))))

(defun find-stdlib ()
  ;; Eventually we should have some smarter way of looking for this - for
  ;; now we just keep looking in the parent dir until we find it
  (loop with file-dir = (canonical (pathname-directory *current-file*))
        ;; I think the fact that PATHNAME-DIRECTORY returns a list of strings
        ;; is implementation-dependent, but we don't care as this is just the
        ;; bootstrap compiler and it doesn't have to be portable.
        for dir = file-dir then (butlast dir)
        for stdlib = (make-pathname :directory (append dir '("stdlib"))
                                    :name "lib"
                                    :type "nuc")
        when (probe-file stdlib) return stdlib
        when (null (cdr dir)) return nil))

;;; TODO: I feel like there should be a function for this, but I can't find it
(defun canonical (dir)
  (loop for cons on dir
        if (member (second cons) '(:up :back))
          do (setf cons (cdr cons))
        else
          collect (car cons)))


(defun process-toplevel-form (form)
  (ecase (car form)
    (|defun|
      (if (eq (second form) '|main|)
        (extern-func "main" (llvm:int-type 32) (llvm:int-type 32)
                     (llvm:pointer-type (llvm:pointer-type (llvm:int-type 8))))
        (declare-function (mangle-name (second form)) (length (third form))))
      (list form))
    (|extern|
      (when (/= (length (cdr form)) 4)
        (error "Invalid number of arguments to 'extern' (got ~D, expected 4)"
               (length (cdr form))))
      (process-toplevel-form
        (let ((args (loop repeat (third form) collecting (gensym))))
          `(|defun| ,(fifth form) ,args
             (|%raw-call| ,(second form) ,@args)))))
    (|defvar|
      (unless (< 0 (length (cdr form)) 3)
        (error "Invalid number of arguments to 'defvar' (got ~D, expected 1-2)"
               (length (cdr form))))
      (unless (symbolp (second form))
        (error "~S is not a valid variable name" (second form)))
      (llvm:set-initializer
        (llvm:add-global *module* *nuc-val* (mangle-name (second form)))
        ; All globals are zeroed
        (llvm-val<-int 0))
      (when (= (length (cdr form)) 2)
        ;; TODO: we could detect if the initialiser is a constant expression,
        ;; and if so initialise it statically instead.
        (push (cons (second form) (third form)) *initialisers*))
      nil)
    (|include|
      (when (/= (length (cdr form)) 1)
        (error "Invalid number of arguments to 'include' (got ~D, expected 1)"
               (length (cdr form))))
      (let* ((includee (lookup-included-filename (second form)))
             (*current-file* includee))
        (when (null includee)
          (error "Could not find included file '~S'" (second form)))
        (mappend #'process-toplevel-form (read-file includee))))
    (|cfun|
      (when (/= (length (cdr form)) 4)
        (error "Invalid number of arguments to 'cfun' (got ~D, expected 4)"
               (length (cdr form))))
      (let* ((name (second form))
            (arg-types (third form))
            (ret-type (fifth form))
            (args (loop for n below (length arg-types)
                            collecting (intern (format nil "~D" n)))))
        (process-toplevel-form
          `(|defun| ,name ,args
             (|%extern-call|
               ,(string name)
               ,(mapcar #'cons arg-types args)
               ,ret-type)))))
    ;; TODO: should this be called 'c'enum? It's just a regular enum really.
    (|cenum|
      (when (null (cdr form))
        (error "An enum must have one or more values"))
      (let ((enum-variant-value -1))
        (dolist (enum-variant-name (cdr form))
          (llvm:set-initializer
            (llvm:add-global *module* *nuc-val* (mangle-name enum-variant-name))
            (nuc-val<-int (llvm-val<-int (incf enum-variant-value)))))))))

(defun mappend (func list)
  (apply #'append (mapcar func list)))

(defun declare-function (name arity)
  (apply #'extern-func name *nuc-val* (loop repeat arity collecting *nuc-val*)))

(defun mangle-name (name)
  (if (eq name '|main|)
    "main"
    ;; Dead simple name-mangling scheme, just to avoid name collisions with
    ;; stuff from the C standard library or anything else we may link against.
    (format nil "nuc(~A)" name)))

;;; TODO: Should probably rename this - it's not necessarily extern it can just
;;; have types in it that aren't *nuc-val*'s
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
      (compile-defun (second form) (third form) (cdddr form)))))

(defun compile-defun (name args body)
  (let* ((func (llvm:named-function *module* (mangle-name name))))
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
        ;; implicit nil return for empty functions
        (push '|nil| body))
      (when (eq name '|main|)
        ;; Code to capture argv
        (llvm:build-call
          *builder*
          (extern-func "rt_store_argv" (llvm:void-type) (llvm:int-type 32)
                       (llvm:pointer-type (llvm:pointer-type (llvm:int-type 8))))
          (map 'list #'identity (llvm:params func))
          "")
        ;; *INITIALISERS* is in reverse order, so when we process it we push
        ;; each initialiser, reversing it back to correct order
        (dolist (init *initialisers*)
          (push (list '|set| (car init) (cdr init)) body)))
      (loop for cons on body
            for compiled-expr = (compile-expr (car cons))
            when (null (cdr cons))
              do (if (eq name '|main|)
                   (llvm:build-ret
                     *builder*
                     (c-val<-nuc-val '|int| (compile-expr '|$status-code|)))
                   (llvm:build-ret *builder* compiled-expr))))
    (unless (llvm:verify-function func)
      (llvm:dump-value func)
      (error "ICE compiling function ~S (failed llvm:verify-function)~%~
              Function has been dumped" name))))

(defun compile-lambda (name args body captured-vars)
  (let* ((params (mapcar (constantly *nuc-val*) args))
         ;; We only take the lambda struct as our first argument if we have
         ;; any captured bindings. This allows us to easily deal with passing
         ;; around static functions in lambdas.
         ;; TODO: We could optimize further by not allocating a new closure if
         ;; we don't capture any bindings, and instead just creating a static
         ;; closure once at compile-time and returning it every time.
         (func (apply #'extern-func (mangle-name name) *nuc-val*
                      (if (null captured-vars)
                        params
                        (cons (llvm:pointer-type *closure*) params))))
         (*current-func* func))
    (when (null body)
      (push '|nil| body))
    (map nil
         (lambda (param name)
           (setf (llvm:value-name param) (string name)))
         (if (null captured-vars)
           (llvm:params func)
           (subseq (llvm:params func) 1))
         args)
    (llvm:position-builder-at-end *builder* (llvm:append-basic-block func "entry"))
    (let* ((*env*
             (append
               (loop for name in captured-vars
                     for i = 0 then (1+ i)
                     collecting
                     (cons
                       name
                       (llvm:build-load
                         *builder*
                         (llvm:build-gep
                           *builder*
                           (llvm:build-gep *builder* (elt (llvm:params func) 0)
                                           (map 'vector (lambda (n) (llvm-val<-int n 32))
                                                (list 0 2))
                                           "get-captured-vars-from-closure")
                           (map 'vector #'llvm-val<-int (list 0 i))
                           "get-var-from-closure")
                         "")))
               (mapcar (lambda (arg-name llvm-param)
                         (let ((arg-on-stack
                                 (llvm:build-alloca *builder* *nuc-val*
                                                    (string arg-name))))
                           (llvm:build-store *builder*
                                             llvm-param arg-on-stack)
                           (cons arg-name arg-on-stack)))
                       args
                       (subseq (llvm:params func) (if (null captured-vars) 0 1)))
               *env*)))
      (loop for cons on body
            for compiled-expr = (compile-expr (car cons))
            when (null (cdr cons))
              do (llvm:build-ret *builder* compiled-expr)))
    (unless (llvm:verify-function func)
      (llvm:dump-value func)
      (error "ICE compiling lambda ~S (failed llvm:verify-function)~%~
              Function has been dumped" name))
    func))

(defun lookup-included-filename (includee)
  ;; Later we could have some predefined include paths or something.
  ;; For now it's just relative to the including file
  (unless (null includee)
    (let* ((path (namestring
                   (merge-pathnames
                     includee (make-pathname
                                :directory (pathname-directory *current-file*)))))
           (extension (search ".nuc" path)))
      (if extension
        path
        (concatenate 'string path ".nuc")))))

(defun compile-expr (expr)
  (etypecase expr
    ;; TODO: limit on size
    (integer (llvm-val<-int (ash expr *lowtag-bits*)))
    (character (llvm-val<-int (ash (char-code expr) *lowtag-bits*)))
    (symbol (let ((const (cdr (assoc expr *constants*))))
              (if const
                const
                (let ((func (llvm:named-function *module* (mangle-name expr))))
                  (if (not (cffi:null-pointer-p func))
                    (make-lambda func (length (llvm:params func)) nil)
                    (multiple-value-bind (binding bindingp) (lookup-lvalue expr)
                      (if bindingp
                        (llvm:build-load *builder* binding (string expr))
                        (error "Undefined variable '~S'~%" expr))))))))
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

(defun make-lambda (func arity captures)
  (let* ((type (llvm:array-type (llvm:pointer-type *nuc-val*) (length captures)))
         (captures-array
           (if (null captures)
             (llvm:const-null type)
             (llvm:build-alloca *builder* type "make-captures-array"))))
    (loop for capture in captures
          for i = 0 then (1+ i)
          ;; TODO: insert-value
          do (llvm:build-store
               *builder*
               (cdr capture)
               (llvm:build-gep *builder* captures-array
                               (map 'vector #'llvm-val<-int (list 0 i))
                               "array-elt")))
    (llvm:build-call
      *builder*
      (extern-func "rt_make_lambda" *nuc-val*
                   *uintptr* (llvm:int-type 8)
                   (llvm:int-type 32) *uintptr*)
      (list (llvm:build-pointer-to-int
              *builder* func *uintptr* "func-pointer-to-int")
            (llvm-val<-int arity 8)
            (llvm-val<-int (length captures) 32)
            (if (null captures)
              (llvm-val<-int 0)
              (llvm:build-pointer-to-int
                *builder* captures-array *uintptr* "array-to-int")))
      "make-lambda")))

(defun lookup-lvalue (name)
  (let ((lexical-binding (cdr (assoc name *env*)))
        (global-binding (llvm:named-global *module* (mangle-name name))))
    (cond
      (lexical-binding (values lexical-binding t))
      ((not (cffi:null-pointer-p global-binding)) (values global-binding t))
      (t (values nil nil)))))

(defun compile-form (form)
  (let* ((name (car form))
         (args (cdr form))
         (builtin (gethash name *builtins*))
         (func (llvm:named-function *module* (mangle-name name))))
    (multiple-value-bind (binding bindingp) (lookup-lvalue name)
      (cond
        (builtin (funcall builtin args))
        ((not (cffi:null-pointer-p func))
         (unless (= (length args) (length (llvm:params func)))
           (error
             "Invalid number of arguments to function '~A' (got ~D, expected ~D)"
             name (length args) (length (llvm:params func))))
         (llvm:build-call *builder* func
                          (mapcar #'compile-expr args)
                          (mangle-name name)))
        (bindingp
          ;; TODO: Runtime arity check
          (let* ((closure (llvm:build-int-to-pointer
                            *builder*
                            (remove-lowtag
                              (llvm:build-load *builder* binding "get-closure"))
                            (llvm:pointer-type *closure*)
                            "cast-nuc-val-to-closure"))
                 (func-pointer
                   (llvm:build-load
                     *builder*
                     ;; TODO: build-struct-gep?
                     (llvm:build-gep
                       *builder*
                       closure
                       (map 'vector (lambda (n) (llvm-val<-int n 32)) (list 0 0))
                       "get-func-pointer-from-closure")
                     "load-func-pointer-from-struct"))
                 (args (mapcar #'compile-expr args))
                 (captures
                   (llvm:build-gep
                     *builder*
                     (llvm:build-gep *builder* closure
                                     (map 'vector (lambda (n) (llvm-val<-int n 32))
                                          (list 0 2))
                                     "")
                     (map 'vector (lambda (n) (llvm-val<-int n 32))
                          (list 0 0))
                     ""))
                 (first-capture (llvm:build-load *builder* captures "")))
            (compile-if
              (compile-expr `(|eq?| (|quote| ,(llvm:build-pointer-to-int *builder* first-capture *uintptr* ""))
                                    (|quote| ,(llvm-val<-int 0))))
              (llvm:build-call
                          *builder*
                          (llvm:build-int-to-pointer
                            *builder*
                            func-pointer
                            (llvm:pointer-type
                              (llvm:function-type
                                *nuc-val*
                                (loop repeat (length args) collecting *nuc-val*)))
                            "cast-to-correct-function-type")
                          args
                          "call-closure")
              (llvm:build-call
                *builder*
                (llvm:build-int-to-pointer
                  *builder*
                  func-pointer
                  (llvm:pointer-type
                    (llvm:function-type
                      *nuc-val*
                      (cons (llvm:pointer-type *closure*)
                            (loop repeat (length args) collecting *nuc-val*))))
                  "cast-to-correct-function-type")
                (cons closure args)
                "call-closure"))))
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
