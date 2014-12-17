(in-package :boot)

(declaim (optimize debug))

(defvar *builder*)
(defvar *module*)

(defvar *lisp-value* (llvm:int-type 64))
(defvar *lowtag-bits* 4)

(defun nuc-compile-file (input-filename output-filename)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "test"))
    (dolist (form (read-file input-filename))
      (compile-toplevel-form form)) 
    (llvm:write-bitcode-to-file *module* output-filename)))

(defun compile-toplevel-form (form)
  (ecase (car form)
    (|defun|
      (compile-defun (cadr form) (caddr form) (cdddr form)))))

(defun compile-defun (name args body)
  (let* ((func-type (llvm:function-type
                     *lisp-value*
                     (make-array (length args) :initial-element *lisp-value*)))
         (func (llvm:add-function *module* (string name) func-type)))
    (map nil
         (lambda (param name)
           (setf (llvm:value-name param) name))
         (llvm:params func)
         args)
    (llvm:position-builder-at-end *builder* (llvm:append-basic-block func "entry"))
    (loop for cons on body
          for compiled-expr = (compile-expr (car cons))
          when (null (cdr cons))
            do (llvm:build-ret *builder* compiled-expr))
    (unless (llvm:verify-function func)
      (error "Invalid definition for function ~S" name))
    (llvm:dump-value func)))

(defun compile-expr (form)
  (typecase form
    ;; TODO: limit on size
    (integer (llvm:const-int *lisp-value*
                             (format nil "~D" (ash form *lowtag-bits*))
                             10))))

(defun read-file (filename)
  (let ((eof-value (gensym))
        (*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (with-open-file (file filename)
      (loop for form = (read file nil eof-value)
            until (eq form eof-value)
              collect form))))
