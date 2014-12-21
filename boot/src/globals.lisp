(in-package boot)

(defvar *builder*)
(defvar *module*)
(defvar *env*)
(defvar *current-func*)

(defparameter *lisp-value* (llvm:int-type 64))
(defparameter *cons-cell* (llvm:struct-type
                            (list *lisp-value* *lisp-value*) nil))
(defparameter *lowtag-bits* 3)
(defparameter *llvm-lowtag-bits-const* 3)

(defun llvm-val<-int (int)
  (llvm:const-int *lisp-value* (format nil "~D" int) 10))

(flet ((make-const (x)
         (llvm-val<-int (logior (ash x *lowtag-bits*) 1))))
  (defparameter *nil* (make-const 0))
  (defparameter *true* (make-const 1))
  (defparameter *false* (make-const 2)))

(defparameter *constants*
  `((|nil| . ,*nil*)
    (|true| . ,*true*)
    (|false| . ,*false*)))
