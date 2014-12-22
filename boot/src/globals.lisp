(in-package boot)

(defvar *builder*)
(defvar *module*)
(defvar *env*)
(defvar *current-func*)

(defparameter *nuc-val* (llvm:int-type 64))
(defparameter *cons-cell* (llvm:struct-type
                            (list *nuc-val* *nuc-val*) nil))
(defparameter *cons-cell-ptr* (llvm:pointer-type *cons-cell*))
(defparameter *lowtag-bits* 3)
(defparameter *llvm-lowtag-bits-const* 3)

(defun llvm-val<-int (int)
  (llvm:const-int *nuc-val* (format nil "~D" int) 10))

(flet ((make-const (x)
         (llvm-val<-int (logior (ash x *lowtag-bits*) 1))))
  (defparameter *nil* (make-const 0))
  (defparameter *true* (make-const 1))
  (defparameter *false* (make-const 2)))

(defparameter *constants*
  `((|nil| . ,*nil*)
    (|true| . ,*true*)
    (|false| . ,*false*)))
