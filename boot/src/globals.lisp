(in-package :boot)

(defvar *builder*)
(defvar *module*)
(defvar *env*)

(defparameter *lisp-value* (llvm:int-type 64))
(defparameter *lowtag-bits* 3)
(defparameter *llvm-lowtag-bits-const* 3)
