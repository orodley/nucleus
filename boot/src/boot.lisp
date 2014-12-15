(in-package :boot)

(defun test ()
  (llvm:with-objects ((builder llvm:builder)
                      (mod llvm:module "test"))
    (llvm:dump-module mod)))
