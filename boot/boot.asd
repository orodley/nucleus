(asdf:defsystem :boot
  :description "Bootstrap compiler for nucleus, implemented in Common Lisp."
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")))
  :depends-on ("llvm"))
