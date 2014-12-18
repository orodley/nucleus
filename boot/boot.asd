;; Get ASDF to load the version in this directory, regardless of whether
;; there's some other random one elsewhere in the filesystem
(let ((asdf:*central-registry* (directory "CL-LLVM")))
  (asdf:load-system 'llvm))

(asdf:defsystem :boot
  :description "Bootstrap compiler for nucleus, implemented in Common Lisp."
  :components ((:file "package")
               (:module "src"
                :components ((:file "boot" :depends-on ("builtins"))
                             (:file "builtins"))
                :depends-on ("package")))
  :depends-on ("llvm"))
