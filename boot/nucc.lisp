(let ((*standard-output* (make-broadcast-stream))
      (quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; For some reason if we merge these LETs, we get a reader error about the
;;; package ASDF not being found...
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system 'boot))

(destructuring-bind (_ in out) sb-ext:*posix-argv*
  (boot::nuc-compile-file in out))
