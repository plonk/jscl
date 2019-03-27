(load "jscl.lisp")

(in-package #:jscl)

(defun make (&rest files)
  (let ((*features* (list* :jscl :jscl-xc *features*))
        (*package* (find-package "JSCL")))
    (setq *environment* (make-lexenv))

    (with-compilation-environment
        (with-open-file (out (merge-pathnames "main.js" *base-directory*)
                             :direction :output
                             :if-exists :supersede)
          (format out "(function(){~%")
          (format out "'use strict';~%")
          (write-string (read-whole-file (source-pathname "prelude.js")) out)
          (do-source input :target
                     (!compile-file input out))
          (dump-global-environment out)

          ;; NOTE: Thie file must be compiled after the global
          ;; environment. Because some web worker code may do some
          ;; blocking, like starting a REPL, we need to ensure that
          ;; *environment* and other critical special variables are
          ;; initialized before we do this.
          (!compile-file "src/toplevel.lisp" out)

          (rename-package 'common-lisp-user 'cl-user)

          (dolist (f files)
            (let ((*package* (find-package "CL-USER")))
              (!compile-file f out)))

          (format out "})();~%")))
    ))
