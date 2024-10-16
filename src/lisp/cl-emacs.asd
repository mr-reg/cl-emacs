#-asdf3.3 (error "CL-EMACS requires ASDF 3.3 or later.")
#-PACKAGE-LOCAL-NICKNAMES (error "CL-EMACS requires ASDF PACKAGE-LOCAL-NICKNAMES capability.")
(load "lib/elisp-packages.lisp")

(asdf:defsystem :cl-emacs
  :version "0.0.1"
  :author "Gleb Borodulia <mr.reg@mail.ru>"
  :license "GPLv3"
  :long-name "cl-emacs"
  :class :package-inferred-system
  :around-compile (lambda (next)
                    (proclaim '(optimize
                                (safety 3)
                                (debug 3)
                                (compilation-speed 3)
                                (speed 0)))
                    ;; #+sbcl
                    ;; (proclaim (sb-ext:muffle-conditions cl:style-warning))
                    #+sbcl
                    (setq *compile-verbose* nil)
                    (funcall next))

  :defsystem-depends-on (;; :cffi
                         :cl-custom-hash-table
                         :cl-plumbing
                         :cl-ppcre
                         :defstar
                         ;; :osicat
                         :fiveam
                         :flexi-streams
                         :float-features
                         ;; :lisp-binary
                         :mstrings
                         :parse-number
                         :serapeum
                         :str
                         :trees
                         :vom
                         :uiop
                         ;; :lisp-critic
                         ;; :usocket
                         )
  :depends-on (
               ;; :cl-emacs/elisp/alien-vars
               ;; :cl-emacs/elisp/alloc
               ;; :cl-emacs/elisp/data
               ;; :cl-emacs/elisp/editfns
               ;; :cl-emacs/elisp/fileio
               ;; :cl-emacs/elisp/fns
               ;; :cl-emacs/elisp/font
               ;; :cl-emacs/elisp/internals
               ;; :cl-emacs/elisp/xfns
               ;; :cl-emacs/elisp-tests/editfns
               ;; :cl-emacs/elisp-tests/fileio
               ;; :cl-emacs/elisp
               ;; :cl-emacs/elisp-tests
               ;; :cl-emacs/log
               :cl-emacs/lib/commons
               :cl-emacs/types/pstrings
               :cl-emacs/elisp
               :cl-emacs/data
               :cl-emacs/lib/reader
               :cl-emacs/lib/printer
               :cl-emacs/main
               :cl-emacs/tests/read-print-test
               :cl-emacs/tests/all-tests
               )
  ;; :components ((:module "src/lisp"
  ;;               :components (
  ;;                            (:file "main" :depends-on ("log" "elisp-package"))
  ;;                            (:file "utils" :depends-on ("log"))
  ;;                            (:file "log")
  ;;                            (:file "elisp-package" :depends-on ("log" "utils"))
  ;;                            (:file "elisp-fileio" :depends-on ("log"))

  ;;                            ))
  ;;              )
  )
(register-system-packages :verbose '(:org.shirakumo.verbose))
