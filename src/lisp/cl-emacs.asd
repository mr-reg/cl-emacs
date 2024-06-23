#-asdf3.1 (error "CL-EMACS requires ASDF 3.1 or later.")
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
                         ;; :pzmq
                         :serapeum
                         :snakes
                         :str
                         :trees
                         :vom
                         :uiop
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
               :cl-emacs/main
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
