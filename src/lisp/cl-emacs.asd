#-asdf3.1 (error "CL-EMACS requires ASDF 3.1 or later.")
(asdf:defsystem :cl-emacs
  :version "0.0.1"
  :author "Gleb Borodulia <mr.reg@mail.ru>"
  :license "GPLv3"
  :long-name "cl-emacs"
  :class :package-inferred-system
  :defsystem-depends-on (:cffi
                         :cl-ppcre
                         :fiveam
                         :flexi-streams
                         :lisp-binary
                         :str
                         :verbose
                         :uiop
                         :usocket)
  :depends-on (:cl-emacs/main
               :cl-emacs/log
               :cl-emacs/utils
               :cl-emacs/elisp/fileio
               :cl-emacs/elisp/internals
               :cl-emacs/elisp
               :cl-emacs/elisp-tests/fileio
               :cl-emacs/elisp-tests
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
