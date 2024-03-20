#-asdf3.1 (error "CL-EMACS requires ASDF 3.1 or later.")
(asdf:defsystem :cl-emacs
  :class :package-inferred-system
  :version "0.0.1"
  :author "Gleb Borodulia <mr.reg@mail.ru>"
  :license "GPLv3"
  :long-name "cl-emacs"
  :depends-on (:cffi
               :uiop
               :bt-semaphore
               :verbose
               "cl-emacs/log"
               "cl-emacs/main"
               ))
