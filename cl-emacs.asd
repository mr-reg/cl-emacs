#-asdf3.1 (error "CL-EMACS requires ASDF 3.1 or later.")
(asdf:defsystem :cl-emacs
  :version "0.0.1"
  :author "Gleb Borodulia <mr.reg@mail.ru>"
  :license "GPLv3"
  :long-name "cl-emacs"
  :depends-on (:cffi
               :closer-mop
               :flexi-streams
               :lisp-binary
               :moptilities
               :osicat
               :trivial-gray-streams
               :quasiquote-2.0
               :str
               :verbose
               :uiop
               :usocket
               :unix-sockets
               )
  :components ((:module "src/lisp"
                :components ((:file "main" :depends-on ("log" "elisp-package"))
                             (:file "utils" :depends-on ("log"))
                             (:file "log")
                             (:file "elisp-package" :depends-on ("log" "utils"))
                             (:file "elisp-fileio" :depends-on ("elisp-package"))
                             
                             ))
               ))
