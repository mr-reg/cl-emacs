(asdf:defsystem :cl-emacs
  :class :package-inferred-system
  :version "0.0.1"
  :author "Gleb Borodulia <mr.reg@mail.ru>"
  :license "GPLv3"
  :long-name "cl-emacs"
  :depends-on (:cffi
               :uiop
               "cl-emacs/src/lisp/main"))
