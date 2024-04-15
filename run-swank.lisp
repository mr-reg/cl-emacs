#|
Copyright (C) 2024 by Gleb Borodulia
Author: Gleb Borodulia <mr.reg@mail.ru>

This file is part of cl-emacs.

cl-emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

cl-emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.
|#

(require :asdf)

(defvar *asdf-only* nil)
(format t "*asdf-only*=~s~%" *asdf-only*)

(unless (uiop:file-exists-p "run-swank.lisp")
  (error "please run this program from folder with file cl-emacs.asd"))

(unless (probe-file (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))
  (error "please install ocicl to continue"))



(load (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))
(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:tree "~/.local/share/ocicl/")))

;; (push (truename "~/.emacs.d/straight/repos/slime/") asdf:*central-registry*)

(asdf:load-system :swank)
;; (load (truename "~/.emacs.d/straight/repos/slime/swank-loader.lisp"))
;; (setq swank-loader::*fasl-directory* "/tmp/fasl/")
;; (swank-loader:init)





(push (truename "./src/lisp/") asdf:*central-registry*)
(ignore-errors
 (asdf:load-system :cl-emacs))


(unless *asdf-only*
  (defvar *swank-port* 4005)
  (format t "starting background swank on port ~a" *swank-port*)
  (setf swank-loader:*started-from-emacs* t)
  (swank:create-server :port *swank-port*
                       :style swank:*communication-style*
                       :interface "0.0.0.0"
                       :dont-close t)
  (print "run-swank.lisp complete")
  )

(when *asdf-only*
  (format t "asdf load complete")
  (quit))
;; (cl-emacs/main::main )
