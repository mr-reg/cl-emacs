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

(unless (uiop:file-exists-p "cl-emacs.asd")
  (error "please run this program from folder with file cl-emacs.asd"))

(unless (probe-file (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))
  (error "please install ocicl to continue"))



(load (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))
(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:tree "~/.local/share/ocicl/")))

(asdf:load-system :swank)
(defvar *swank-port* 4008)
(format t "starting background swank on port ~a" *swank-port*)
(swank:create-server :port *swank-port*
                     :style swank:*communication-style*
                     :dont-close t)

(push (truename ".") asdf:*central-registry*)
(asdf:load-system :cl-emacs)
