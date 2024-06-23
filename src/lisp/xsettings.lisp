;; Copyright (C) 2024 by Gleb Borodulia
;; Author: Gleb Borodulia <mr.reg@mail.ru>

;; This file is part of cl-emacs.

;; cl-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; cl-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.

(uiop:define-package :cl-emacs/xsettings
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/xsettings)
(log-enable :cl-emacs/xsettings :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* font-get-system-font ()
  #M"Get the system default fixed width font.
The font is returned as either a font-spec or font name.

(fn)"
  (error 'unimplemented-error))
(defun* font-get-system-normal-font ()
  #M"Get the system default application font.
The font is returned as either a font-spec or font name.

(fn)"
  (error 'unimplemented-error))
(defun* tool-bar-get-system-style ()
  #M"Get the system tool bar style.
If no system tool bar style is known, return ‘tool-bar-style' if set to a
known style.  Otherwise return image.

(fn)"
  (error 'unimplemented-error))
