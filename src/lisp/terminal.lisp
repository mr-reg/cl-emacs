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

(uiop:define-package :cl-emacs/terminal
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/terminal)
(log-enable :cl-emacs/terminal :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* delete-terminal ()
  #M"Delete TERMINAL by deleting all frames on it and closing the terminal.
TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

Normally, you may not delete a display if all other displays are suspended,
but if the second argument FORCE is non-nil, you may do so.

(fn &optional TERMINAL FORCE)"
  (error 'unimplemented-error))
(defun* frame-terminal ()
  #M"Return the terminal that FRAME is displayed on.
If FRAME is nil, use the selected frame.

The terminal device is represented by its integer identifier.

(fn &optional FRAME)"
  (error 'unimplemented-error))
(defun* set-terminal-parameter ()
  #M"Set TERMINAL's value for parameter PARAMETER to VALUE.
Return the previous value of PARAMETER.

TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).

(fn TERMINAL PARAMETER VALUE)"
  (error 'unimplemented-error))
(defun* terminal-list ()
  #M"Return a list of all terminal devices.

(fn)"
  (error 'unimplemented-error))
(defun* terminal-live-p ()
  #M"Return non-nil if OBJECT is a terminal which has not been deleted.
Return nil if OBJECT is not a live display terminal.
OBJECT may be a terminal object, a frame, or nil (meaning the
selected frame's terminal).
If OBJECT is a live display terminal, return what sort of output
terminal it uses.  See the documentation of ‘framep' for possible
return values.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* terminal-name ()
  #M"Return the name of the terminal device TERMINAL.
It is not guaranteed that the returned value is unique among opened devices.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* terminal-parameter ()
  #M"Return TERMINAL's value for parameter PARAMETER.
TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

(fn TERMINAL PARAMETER)"
  (error 'unimplemented-error))
(defun* terminal-parameters ()
  #M"Return the parameter-alist of terminal TERMINAL.
The value is a list of elements of the form (PARM . VALUE), where PARM
is a symbol.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
