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

(uiop:define-package :cl-emacs/xsmfns
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/xsmfns)
(log-enable :cl-emacs/xsmfns :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* handle-save-session ()
  #M"Handle the save_yourself event from a session manager.
A session manager can tell Emacs that the window system is shutting down
by sending Emacs a save_yourself message.  Emacs executes this function when
such an event occurs.  This function then executes ‘emacs-session-save'.
After that, this function informs the session manager that it can continue
or abort shutting down the window system depending on the return value
from ‘emacs-session-save'  If the return value is non-nil the session manager
is told to abort the window system shutdown.

Do not call this function yourself.

(fn EVENT)"
  (error 'unimplemented-error))
