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

(uiop:define-package :cl-emacs/callint
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/callint)
(log-enable :cl-emacs/callint :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* call-interactively ()
  #M"Call FUNCTION, providing args according to its interactive calling specs.
Return the value FUNCTION returns.
The function contains a specification of how to do the argument reading.
In the case of user-defined functions, this is specified by placing a call
to the function ‘interactive' at the top level of the function body.
See ‘interactive'.

Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in the variable ‘command-history'.
Otherwise, this is done only if an arg is read using the minibuffer.

Optional third arg KEYS, if given, specifies the sequence of events to
supply, as a vector, if FUNCTION inquires which events were used to
invoke it (via an ‘interactive' spec that contains, for instance, an
\"e\" code letter).  If KEYS is omitted or nil, the return value of
‘this-command-keys-vector' is used.

(fn FUNCTION &optional RECORD-FLAG KEYS)"
  (error 'unimplemented-error))
(defun* funcall-interactively ()
  #M"Like ‘funcall' but marks the call as interactive.
I.e. arrange that within the called function ‘called-interactively-p' will
return non-nil.

(fn FUNCTION &rest ARGUMENTS)"
  (error 'unimplemented-error))
(defun* prefix-numeric-value ()
  #M"Return numeric meaning of raw prefix argument RAW.
A raw prefix argument is what you get from ‘(interactive \"P\")'.
Its numeric meaning is what you would get from ‘(interactive \"p\")'.

(fn RAW)"
  (error 'unimplemented-error))
