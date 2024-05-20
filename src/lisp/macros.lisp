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

(uiop:define-package :cl-emacs/macros
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/macros)
(log-enable :cl-emacs/macros :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* call-last-kbd-macro () "Call the last keyboard macro that you defined with \\[start-kbd-macro].

A prefix argument serves as a repeat count.  Zero means repeat until error.

To make a macro permanent so you can call it even after
defining others, use \\[name-last-kbd-macro].

In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.

(fn &optional PREFIX LOOPFUNC)"
  (error ’unimplemented-error))
(defun* cancel-kbd-macro-events () "Cancel the events added to a keyboard macro for this command.

(fn)"
  (error ’unimplemented-error))
(defun* defining-kbd-macro () "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
Non-nil arg (prefix arg) means append to last macro defined;
this begins by re-executing that macro as if you typed it again.
If optional second arg, NO-EXEC, is non-nil, do not re-execute last
macro before appending to it.

(fn APPEND &optional NO-EXEC)"
  (error ’unimplemented-error))
(defun* end-kbd-macro () "Finish defining a keyboard macro.
The definition was started by \\[start-kbd-macro].
The macro is now available for use via \\[call-last-kbd-macro],
or it can be given a name with \\[name-last-kbd-macro] and then invoked
under that name.

With numeric arg, repeat macro now that many times,
counting the definition just completed as the first repetition.
An argument of zero means repeat until error.

In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.

(fn &optional REPEAT LOOPFUNC)"
  (error ’unimplemented-error))
(defun* execute-kbd-macro () "Execute MACRO as a sequence of events.
If MACRO is a string or vector, then the events in it are executed
exactly as if they had been input by the user.

If MACRO is a symbol, its function definition is used.  If that is
another symbol, this process repeats.  Eventually the result should be
a string or vector.  If the result is not a symbol, string, or vector,
an error is signaled.

COUNT is a repeat count, or nil for once, or 0 for infinite loop.

Optional third arg LOOPFUNC may be a function that is called prior to
each iteration of the macro.  Iteration stops if LOOPFUNC returns nil.

The buffer shown in the currently selected window will be made the current
buffer before the macro is executed.

(fn MACRO &optional COUNT LOOPFUNC)"
  (error ’unimplemented-error))
(defun* start-kbd-macro () "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
Non-nil arg (prefix arg) means append to last macro defined;
this begins by re-executing that macro as if you typed it again.
If optional second arg, NO-EXEC, is non-nil, do not re-execute last
macro before appending to it.

(fn APPEND &optional NO-EXEC)"
  (error ’unimplemented-error))
(defun* store-kbd-macro-event () "Store EVENT into the keyboard macro being defined.

(fn EVENT)"
  (error ’unimplemented-error))
