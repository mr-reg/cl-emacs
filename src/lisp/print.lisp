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

(uiop:define-package :cl-emacs/print
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/print)
(log-enable :cl-emacs/print :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* error-message-string () "Convert an error value (ERROR-SYMBOL . DATA) to an error message.
See Info anchor ‘(elisp)Definition of signal’ for some details on how this
error message is constructed.

(fn OBJ)"
  (error ’unimplemented-error))
(defun* external-debugging-output () "Write CHARACTER to stderr.
You can call ‘print’ while debugging emacs, and pass it this function
to make it write to the debugging output.

(fn CHARACTER)"
  (error ’unimplemented-error))
(defun* flush-standard-output () "Flush standard-output.
This can be useful after using ‘princ’ and the like in scripts.

(fn)"
  (error ’unimplemented-error))
(defun* prin1 () "Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that ‘read’
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by ‘print-level’ and ‘print-length’, which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker’s position;
   - a function, in which case that function is called once for each
     character of OBJECT’s printed representation;
   - a symbol, in which case that symbol’s function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of ‘standard-output’ (which see)
is used instead.

Optional argument OVERRIDES should be a list of settings for print-related
variables.  An element in this list can be the symbol t, which means \"reset
all the values to their defaults\".  Otherwise, an element should be a pair,
where the ‘car’ or the pair is the setting symbol, and the ‘cdr’ is the
value of the setting to use for this ‘prin1’ call.

For instance:

  (prin1 object nil \\=’((length . 100) (circle . t))).

See the manual entry ‘(elisp)Output Overrides’ for a list of possible
values.

As a special case, OVERRIDES can also simply be the symbol t, which
means \"use default values for all the print-related settings\".

(fn OBJECT &optional PRINTCHARFUN OVERRIDES)"
  (error ’unimplemented-error))
(defun* prin1-to-string () "Return a string containing the printed representation of OBJECT.
OBJECT can be any Lisp object.  This function outputs quoting characters
when necessary to make output that ‘read’ can handle, whenever possible,
unless the optional second argument NOESCAPE is non-nil.  For complex objects,
the behavior is controlled by ‘print-level’ and ‘print-length’, which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

See ‘prin1’ for the meaning of OVERRIDES.

A printed representation of an object is text which describes that object.

(fn OBJECT &optional NOESCAPE OVERRIDES)"
  (error ’unimplemented-error))
(defun* princ () "Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker’s position;
   - a function, in which case that function is called once for each
     character of OBJECT’s printed representation;
   - a symbol, in which case that symbol’s function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of ‘standard-output’ (which see)
is used instead.

(fn OBJECT &optional PRINTCHARFUN)"
  (error ’unimplemented-error))
(defun* print () "Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that ‘read’
can handle, whenever this is possible.  For complex objects, the behavior
is controlled by ‘print-level’ and ‘print-length’, which see.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker’s position;
   - a function, in which case that function is called once for each
     character of OBJECT’s printed representation;
   - a symbol, in which case that symbol’s function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of ‘standard-output’ (which see)
is used instead.

(fn OBJECT &optional PRINTCHARFUN)"
  (error ’unimplemented-error))
(defun* print--preprocess () "Extract sharing info from OBJECT needed to print it.
Fills ‘print-number-table’ if ‘print-circle’ is non-nil.  Does nothing
if ‘print-circle’ is nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* redirect-debugging-output () "Redirect debugging output (stderr stream) to file FILE.
If FILE is nil, reset target to the initial stderr stream.
Optional arg APPEND non-nil (interactively, with prefix arg) means
append to existing target file.

(fn FILE &optional APPEND)"
  (error ’unimplemented-error))
(defun* terpri () "Output a newline to stream PRINTCHARFUN.
If ENSURE is non-nil only output a newline if not already at the
beginning of a line.  Value is non-nil if a newline is printed.
If PRINTCHARFUN is omitted or nil, the value of ‘standard-output’ is used.

(fn &optional PRINTCHARFUN ENSURE)"
  (error ’unimplemented-error))
(defun* write-char () "Output character CHARACTER to stream PRINTCHARFUN.
PRINTCHARFUN defaults to the value of ‘standard-output’ (which see).

(fn CHARACTER &optional PRINTCHARFUN)"
  (error ’unimplemented-error))
