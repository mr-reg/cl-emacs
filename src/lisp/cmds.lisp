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

(uiop:define-package :cl-emacs/cmds
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/cmds)
(log-enable :cl-emacs/cmds :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* backward-char ()
  #M"Move point N characters backward (forward if N is negative).
On attempt to pass beginning or end of buffer, stop and signal error.
Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 character backward.

Depending on the bidirectional context, the movement may be to the
right or to the left on the screen.  This is in contrast with
\\[left-char], which see.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* beginning-of-line ()
  #M"Move point to beginning of current line (in the logical order).
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.

This function constrains point to the current field unless this moves
point to a different line from the original, unconstrained result.
If N is nil or 1, and a front-sticky field starts at point, the point
does not move.  To ignore field boundaries bind
‘inhibit-field-text-motion' to t, or use the ‘forward-line' function
instead.  For instance, ‘(forward-line 0)' does the same thing as
‘(beginning-of-line)', except that it ignores field boundaries.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* delete-char ()
  #M"Delete the following N characters (previous if N is negative).
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, N is the prefix arg, and KILLFLAG is set if
N was explicitly specified.

The command ‘delete-forward-char' is preferable for interactive use, e.g.
because it respects values of ‘delete-active-region' and ‘overwrite-mode'.

(fn N &optional KILLFLAG)"
  (error 'unimplemented-error))
(defun* end-of-line ()
  #M"Move point to end of current line (in the logical order).
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind ‘inhibit-point-motion-hooks' to t.

This function constrains point to the current field unless this moves
point to a different line from the original, unconstrained result.  If
N is nil or 1, and a rear-sticky field ends at point, the point does
not move.  To ignore field boundaries bind ‘inhibit-field-text-motion'
to t.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* forward-char ()
  #M"Move point N characters forward (backward if N is negative).
On reaching end or beginning of buffer, stop and signal error.
Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 character forward.

Depending on the bidirectional context, the movement may be to the
right or to the left on the screen.  This is in contrast with
\\[right-char], which see.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* forward-line ()
  #M"Move N lines forward (backward if N is negative).
Precisely, if point is on line I, move to the start of line I + N
(\"start of line\" in the logical order).
If there isn't room, go as far as possible (no error).
Interactively, N is the numeric prefix argument and defaults to 1.

Returns the count of lines left to move.  If moving forward,
that is N minus number of lines moved; if backward, N plus number
moved.

Exception: With positive N, a non-empty line at the end of the
buffer, or of its accessible portion, counts as one line
successfully moved (for the return value).  This means that the
function will move point to the end of such a line and will count
it as a line moved across, even though there is no next line to
go to its beginning.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* self-insert-command ()
  #M"Insert the character you type.
Whichever character C you type to run this command is inserted.
The numeric prefix argument N says how many times to repeat the insertion.
Before insertion, ‘expand-abbrev' is executed if the inserted character does
not have word syntax and the previous character in the buffer does.
After insertion, ‘internal-auto-fill' is called if
‘auto-fill-function' is non-nil and if the ‘auto-fill-chars' table has
a non-nil value for the inserted character.  At the end, it runs
‘post-self-insert-hook'.

(fn N &optional C)"
  (error 'unimplemented-error))
