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

(uiop:define-package :cl-emacs/dispnew
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/dispnew)
(log-enable :cl-emacs/dispnew :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* ding ()
  #M"Beep, or flash the screen.
Also, unless an argument is given,
terminate any keyboard macro currently executing.

(fn &optional ARG)"
  (error 'unimplemented-error))
(defun* display--update-for-mouse-movement ()
  #M"Handle mouse movement detected by Lisp code.

This function should be called when Lisp code detects the mouse has
moved, even if ‘track-mouse' is nil.  This handles updates that do not
rely on input events such as updating display for mouse-face
properties or updating the help echo text.

(fn MOUSE-X MOUSE-Y)"
  (error 'unimplemented-error))
(defun* frame-or-buffer-changed-p ()
  #M"Return non-nil if the frame and buffer state appears to have changed.
VARIABLE is a variable name whose value is either nil or a state vector
that will be updated to contain all frames and buffers,
aside from buffers whose names start with space,
along with the buffers' read-only and modified flags.  This allows a fast
check to see whether buffer menus might need to be recomputed.
If this function returns non-nil, it updates the internal vector to reflect
the current state.

If VARIABLE is nil, an internal variable is used.  Users should not
pass nil for VARIABLE.

(fn &optional VARIABLE)"
  (error 'unimplemented-error))
(defun* internal-show-cursor ()
  #M"Set the cursor-visibility flag of WINDOW to SHOW.
WINDOW nil means use the selected window.  SHOW non-nil means
show a cursor in WINDOW in the next redisplay.  SHOW nil means
don't show a cursor.

(fn WINDOW SHOW)"
  (error 'unimplemented-error))
(defun* internal-show-cursor-p ()
  #M"Value is non-nil if next redisplay will display a cursor in WINDOW.
WINDOW nil or omitted means report on the selected window.

(fn &optional WINDOW)"
  (error 'unimplemented-error))
(defun* open-termscript ()
  #M"Start writing all terminal output to FILE as well as the terminal.
FILE = nil means just close any termscript file currently open.

(fn FILE)"
  (error 'unimplemented-error))
(defun* redisplay ()
  #M"Perform redisplay.
Optional arg FORCE, if non-nil, prevents redisplay from being
preempted by arriving input, even if ‘redisplay-dont-pause' is nil.
If ‘redisplay-dont-pause' is non-nil (the default), redisplay is never
preempted by arriving input, so FORCE does nothing.

Return t if redisplay was performed, nil if redisplay was preempted
immediately by pending input.

(fn &optional FORCE)"
  (error 'unimplemented-error))
(defun* redraw-display ()
  #M"Clear and redisplay all visible frames.

(fn)"
  (error 'unimplemented-error))
(defun* redraw-frame ()
  #M"Clear frame FRAME and output again what is supposed to appear on it.
If FRAME is omitted or nil, the selected frame is used.

(fn &optional FRAME)"
  (error 'unimplemented-error))
(defun* send-string-to-terminal ()
  #M"Send STRING to the terminal without alteration.
Control characters in STRING will have terminal-dependent effects.

Optional parameter TERMINAL specifies the tty terminal device to use.
It may be a terminal object, a frame, or nil for the terminal used by
the currently selected frame.  In batch mode, STRING is sent to stdout
when TERMINAL is nil.

(fn STRING &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* sleep-for ()
  #M"Pause, without updating display, for SECONDS seconds.
SECONDS may be a floating-point value, meaning that you can wait for a
fraction of a second.  Optional second arg MILLISECONDS specifies an
additional wait period, in milliseconds; this is for backwards compatibility.
(Not all operating systems support waiting for a fraction of a second.)

(fn SECONDS &optional MILLISECONDS)"
  (error 'unimplemented-error))
