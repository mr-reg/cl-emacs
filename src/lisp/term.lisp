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

(uiop:define-package :cl-emacs/term
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/term)
(log-enable :cl-emacs/term :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* controlling-tty-p ()
  #M"Return non-nil if TERMINAL is the controlling tty of the Emacs process.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL is not on a tty device.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* resume-tty ()
  #M"Resume the previously suspended terminal device TTY.
The terminal is opened and reinitialized.  Frames that are on the
suspended terminal are revived.

It is an error to resume a terminal while another terminal is active
on the same device.

This function runs ‘resume-tty-functions' after resuming the terminal.
The functions are run with one arg, the id of the resumed terminal
device.

‘resume-tty' does nothing if it is called on a device that is not
suspended.

TTY may be a terminal object, a frame, or nil (meaning the selected
frame's terminal).

(fn &optional TTY)"
  (error 'unimplemented-error))
(defun* suspend-tty ()
  #M"Suspend the terminal device TTY.

The device is restored to its default state, and Emacs ceases all
access to the tty device.  Frames that use the device are not deleted,
but input is not read from them and if they change, their display is
not updated.

TTY may be a terminal object, a frame, or nil for the terminal device
of the currently selected frame.

This function runs ‘suspend-tty-functions' after suspending the
device.  The functions are run with one arg, the id of the suspended
terminal device.

‘suspend-tty' does nothing if it is called on a device that is already
suspended.

A suspended tty may be resumed by calling ‘resume-tty' on it.

(fn &optional TTY)"
  (error 'unimplemented-error))
(defun* tty--output-buffer-size ()
  #M"Return the output buffer size of TTY.

TTY may be a terminal object, a frame, or nil (meaning the selected
frame's terminal).

A value of zero means TTY uses the system's default value.

(fn &optional TTY)"
  (error 'unimplemented-error))
(defun* tty--set-output-buffer-size ()
  #M"Set the output buffer size for a TTY.

SIZE zero means use the system's default value.  If SIZE is
non-zero, this also avoids flushing the output stream.

TTY may be a terminal object, a frame, or nil (meaning the selected
frame's terminal).

This function temporarily suspends and resumes the terminal
device.

(fn SIZE &optional TTY)"
  (error 'unimplemented-error))
(defun* tty-display-color-cells ()
  #M"Return the number of colors supported by the tty device TERMINAL.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns 0 if
TERMINAL does not refer to a text terminal.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* tty-display-color-p ()
  #M"Return non-nil if the tty device TERMINAL can display colors.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL does not refer to a text terminal.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* tty-no-underline ()
  #M"Declare that the tty used by TERMINAL does not handle underlining.
This is used to override the terminfo data, for certain terminals that
do not really do underlining, but say that they do.  This function has
no effect if used on a non-tty terminal.

TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).  This function always returns nil if
TERMINAL does not refer to a text terminal.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* tty-top-frame ()
  #M"Return the topmost terminal frame on TERMINAL.
TERMINAL can be a terminal object, a frame or nil (meaning the
selected frame's terminal).  This function returns nil if TERMINAL
does not refer to a text terminal.  Otherwise, it returns the
top-most frame on the text terminal.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* tty-type ()
  #M"Return the type of the tty device that TERMINAL uses.
Returns nil if TERMINAL is not on a tty device.

TERMINAL can be a terminal object, a frame, or nil (meaning the
selected frame's terminal).

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
