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

(uiop:define-package :cl-emacs/xselect
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/xselect)
(log-enable :cl-emacs/xselect :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* x-disown-selection-internal ()
  #M"If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

Sets the last-change time for the selection to TIME-OBJECT (by default
the time of the last event).

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, the TIME-OBJECT and TERMINAL arguments are unused.
On MS-DOS, all this does is return non-nil if we own the selection.

(fn SELECTION &optional TIME-OBJECT TERMINAL)"
  (error 'unimplemented-error))
(defun* x-get-atom-name ()
  #M"Return the X atom name for VALUE as a string.
VALUE may be a number or a cons where the car is the upper 16 bits and
the cdr is the lower 16 bits of a 32 bit value.
Use the display for FRAME or the current frame if FRAME is not given or nil.

If the value is 0 or the atom is not known, return the empty string.

(fn VALUE &optional FRAME)"
  (error 'unimplemented-error))
(defun* x-get-local-selection ()
  #M"Run selection converters for VALUE, and return the result.
TARGET is the selection target that is used to find a suitable
converter.  VALUE is a list of 4 values NAME, SELECTION-VALUE,
TIMESTAMP and FRAME.  NAME is the name of the selection that will be
passed to selection converters, SELECTION-VALUE is the value of the
selection used by the converter, TIMESTAMP is not meaningful (but must
be a number that fits in an X timestamp), and FRAME is the frame
describing the terminal for which the selection converter will be
run.

(fn &optional VALUE TARGET)"
  (error 'unimplemented-error))
(defun* x-get-selection-internal ()
  #M"Return text selected from some X window.
SELECTION-SYMBOL is typically ‘PRIMARY', ‘SECONDARY', or ‘CLIPBOARD'.
(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically ‘STRING'.

TIME-STAMP is the time to use in the XConvertSelection call for foreign
selections.  If omitted, defaults to the time for the last event.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TIME-STAMP and TERMINAL are unused.

(fn SELECTION-SYMBOL TARGET-TYPE &optional TIME-STAMP TERMINAL)"
  (error 'unimplemented-error))
(defun* x-own-selection-internal ()
  #M"Assert an X selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically ‘PRIMARY', ‘SECONDARY', or ‘CLIPBOARD'.
(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on ‘selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame.

On Nextstep, FRAME is unused.

(fn SELECTION VALUE &optional FRAME)"
  (error 'unimplemented-error))
(defun* x-register-dnd-atom ()
  #M"Request that dnd events are made for ClientMessages with ATOM.
ATOM can be a symbol or a string.  The ATOM is interned on the display that
FRAME is on.  If FRAME is nil, the selected frame is used.

(fn ATOM &optional FRAME)"
  (error 'unimplemented-error))
(defun* x-selection-exists-p ()
  #M"Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols ‘PRIMARY', ‘SECONDARY', ‘CLIPBOARD', or
‘CLIPBOARD_MANAGER' (X expects these literal upper-case names.)  The
symbol nil is the same as ‘PRIMARY', and t is the same as ‘SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.

(fn &optional SELECTION TERMINAL)"
  (error 'unimplemented-error))
(defun* x-selection-owner-p ()
  #M"Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols ‘PRIMARY', ‘SECONDARY', or ‘CLIPBOARD'.
(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as ‘PRIMARY',
and t is the same as ‘SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.

(fn &optional SELECTION TERMINAL)"
  (error 'unimplemented-error))
(defun* x-send-client-message ()
  #M"Send a client message of MESSAGE-TYPE to window DEST on DISPLAY.

For DISPLAY, specify either a frame or a display name (a string).
If DISPLAY is nil, that stands for the selected frame's display.
DEST may be a number, in which case it is a Window id.  The value 0 may
be used to send to the root window of the DISPLAY.
If DEST is a cons, it is converted to a 32 bit number
with the high 16 bits from the car and the lower 16 bit from the cdr.  That
number is then used as a window id.
If DEST is a frame the event is sent to the outer window of that frame.
A value of nil means the currently selected frame.
If DEST is the string \"PointerWindow\" the event is sent to the window that
contains the pointer.  If DEST is the string \"InputFocus\" the event is
sent to the window that has the input focus.
FROM is the frame sending the event.  Use nil for currently selected frame.
MESSAGE-TYPE is the name of an Atom as a string.
FORMAT must be one of 8, 16 or 32 and determines the size of the values in
bits.  VALUES is a list of numbers, cons and/or strings containing the values
to send.  If a value is a string, it is converted to an Atom and the value of
the Atom is sent.  If a value is a cons, it is converted to a 32 bit number
with the high 16 bits from the car and the lower 16 bit from the cdr.
If more values than fits into the event is given, the excessive values
are ignored.

Wait for the event to be sent and signal any error, unless
‘x-fast-protocol-requests' is non-nil, in which case errors will be
silently ignored.

(fn DISPLAY DEST FROM MESSAGE-TYPE FORMAT VALUES)"
  (error 'unimplemented-error))
