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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/lread
    (:use
     :defstar
     :cl-emacs/lib/log
     :fiveam
     :cl-emacs/lib/commons)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:reader #:cl-emacs/lib/reader))
  )
(in-package :cl-emacs/lread)
(log-enable :cl-emacs/lread :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* eval-buffer ()
  #M"Execute the accessible portion of current buffer as Lisp code.
You can use \\[narrow-to-region] to limit the part of buffer to be evaluated.
When called from a Lisp program (i.e., not interactively), this
function accepts up to five optional arguments:
BUFFER is the buffer to evaluate (nil means use current buffer),
 or a name of a buffer (a string).
PRINTFLAG controls printing of output by any output functions in the
 evaluated code, such as ‘print', ‘princ', and ‘prin1':
  a value of nil means discard it; anything else is the stream to print to.
  See Info node ‘(elisp)Output Streams' for details on streams.
FILENAME specifies the file name to use for ‘load-history'.
UNIBYTE, if non-nil, specifies ‘load-convert-to-unibyte' for this
 invocation.
DO-ALLOW-PRINT, if non-nil, specifies that output functions in the
 evaluated code should work normally even if PRINTFLAG is nil, in
 which case the output is displayed in the echo area.

This function ignores the current value of the ‘lexical-binding'
variable.  Instead it will heed any
  -*- lexical-binding: t -*-
settings in the buffer, and if there is no such setting, the buffer
will be evaluated without lexical binding.

This function preserves the position of point.

(fn &optional BUFFER PRINTFLAG FILENAME UNIBYTE DO-ALLOW-PRINT)"
  (error 'unimplemented-error))
(defun* eval-region ()
  #M"Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls output:
 a value of nil means discard it; anything else is stream for printing it.
 See Info node ‘(elisp)Output Streams' for details on streams.
Also the fourth argument READ-FUNCTION, if non-nil, is used
instead of ‘read' to read each expression.  It gets one argument
which is the input stream for reading characters.

This function does not move point.

(fn START END &optional PRINTFLAG READ-FUNCTION)"
  (error 'unimplemented-error))
(defun* get-file-char ()
  #M"Don't use this yourself.

(fn)"
  (error 'unimplemented-error))
(defun* get-load-suffixes ()
  #M"Return the suffixes that ‘load' should try if a suffix is required.
This uses the variables ‘load-suffixes' and ‘load-file-rep-suffixes'.

(fn)"
  (error 'unimplemented-error))
(defun* intern ()
  #M"Return the canonical symbol whose name is STRING.
If there is none, one is created by this function and returned.
A second optional argument specifies the obarray to use;
it defaults to the value of ‘obarray'.

(fn STRING &optional OBARRAY)"
  (error 'unimplemented-error))
(defun* intern-soft ()
  #M"Return the canonical symbol named NAME, or nil if none exists.
NAME may be a string or a symbol.  If it is a symbol, that exact
symbol is searched for.
A second optional argument specifies the obarray to use;
it defaults to the value of ‘obarray'.

(fn NAME &optional OBARRAY)"
  (error 'unimplemented-error))
(defun* locate-file-internal ()
  #M"Search for FILENAME through PATH.
Returns the file's name in absolute form, or nil if not found.
If SUFFIXES is non-nil, it should be a list of suffixes to append to
file name when searching.
If non-nil, PREDICATE is used instead of ‘file-readable-p'.
PREDICATE can also be an integer to pass to the faccessat(2) function,
in which case file-name-handlers are ignored.
This function will normally skip directories, so if you want it to find
directories, make sure the PREDICATE function returns ‘dir-ok' for them.

(fn FILENAME PATH &optional SUFFIXES PREDICATE)"
  (error 'unimplemented-error))
(defun* lread--substitute-object-in-subtree ()
  #M"In OBJECT, replace every occurrence of PLACEHOLDER with OBJECT.
COMPLETED is a hash table of objects that might be circular, or is t
if any object might be circular.

(fn OBJECT PLACEHOLDER COMPLETED)"
  (error 'unimplemented-error))
(defun* mapatoms ()
  #M"Call FUNCTION on every symbol in OBARRAY.
OBARRAY defaults to the value of ‘obarray'.

(fn FUNCTION &optional OBARRAY)"
  (error 'unimplemented-error))
(defun* read (source)
  #M"Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of ‘standard-input' (which see).
STREAM or the value of ‘standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it, or read from
    standard input in batch mode).

(fn &optional STREAM)"
  (cl:cond
    ;;; common lisp string here for is just for convenience
    ;; ((cl:stringp source)
    ;;  (reader:read-cl-string source))
    (t (error 'unimplemented-error))))
(defun* read-char ()
  #M"Read a character event from the command input (keyboard or macro).
It is returned as a number.
If the event has modifiers, they are resolved and reflected in the
returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
If some of the modifiers cannot be reflected in the character code, the
returned value will include those modifiers, and will not be a valid
character code: it will fail the ‘characterp' test.  Use ‘event-basic-type'
to recover the character code with the modifiers removed.

If the user generates an event which is not a character (i.e. a mouse
click or function key event), ‘read-char' signals an error.  As an
exception, switch-frame events are put off until non-character events
can be read.
If you want to read non-character events, or ignore them, call
‘read-event' or ‘read-char-exclusive' instead.

If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.

If ‘inhibit-interaction' is non-nil, this function will signal an
‘inhibited-interaction' error.

(fn &optional PROMPT INHERIT-INPUT-METHOD SECONDS)"
  (error 'unimplemented-error))
(defun* read-char-exclusive ()
  #M"Read a character event from the command input (keyboard or macro).
It is returned as a number.  Non-character events are ignored.
If the event has modifiers, they are resolved and reflected in the
returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
If some of the modifiers cannot be reflected in the character code, the
returned value will include those modifiers, and will not be a valid
character code: it will fail the ‘characterp' test.  Use ‘event-basic-type'
to recover the character code with the modifiers removed.

If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.

If ‘inhibit-interaction' is non-nil, this function will signal an
‘inhibited-interaction' error.

(fn &optional PROMPT INHERIT-INPUT-METHOD SECONDS)"
  (error 'unimplemented-error))
(defun* read-event ()
  #M"Read an event object from the input stream.

If you want to read non-character events, consider calling ‘read-key'
instead.  ‘read-key' will decode events via ‘input-decode-map' that
‘read-event' will not.  On a terminal this includes function keys such
as <F7> and <RIGHT>, or mouse events generated by ‘xterm-mouse-mode'.

If the optional argument PROMPT is non-nil, display that as a prompt.
If PROMPT is nil or the string \"\", the key sequence/events that led
to the current command is used as the prompt.

If the optional argument INHERIT-INPUT-METHOD is non-nil and some
input method is turned on in the current buffer, that input method
is used for reading a character.

If the optional argument SECONDS is non-nil, it should be a number
specifying the maximum number of seconds to wait for input.  If no
input arrives in that time, return nil.  SECONDS may be a
floating-point value.

If ‘inhibit-interaction' is non-nil, this function will signal an
‘inhibited-interaction' error.

(fn &optional PROMPT INHERIT-INPUT-METHOD SECONDS)"
  (error 'unimplemented-error))
(defun* read-from-string ()
  #M"Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
FINAL-STRING-INDEX is an integer giving the position of the next
remaining character in STRING.  START and END optionally delimit
a substring of STRING from which to read;  they default to 0 and
(length STRING) respectively.  Negative values are counted from
the end of STRING.

(fn STRING &optional START END)"
  (error 'unimplemented-error))
(defun* read-positioning-symbols ()
  #M"Read one Lisp expression as text from STREAM, return as Lisp object.
Convert each occurrence of a symbol into a \"symbol with pos\" object.

If STREAM is nil, use the value of ‘standard-input' (which see).
STREAM or the value of ‘standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it, or read from
    standard input in batch mode).

(fn &optional STREAM)"
  (error 'unimplemented-error))
(defun* unintern ()
  #M"Delete the symbol named NAME, if any, from OBARRAY.
The value is t if a symbol was found and deleted, nil otherwise.
NAME may be a string or a symbol.  If it is a symbol, that symbol
is deleted, if it belongs to OBARRAY--no other symbol is deleted.
OBARRAY, if nil, defaults to the value of the variable ‘obarray'.

(fn NAME OBARRAY)"
  (error 'unimplemented-error))
