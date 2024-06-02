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

(uiop:define-package :cl-emacs/coding
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/coding)
(log-enable :cl-emacs/coding :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* check-coding-system ()
  #M"Check validity of CODING-SYSTEM.
If valid, return CODING-SYSTEM, else signal a ‘coding-system-error' error.
It is valid if it is nil or a symbol defined as a coding system by the
function ‘define-coding-system'.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* check-coding-systems-region ()
  #M"Check if text between START and END is encodable by CODING-SYSTEM-LIST.

START and END are buffer positions specifying the region.
CODING-SYSTEM-LIST is a list of coding systems to check.

If all coding systems in CODING-SYSTEM-LIST can encode the region, the
function returns nil.

If some of the coding systems cannot encode the whole region, value is
an alist, each element of which has the form (CODING-SYSTEM POS1 POS2 ...),
which means that CODING-SYSTEM cannot encode the text at buffer positions
POS1, POS2, ...

START may be a string.  In that case, check if the string is
encodable, and the value contains character indices into the string
instead of buffer positions.  END is ignored in this case.

If the current buffer (or START if it is a string) is unibyte, the value
is nil.

(fn START END CODING-SYSTEM-LIST)"
  (error 'unimplemented-error))
(defun* coding-system-aliases ()
  #M"Return the list of aliases of CODING-SYSTEM.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* coding-system-base ()
  #M"Return the base of CODING-SYSTEM.
Any alias or subsidiary coding system is not a base coding system.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* coding-system-eol-type ()
  #M"Return eol-type of CODING-SYSTEM.
An eol-type is an integer 0, 1, 2, or a vector of coding systems.

Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
and CR respectively.

A vector value indicates that a format of end-of-line should be
detected automatically.  Nth element of the vector is the subsidiary
coding system whose eol-type is N.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* coding-system-p ()
  #M"Return t if OBJECT is nil or a coding-system.
See the documentation of ‘define-coding-system' for information
about coding-system objects.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* coding-system-plist ()
  #M"Return the property list of CODING-SYSTEM.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* coding-system-priority-list ()
  #M"Return a list of coding systems ordered by their priorities.
The list contains a subset of coding systems; i.e. coding systems
assigned to each coding category (see ‘coding-category-list').

HIGHESTP non-nil means just return the highest priority one.

(fn &optional HIGHESTP)"
  (error 'unimplemented-error))
(defun* coding-system-put ()
  #M"Change value of CODING-SYSTEM's property PROP to VAL.

The following properties, if set by this function, override the values
of the corresponding attributes set by ‘define-coding-system':

  ‘:mnemonic', ‘:default-char', ‘:ascii-compatible-p'
  ‘:decode-translation-table', ‘:encode-translation-table',
  ‘:post-read-conversion', ‘:pre-write-conversion'

See ‘define-coding-system' for the description of these properties.
See ‘coding-system-get' and ‘coding-system-plist' for accessing the
property list of a coding-system.

(fn CODING-SYSTEM PROP VAL)"
  (error 'unimplemented-error))
(defun* decode-big5-char ()
  #M"Decode a Big5 character which has CODE in BIG5 coding system.
Return the corresponding character.

(fn CODE)"
  (error 'unimplemented-error))
(defun* decode-coding-region ()
  #M"Decode the current region using the specified coding system.
Interactively, prompt for the coding system to decode the region, and
replace the region with the decoded text.

\"Decoding\" means transforming bytes into readable text (characters).
If, for instance, you have a region that contains data that represents
the two bytes #xc2 #xa9, after calling this function with the utf-8
coding system, the region will contain the single
character ?\\N{COPYRIGHT SIGN}.

When called from a program, takes four arguments:
	START, END, CODING-SYSTEM, and DESTINATION.
START and END are buffer positions.

Optional 4th arguments DESTINATION specifies where the decoded text goes.
If nil, the region between START and END is replaced by the decoded text.
If buffer, the decoded text is inserted in that buffer after point (point
does not move).  If that buffer is unibyte, it receives the individual
bytes of the internal representation of the decoded text.
In those cases, the length of the decoded text is returned.
If DESTINATION is t, the decoded text is returned.

This function sets ‘last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)

(fn START END CODING-SYSTEM &optional DESTINATION)"
  (error 'unimplemented-error))
(defun* decode-coding-string ()
  #M"Decode STRING which is encoded in CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING itself
if the decoding operation is trivial.

Optional fourth arg BUFFER non-nil means that the decoded text is
inserted in that buffer after point (point does not move).  In this
case, the return value is the length of the decoded text.  If that
buffer is unibyte, it receives the individual bytes of the internal
representation of the decoded text.

This function sets ‘last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  The function does not change the match data.

(fn STRING CODING-SYSTEM &optional NOCOPY BUFFER)"
  (error 'unimplemented-error))
(defun* decode-sjis-char ()
  #M"Decode a Japanese character which has CODE in shift_jis encoding.
Return the corresponding character.

(fn CODE)"
  (error 'unimplemented-error))
(defun* define-coding-system-alias ()
  #M"Define ALIAS as an alias for CODING-SYSTEM.

(fn ALIAS CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* define-coding-system-internal ()
  #M"For internal use only.

(fn ...)"
  (error 'unimplemented-error))
(defun* detect-coding-region ()
  #M"Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority.
The coding systems to try and their priorities follows what
the function ‘coding-system-priority-list' (which see) returns.

If only ASCII characters are found (except for such ISO-2022 control
characters as ESC), it returns a list of single element ‘undecided'
or its subsidiary coding system according to a detected end-of-line
format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.

(fn START END &optional HIGHEST)"
  (error 'unimplemented-error))
(defun* detect-coding-string ()
  #M"Detect coding system of the text in STRING.
Return a list of possible coding systems ordered by priority.
The coding systems to try and their priorities follows what
the function ‘coding-system-priority-list' (which see) returns.

If only ASCII characters are found (except for such ISO-2022 control
characters as ESC), it returns a list of single element ‘undecided'
or its subsidiary coding system according to a detected end-of-line
format.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority.

(fn STRING &optional HIGHEST)"
  (error 'unimplemented-error))
(defun* encode-big5-char ()
  #M"Encode the Big5 character CH to BIG5 coding system.
Return the corresponding character code in Big5.

(fn CH)"
  (error 'unimplemented-error))
(defun* encode-coding-region ()
  #M"Encode the current region using th specified coding system.
Interactively, prompt for the coding system to encode the region, and
replace the region with the bytes that are the result of the encoding.

What's meant by \"encoding\" is transforming textual data (characters)
into bytes.  If, for instance, you have a region that contains the
single character ?\\N{COPYRIGHT SIGN}, after calling this function with
the utf-8 coding system, the data in the region will represent the two
bytes #xc2 #xa9.

When called from a program, takes four arguments:
        START, END, CODING-SYSTEM and DESTINATION.
START and END are buffer positions.

Optional 4th argument DESTINATION specifies where the encoded text goes.
If nil, the region between START and END is replaced by the encoded text.
If buffer, the encoded text is inserted in that buffer after point (point
does not move).
In those cases, the length of the encoded text is returned.
If DESTINATION is t, the encoded text is returned.

This function sets ‘last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)

(fn START END CODING-SYSTEM &optional DESTINATION)"
  (error 'unimplemented-error))
(defun* encode-coding-string ()
  #M"Encode STRING to CODING-SYSTEM, and return the result.

Optional third arg NOCOPY non-nil means it is OK to return STRING
itself if the encoding operation is trivial.

Optional fourth arg BUFFER non-nil means that the encoded text is
inserted in that buffer after point (point does not move).  In this
case, the return value is the length of the encoded text.

This function sets ‘last-coding-system-used' to the precise coding system
used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
not fully specified.)  The function does not change the match data.

(fn STRING CODING-SYSTEM &optional NOCOPY BUFFER)"
  (error 'unimplemented-error))
(defun* encode-sjis-char ()
  #M"Encode a Japanese character CH to shift_jis encoding.
Return the corresponding code in SJIS.

(fn CH)"
  (error 'unimplemented-error))
(defun* find-coding-systems-region-internal ()
  #M"Internal use only.

(fn START END &optional EXCLUDE)"
  (error 'unimplemented-error))
(defun* find-operation-coding-system ()
  #M"Choose a coding system for an operation based on the target name.
The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
DECODING-SYSTEM is the coding system to use for decoding
(in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
for encoding (in case OPERATION does encoding).

The first argument OPERATION specifies an I/O primitive:
  For file I/O, ‘insert-file-contents' or ‘write-region'.
  For process I/O, ‘call-process', ‘call-process-region', or ‘start-process'.
  For network I/O, ‘open-network-stream'.

The remaining arguments should be the same arguments that were passed
to the primitive.  Depending on which primitive, one of those arguments
is selected as the TARGET.  For example, if OPERATION does file I/O,
whichever argument specifies the file name is TARGET.

TARGET has a meaning which depends on OPERATION:
  For file I/O, TARGET is a file name (except for the special case below).
  For process I/O, TARGET is a process name.
  For network I/O, TARGET is a service name or a port number.

This function looks up what is specified for TARGET in
‘file-coding-system-alist', ‘process-coding-system-alist',
or ‘network-coding-system-alist' depending on OPERATION.
They may specify a coding system, a cons of coding systems,
or a function symbol to call.
In the last case, we call the function with one argument,
which is a list of all the arguments given to this function.
If the function can't decide a coding system, it can return
‘undecided' so that the normal code-detection is performed.

If OPERATION is ‘insert-file-contents', the argument corresponding to
TARGET may be a cons (FILENAME . BUFFER).  In that case, FILENAME is a
file name to look up, and BUFFER is a buffer that contains the file's
contents (not yet decoded).  If ‘file-coding-system-alist' specifies a
function to call for FILENAME, that function should examine the
contents of BUFFER instead of reading the file.

(fn OPERATION ARGUMENTS...)"
  (error 'unimplemented-error))
(defun* keyboard-coding-system ()
  #M"Return coding system specified for decoding keyboard input.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* read-coding-system ()
  #M"Read a coding system from the minibuffer, prompting with string PROMPT.
If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
Ignores case when completing coding systems (all Emacs coding systems
are lower-case).

(fn PROMPT &optional DEFAULT-CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* read-non-nil-coding-system ()
  #M"Read a coding system from the minibuffer, prompting with string PROMPT.

(fn PROMPT)"
  (error 'unimplemented-error))
(defun* set-coding-system-priority ()
  #M"Assign higher priority to the coding systems given as arguments.
If multiple coding systems belong to the same category,
all but the first one are ignored.

(fn &rest coding-systems)"
  (error 'unimplemented-error))
(defun* set-keyboard-coding-system-internal ()
  #M"Internal use only.

(fn CODING-SYSTEM &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* set-safe-terminal-coding-system-internal ()
  #M"Internal use only.

(fn CODING-SYSTEM)"
  (error 'unimplemented-error))
(defun* set-terminal-coding-system-internal ()
  #M"Internal use only.

(fn CODING-SYSTEM &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* terminal-coding-system ()
  #M"Return coding system specified for terminal output on the given terminal.
TERMINAL may be a terminal object, a frame, or nil for the selected
frame's terminal device.

(fn &optional TERMINAL)"
  (error 'unimplemented-error))
(defun* unencodable-char-position ()
  #M"Return position of first un-encodable character in a region.
START and END specify the region and CODING-SYSTEM specifies the
encoding to check.  Return nil if CODING-SYSTEM does encode the region.

If optional 4th argument COUNT is non-nil, it specifies at most how
many un-encodable characters to search.  In this case, the value is a
list of positions.

If optional 5th argument STRING is non-nil, it is a string to search
for un-encodable characters.  In that case, START and END are indexes
to the string and treated as in ‘substring'.

(fn START END CODING-SYSTEM &optional COUNT STRING)"
  (error 'unimplemented-error))
