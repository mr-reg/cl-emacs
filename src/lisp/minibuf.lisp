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

(uiop:define-package :cl-emacs/minibuf
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/minibuf)
(log-enable :cl-emacs/minibuf :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* abort-minibuffers ()
  #M"Abort the current minibuffer.
If we are not currently in the innermost minibuffer, prompt the user to
confirm the aborting of the current minibuffer and all contained ones.

(fn)"
  (error 'unimplemented-error))
(defun* active-minibuffer-window ()
  #M"Return the currently active minibuffer window, or nil if none.

(fn)"
  (error 'unimplemented-error))
(defun* all-completions ()
  #M"Search for partial matches of STRING in COLLECTION.

Test each possible completion specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
by using ‘symbol-name'.

The value is a list of all the possible completions that match STRING.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is the possible completion.
If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: STRING, PREDICATE and t.
Whatever it returns becomes the value of ‘all-completions'.

If optional third argument PREDICATE is non-nil, it must be a function
of one or two arguments, and is used to test each possible completion.
A possible completion is accepted only if PREDICATE returns non-nil.

The argument given to PREDICATE is either a string or a cons cell (whose
car is a string) from the alist, or a symbol from the obarray.
If COLLECTION is a hash-table, PREDICATE is called with two arguments:
the string key and the associated value.

To be acceptable, a possible completion must also match all the regexps
in ‘completion-regexp-list' (unless COLLECTION is a function, in
which case that function should itself handle ‘completion-regexp-list').

An obsolete optional fourth argument HIDE-SPACES is still accepted for
backward compatibility.  If non-nil, strings in COLLECTION that start
with a space are ignored unless STRING itself starts with a space.

(fn STRING COLLECTION &optional PREDICATE HIDE-SPACES)"
  (error 'unimplemented-error))
(defun* assoc-string ()
  #M"Like ‘assoc' but specifically for strings (and symbols).

This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, both
KEY and the elements of LIST are upcased for comparison.

Unlike ‘assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.

(fn KEY LIST &optional CASE-FOLD)"
  (error 'unimplemented-error))
(defun* completing-read ()
  #M"Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
COLLECTION can also be a function to do the completion itself.
PREDICATE limits completion to a subset of COLLECTION.
See ‘try-completion', ‘all-completions', ‘test-completion',
and ‘completion-boundaries', for more details on completion,
COLLECTION, and PREDICATE.  See also Info node ‘(elisp)Basic Completion'
for the details about completion, and Info node ‘(elisp)Programmed
Completion' for expectations from COLLECTION when it's a function.

REQUIRE-MATCH can take the following values:
- t means that the user is not allowed to exit unless the input is (or
  completes to) an element of COLLECTION or is null.
- nil means that the user can exit with any input.
- ‘confirm' means that the user can exit with any input, but she needs
  to confirm her choice if the input is not an element of COLLECTION.
- ‘confirm-after-completion' means that the user can exit with any
  input, but she needs to confirm her choice if she called
  ‘minibuffer-complete' right before ‘minibuffer-complete-and-exit'
  and the input is not an element of COLLECTION.
- a function, which will be called with the input as the
  argument.  If the function returns a non-nil value, the
  minibuffer is exited with that argument as the value.
- anything else behaves like t except that typing RET does not exit if it
  does non-null completion.

If the input is null, ‘completing-read' returns DEF, or the first
element of the list of default values, or an empty string if DEF is
nil, regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
  with point positioned at the end.  If it is (STRING . POSITION), the
  initial input is STRING, but point is placed at _zero-indexed_
  position POSITION in STRING.  (*Note* that this is different from
  ‘read-from-minibuffer' and related functions, which use one-indexing
  for POSITION.)  This feature is deprecated--it is best to pass nil
  for INITIAL-INPUT and supply the default value DEF instead.  The
  user can yank the default value into the minibuffer easily using
  \\<minibuffer-local-map>\\[next-history-element].

HIST, if non-nil, specifies a history list and optionally the initial
  position in the list.  It can be a symbol, which is the history list
  variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
  that case, HISTVAR is the history list variable to use, and HISTPOS
  is the initial position (the position in the list used by the
  minibuffer history commands).  For consistency, you should also
  specify that element of the history as the value of INITIAL-INPUT.
  (This is the only case in which you should use INITIAL-INPUT instead
  of DEF.)  Positions are counted starting from 1 at the beginning of
  the list.  The variable ‘history-length' controls the maximum length
  of a history list.  If HIST is t, history is not recorded.

DEF, if non-nil, is the default value or the list of default values.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
  current input method and the setting of ‘enable-multibyte-characters'.

Completion ignores case if the ambient value of
  ‘completion-ignore-case' is non-nil.

See also ‘completing-read-function'.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
  (error 'unimplemented-error))
(defun* innermost-minibuffer-p ()
  #M"Return t if BUFFER is the most nested active minibuffer.
No argument or nil as argument means use the current buffer as BUFFER.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* internal-complete-buffer ()
  #M"Perform completion on buffer names.
STRING and PREDICATE have the same meanings as in ‘try-completion',
‘all-completions', and ‘test-completion'.

If FLAG is nil, invoke ‘try-completion'; if it is t, invoke
‘all-completions'; otherwise invoke ‘test-completion'.

(fn STRING PREDICATE FLAG)"
  (error 'unimplemented-error))
(defun* minibuffer-contents ()
  #M"Return the user input in a minibuffer as a string.
If the current buffer is not a minibuffer, return its entire contents.

(fn)"
  (error 'unimplemented-error))
(defun* minibuffer-contents-no-properties ()
  #M"Return the user input in a minibuffer as a string, without text-properties.
If the current buffer is not a minibuffer, return its entire contents.

(fn)"
  (error 'unimplemented-error))
(defun* minibuffer-depth ()
  #M"Return current depth of activations of minibuffer, a nonnegative integer.

(fn)"
  (error 'unimplemented-error))
(defun* minibuffer-innermost-command-loop-p ()
  #M"Return t if BUFFER is a minibuffer at the current command loop level.
No argument or nil as argument means use the current buffer as BUFFER.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* minibuffer-prompt ()
  #M"Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.

(fn)"
  (error 'unimplemented-error))
(defun* minibuffer-prompt-end ()
  #M"Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a minibuffer.

(fn)"
  (error 'unimplemented-error))
(defun* minibufferp ()
  #M"Return t if BUFFER is a minibuffer.
No argument or nil as argument means use current buffer as BUFFER.
BUFFER can be a buffer or a buffer name.  If LIVE is non-nil, then
return t only if BUFFER is an active minibuffer.

(fn &optional BUFFER LIVE)"
  (error 'unimplemented-error))
(defun* read-buffer ()
  #M"Read the name of a buffer and return it as a string.
Prompt with PROMPT, which should be a string ending with a colon and a space.
Provides completion on buffer names the user types.
Optional second arg DEF is value to return if user enters an empty line,
 instead of that empty string.
 If DEF is a list of default values, return its first element.
Optional third arg REQUIRE-MATCH has the same meaning as the
 REQUIRE-MATCH argument of ‘completing-read'.
Optional arg PREDICATE, if non-nil, is a function limiting the buffers that
can be considered.  It will be called with each potential candidate, in
the form of either a string or a cons cell whose ‘car' is a string, and
should return non-nil to accept the candidate for completion, nil otherwise.
If ‘read-buffer-completion-ignore-case' is non-nil, completion ignores
case while reading the buffer name.
If ‘read-buffer-function' is non-nil, this works by calling it as a
function, instead of the usual behavior.

(fn PROMPT &optional DEF REQUIRE-MATCH PREDICATE)"
  (error 'unimplemented-error))
(defun* read-command ()
  #M"Read the name of a command and return as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list.  If DEFAULT-VALUE is omitted or nil, and the user enters
null input, return a symbol whose name is an empty string.

(fn PROMPT &optional DEFAULT-VALUE)"
  (error 'unimplemented-error))
(defun* read-from-minibuffer ()
  #M"Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an obsolete alternative to
  DEFAULT-VALUE.  It normally should be nil in new code, except when
  HIST is a cons.  It is discussed in more detail below.

Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is ‘minibuffer-local-map'.

If fourth arg READ is non-nil, interpret the result as a Lisp object
  and return that object:
  in other words, do ‘(car (read-from-string INPUT-STRING))'

Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use, and
  HISTPOS is the initial position for use by the minibuffer history
  commands.  For consistency, you should also specify that element of
  the history as the value of INITIAL-CONTENTS.  Positions are counted
  starting from 1 at the beginning of the list.  If HIST is nil, the
  default history list ‘minibuffer-history' is used.  If HIST is t,
  history is not recorded.

  If ‘history-add-new-input' is non-nil (the default), the result will
  be added to the history list using ‘add-to-history'.

Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
  as the default to ‘read' if READ is non-nil and the user enters
  empty input.  But if READ is nil, this function does _not_ return
  DEFAULT-VALUE for empty input!  Instead, it returns the empty string.

  Whatever the value of READ, DEFAULT-VALUE is made available via the
  minibuffer history commands.  DEFAULT-VALUE can also be a list of
  strings, in which case all the strings are available in the history,
  and the first string is the default to ‘read' if READ is non-nil.

Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of ‘enable-multibyte-characters'.

If the variable ‘minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

If ‘inhibit-interaction' is non-nil, this function will signal an
  ‘inhibited-interaction' error.

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  It is only relevant when
studying existing code, or when HIST is a cons.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
input is STRING, but point is placed at _one-indexed_ position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  *Note* that this
behavior differs from the way such arguments are used in ‘completing-read'
and some related functions, which use zero-indexing for POSITION.

(fn PROMPT &optional INITIAL-CONTENTS KEYMAP READ HIST DEFAULT-VALUE INHERIT-INPUT-METHOD)"
  (error 'unimplemented-error))
(defun* read-string ()
  #M"Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  This argument has been superseded by DEFAULT-VALUE and should normally be nil
  in new code.  It behaves as INITIAL-CONTENTS in ‘read-from-minibuffer' (which
  see).
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See ‘read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value or the list of default values.
 If non-nil, it is used for history commands, and as the value (or the first
 element of the list of default values) to return if the user enters the
 empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of ‘enable-multibyte-characters'.

(fn PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)"
  (error 'unimplemented-error))
(defun* read-variable ()
  #M"Read the name of a user option and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list of strings.
A user option, or customizable variable, is one for which
‘custom-variable-p' returns non-nil.

(fn PROMPT &optional DEFAULT-VALUE)"
  (error 'unimplemented-error))
(defun* set-minibuffer-window ()
  #M"Specify which minibuffer window to use for the minibuffer.
This affects where the minibuffer is displayed if you put text in it
without invoking the usual minibuffer commands.

(fn WINDOW)"
  (error 'unimplemented-error))
(defun* test-completion ()
  #M"Return non-nil if STRING is a valid completion.
For instance, if COLLECTION is a list of strings, STRING is a
valid completion if it appears in the list and PREDICATE is satisfied.

Takes the same arguments as ‘all-completions' and ‘try-completion'.

If COLLECTION is a function, it is called with three arguments:
the values STRING, PREDICATE and ‘lambda'.

(fn STRING COLLECTION &optional PREDICATE)"
  (error 'unimplemented-error))
(defun* try-completion ()
  #M"Return longest common substring of all completions of STRING in COLLECTION.

Test each possible completion specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
by using ‘symbol-name'.

If no possible completions match, the function returns nil; if
there's just one exact match, it returns t; otherwise it returns
the longest initial substring common to all possible completions
that begin with STRING.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is a possible completion.
If COLLECTION is a hash-table, all the keys that are either strings
or symbols are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: STRING, PREDICATE and nil.
Whatever it returns becomes the value of ‘try-completion'.

If optional third argument PREDICATE is non-nil, it must be a function
of one or two arguments, and is used to test each possible completion.
A possible completion is accepted only if PREDICATE returns non-nil.

The argument given to PREDICATE is either a string or a cons cell (whose
car is a string) from the alist, or a symbol from the obarray.
If COLLECTION is a hash-table, PREDICATE is called with two arguments:
the string key and the associated value.

To be acceptable, a possible completion must also match all the regexps
in ‘completion-regexp-list' (unless COLLECTION is a function, in
which case that function should itself handle ‘completion-regexp-list').

If ‘completion-ignore-case' is non-nil, possible completions are matched
while ignoring letter-case, but no guarantee is made about the letter-case
of the return value, except that it comes either from the user's input
or from one of the possible completions.

(fn STRING COLLECTION &optional PREDICATE)"
  (error 'unimplemented-error))
