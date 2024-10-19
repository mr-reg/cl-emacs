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

(uiop:define-package :cl-emacs/syntax
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/syntax)
(log-enable :cl-emacs/syntax :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* backward-prefix-chars ()
  #M"Move point backward over any number of chars with prefix syntax.
This includes chars with expression prefix syntax class (\\=') and those with
the prefix syntax flag (p).

(fn)"
  (error 'unimplemented-error))
(defun* char-syntax ()
  #M"Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent, the
character ‘w' (119) is returned.
The characters that correspond to various syntax codes
are listed in the documentation of ‘modify-syntax-entry'.

If you're trying to determine the syntax of characters in the buffer,
this is probably the wrong function to use, because it can't take
‘syntax-table' text properties into account.  Consider using
‘syntax-after' instead.

(fn CHARACTER)"
  (error 'unimplemented-error))
(defun* copy-syntax-table ()
  #M"Construct a new syntax table and return it.
It is a copy of the TABLE, which defaults to the standard syntax table.

(fn &optional TABLE)"
  (error 'unimplemented-error))
(defun* forward-comment ()
  #M"Move forward across up to COUNT comments.  If COUNT is negative, move backward.
Stop scanning if we find something other than a comment or whitespace.
Set point to where scanning stops.
If COUNT comments are found as expected, with nothing except whitespace
between them, return t; otherwise return nil.

(fn COUNT)"
  (error 'unimplemented-error))
(defun* forward-word ()
  #M"Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word.
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is
left there and the function returns nil.  Field boundaries are not
noticed if ‘inhibit-field-text-motion' is non-nil.

The word boundaries are normally determined by the buffer's syntax
table and character script (according to ‘char-script-table'), but
‘find-word-boundary-function-table', such as set up by ‘subword-mode',
can change that.  If a Lisp program needs to move by words determined
strictly by the syntax table, it should use ‘forward-word-strictly'
instead.  See Info node ‘(elisp) Word Motion' for details.

(fn &optional ARG)"
  (error 'unimplemented-error))
(defun* internal-describe-syntax-value ()
  #M"Insert a description of the internal syntax description SYNTAX at point.

(fn SYNTAX)"
  (error 'unimplemented-error))
(defun* matching-paren ()
  #M"Return the matching parenthesis of CHARACTER, or nil if none.

(fn CHARACTER)"
  (error 'unimplemented-error))
(defun* modify-syntax-entry ()
  #M"Set syntax for character CHAR according to string NEWENTRY.
The syntax is changed only for table SYNTAX-TABLE, which defaults to
 the current buffer's syntax table.
CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
in the range MIN to MAX are changed.
The first character of NEWENTRY should be one of the following:
  Space or -  whitespace syntax.    w   word constituent.
  _           symbol constituent.   .   punctuation.
  (           open-parenthesis.     )   close-parenthesis.
  \"           string quote.         \\   escape.
  $           paired delimiter.     \\='   expression quote or prefix operator.
  <           comment starter.      >   comment ender.
  /           character-quote.      @   inherit from parent table.
  |           generic string fence. !   generic comment fence.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of NEWENTRY is the matching parenthesis,
 used only if the first character is ‘(' or ‘)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, b, p, and n.
 1 means CHAR is the start of a two-char comment start sequence.
 2 means CHAR is the second character of such a sequence.
 3 means CHAR is the start of a two-char comment end sequence.
 4 means CHAR is the second character of such a sequence.

There can be several orthogonal comment sequences.  This is to support
language modes such as C++.  By default, all comment sequences are of style
a, but you can set the comment sequence style to b (on the second character
of a comment-start, and the first character of a comment-end sequence) and/or
c (on any of its chars) using this flag:
 b means CHAR is part of comment sequence b.
 c means CHAR is part of comment sequence c.
 n means CHAR is part of a nestable comment sequence.

 p means CHAR is a prefix character for ‘backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.

(fn CHAR NEWENTRY &optional SYNTAX-TABLE)"
  (error 'unimplemented-error))
(defun* parse-partial-sexp ()
  #M"Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.

If OLDSTATE is omitted or nil, parsing assumes that FROM is the
 beginning of a function.  If not, OLDSTATE should be the state at
 FROM.

Value is a list of elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)
 4. nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting).
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. style of comment, if any.
 8. character address of start of comment or string; nil if not in one.
 9. List of positions of currently open parens, outermost first.
10. When the last position scanned holds the first character of a
    (potential) two character construct, the syntax of that position,
    otherwise nil.  That construct can be a two character comment
    delimiter or an Escaped or Char-quoted character.
11..... Possible further internal information used by ‘parse-partial-sexp'.

If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when we come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a list like what this function returns.
 It is used to initialize the state of the parse.  Elements number 1, 2, 6
 are ignored.
Sixth arg COMMENTSTOP non-nil means stop after the start of a comment.
 If it is the symbol ‘syntax-table', stop after the start of a comment or a
 string, or after end of a comment or a string.

(fn FROM TO &optional TARGETDEPTH STOPBEFORE OLDSTATE COMMENTSTOP)"
  (error 'unimplemented-error))
(defun* scan-lists ()
  #M"Scan from character number FROM by COUNT lists.
Scan forward if COUNT is positive, backward if COUNT is negative.
Return the character number of the position thus found.

A \"list\", in this context, refers to a balanced parenthetical
grouping, as determined by the syntax table.

If DEPTH is nonzero, treat that as the nesting depth of the starting
point (i.e. the starting point is DEPTH parentheses deep).  This
function scans over parentheses until the depth goes to zero COUNT
times.  Hence, positive DEPTH moves out that number of levels of
parentheses, while negative DEPTH moves to a deeper level.

Comments are ignored if ‘parse-sexp-ignore-comments' is non-nil.

If we reach the beginning or end of the accessible part of the buffer
before we have scanned over COUNT lists, return nil if the depth at
that point is zero, and signal an error if the depth is nonzero.

(fn FROM COUNT DEPTH)"
  (error 'unimplemented-error))
(defun* scan-sexps ()
  #M"Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if ‘parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned.

(fn FROM COUNT)"
  (error 'unimplemented-error))
(defun* set-syntax-table ()
  #M"Select a new syntax table for the current buffer.
One argument, a syntax table.

(fn TABLE)"
  (error 'unimplemented-error))
(defun* skip-chars-backward ()
  #M"Move point backward, stopping after a char not in STRING, or at pos LIM.
See ‘skip-chars-forward' for details.
Returns the distance traveled, either zero or negative.

(fn STRING &optional LIM)"
  (error 'unimplemented-error))
(defun* skip-chars-forward ()
  #M"Move point forward, stopping before a char not in STRING, or at pos LIM.
STRING is like the inside of a ‘[...]' in a regular expression
except that ‘]' is never special and ‘\\' quotes ‘^', ‘-' or ‘\\'
 (but not at the end of a range; quoting is never needed there).
Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.
With arg \"^a-zA-Z\", skips nonletters stopping before first letter.
Char classes, e.g. ‘[:alpha:]', are supported.

Returns the distance traveled, either zero or positive.

(fn STRING &optional LIM)"
  (error 'unimplemented-error))
(defun* skip-syntax-backward ()
  #M"Move point backward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns either zero or a negative number, and the absolute value
of this is the distance traveled.

(fn SYNTAX &optional LIM)"
  (error 'unimplemented-error))
(defun* skip-syntax-forward ()
  #M"Move point forward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop before a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or positive.

(fn SYNTAX &optional LIM)"
  (error 'unimplemented-error))
(defun* standard-syntax-table ()
  #M"Return the standard syntax table.
This is the one used for new buffers.

(fn)"
  (error 'unimplemented-error))
(defun* string-to-syntax ()
  #M"Convert a syntax descriptor STRING into a raw syntax descriptor.
STRING should be a string of the form allowed as argument of
‘modify-syntax-entry'.  The return value is a raw syntax descriptor: a
cons cell (CODE . MATCHING-CHAR) which can be used, for example, as
the value of a ‘syntax-table' text property.

(fn STRING)"
  (error 'unimplemented-error))
(defun* syntax-class-to-char ()
  #M"Return the syntax char of CLASS, described by an integer.
For example, if SYNTAX is word constituent (the integer 2), the
character ‘w' (119) is returned.

(fn SYNTAX)"
  (error 'unimplemented-error))
(defun* syntax-table ()
  #M"Return the current syntax table.
This is the one specified by the current buffer.

(fn)"
  (error 'unimplemented-error))
(defun* syntax-table-p ()
  #M"Return t if OBJECT is a syntax table.
Currently, any char-table counts as a syntax table.

(fn OBJECT)"
  (error 'unimplemented-error))
