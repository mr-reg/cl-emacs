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

(uiop:define-package :cl-emacs/search
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/search)
(log-enable :cl-emacs/search :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* looking-at ()
  #M"Return t if text after point matches regular expression REGEXP.
By default, this function modifies the match data that
‘match-beginning', ‘match-end' and ‘match-data' access.  If
INHIBIT-MODIFY is non-nil, don't modify the match data.

(fn REGEXP &optional INHIBIT-MODIFY)"
  (error 'unimplemented-error))
(defun* match-beginning ()
  #M"Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.

Return value is undefined if the last search failed.

(fn SUBEXP)"
  (error 'unimplemented-error))
(defun* match-data ()
  #M"Return a list of positions that record text matched by the last search.
Element 2N of the returned list is the position of the beginning of the
match of the Nth subexpression; it corresponds to ‘(match-beginning N)';
element 2N + 1 is the position of the end of the match of the Nth
subexpression; it corresponds to ‘(match-end N)'.  See ‘match-beginning'
and ‘match-end'.
If the last search was on a buffer, all the elements are by default
markers or nil (nil when the Nth pair didn't match); they are integers
or nil if the search was on a string.  But if the optional argument
INTEGERS is non-nil, the elements that represent buffer positions are
always integers, not markers, and (if the search was on a buffer) the
buffer itself is appended to the list as one additional element.

Use ‘set-match-data' to reinstate the match data from the elements of
this list.

Note that non-matching optional groups at the end of the regexp are
elided instead of being represented with two ‘nil's each.  For instance:

  (progn
    (string-match \"^\\(a\\)?\\(b\\)\\(c\\)?$\" \"b\")
    (match-data))
  => (0 1 nil nil 0 1)

If REUSE is a list, store the value in REUSE by destructively modifying it.
If REUSE is long enough to hold all the values, its length remains the
same, and any unused elements are set to nil.  If REUSE is not long
enough, it is extended.  Note that if REUSE is long enough and INTEGERS
is non-nil, no consing is done to make the return value; this minimizes GC.

If optional third argument RESEAT is non-nil, any previous markers on the
REUSE list will be modified to point to nowhere.

Return value is undefined if the last search failed.

(fn &optional INTEGERS REUSE RESEAT)"
  (error 'unimplemented-error))
(defun* match-data--translate ()
  #M"Add N to all positions in the match data.  Internal.

(fn N)"
  (error 'unimplemented-error))
(defun* match-end ()
  #M"Return position of end of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string.

Return value is undefined if the last search failed.

(fn SUBEXP)"
  (error 'unimplemented-error))
(defun* newline-cache-check ()
  #M"Check the newline cache of BUFFER against buffer contents.

BUFFER defaults to the current buffer.

Value is an array of 2 sub-arrays of buffer positions for newlines,
the first based on the cache, the second based on actually scanning
the buffer.  If the buffer doesn't have a cache, the value is nil.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* posix-looking-at ()
  #M"Return t if text after point matches REGEXP according to Posix rules.
Find the longest match, in accordance with Posix regular expression rules.

By default, this function modifies the match data that
‘match-beginning', ‘match-end' and ‘match-data' access.  If
INHIBIT-MODIFY is non-nil, don't modify the match data.

(fn REGEXP &optional INHIBIT-MODIFY)"
  (error 'unimplemented-error))
(defun* posix-search-backward ()
  #M"Search backward from point for match for REGEXP according to Posix rules.
Find the longest match in accord with Posix regular expression rules.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not begin before that position.  A value of nil
  means search to the beginning of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, position at limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  forward, instead of backward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth to last one (or
  last, if COUNT is 1 or nil) in the buffer located entirely before
  the origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end', ‘match-string',
and ‘replace-match'.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* posix-search-forward ()
  #M"Search forward from point for REGEXP according to Posix rules.
Find the longest match in accord with Posix regular expression rules.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not end after that position.  A value of nil
  means search to the end of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  backward, instead of forward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth one (or first,
  if COUNT is 1 or nil) in the buffer located entirely after the
  origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end', ‘match-string',
and ‘replace-match'.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* posix-string-match ()
  #M"Return index of start of first match for Posix REGEXP in STRING, or nil.
Find the longest match, in accord with Posix regular expression rules.
Case is ignored if ‘case-fold-search' is non-nil in the current buffer.

If INHIBIT-MODIFY is non-nil, match data is not changed.

If INHIBIT-MODIFY is nil or missing, match data is changed, and
‘match-end' and ‘match-beginning' give indices of substrings matched
by parenthesis constructs in the pattern.  You can use the function
‘match-string' to extract the substrings matched by the parenthesis
constructions in REGEXP.  For index of first char beyond the match, do
(match-end 0).

(fn REGEXP STRING &optional START INHIBIT-MODIFY)"
  (error 'unimplemented-error))
(defun* re-search-backward ()
  #M"Search backward from point for regular expression REGEXP.
This function is almost identical to ‘re-search-forward', except that
by default it searches backward instead of forward, and the sign of
COUNT also indicates exactly the opposite searching direction.
See ‘re-search-forward' for details.

Note that searching backwards may give a shorter match than expected,
because REGEXP is still matched in the forward direction.  See Info
anchor ‘(elisp) re-search-backward' for details.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* re-search-forward ()
  #M"Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional third argument NOERROR indicates how errors are handled
  when the search fails.  If it is nil or omitted, emit an error; if
  it is t, simply return nil and do nothing; if it is neither nil nor
  t, move to the limit of search and return nil.
The optional fourth argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for.  If it
  is positive, search forward for COUNT successive occurrences; if it
  is negative, search backward, instead of forward, for -COUNT
  occurrences.  A value of nil means the same as 1.
With COUNT positive/negative, the match found is the COUNTth/-COUNTth
  one in the buffer located entirely after/before the origin of the
  search.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end', ‘match-string',
and ‘replace-match'.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* regexp-quote ()
  #M"Return a regexp string which matches exactly STRING and nothing else.

(fn STRING)"
  (error 'unimplemented-error))
(defun* replace-match ()
  #M"Replace text matched by last search with NEWTEXT.
Leave point at the end of the replacement text.

If optional second arg FIXEDCASE is non-nil, do not alter the case of
the replacement text.  Otherwise, maybe capitalize the whole text, or
maybe just word initials, based on the replaced text.  If the replaced
text has only capital letters and has at least one multiletter word,
convert NEWTEXT to all caps.  Otherwise if all words are capitalized
in the replaced text, capitalize each word in NEWTEXT.

If optional third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat ‘\\' as special:
  ‘\\&' in NEWTEXT means substitute original matched text.
  ‘\\N' means substitute what matched the Nth ‘\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  ‘\\\\' means insert one ‘\\'.
  ‘\\?' is treated literally
       (for compatibility with ‘query-replace-regexp').
  Any other character following ‘\\' signals an error.
Case conversion does not apply to these substitutions.

If optional fourth argument STRING is non-nil, it should be a string
to act on; this should be the string on which the previous match was
done via ‘string-match'.  In this case, ‘replace-match' creates and
returns a new string, made by copying STRING and replacing the part of
STRING that was matched (the original STRING itself is not altered).

The optional fifth argument SUBEXP specifies a subexpression;
it says to replace just that subexpression with NEWTEXT,
rather than replacing the entire matched text.
This is, in a vague sense, the inverse of using ‘\\N' in NEWTEXT;
‘\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
NEWTEXT in place of subexp N.
This is useful only after a regular expression search or match,
since only regular expressions have distinguished subexpressions.

(fn NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)"
  (error 'unimplemented-error))
(defun* search-backward ()
  #M"Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not begin before that position.  A value of nil
  means search to the beginning of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, position at limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  forward, instead of backward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth to last one (or
  last, if COUNT is 1 or nil) in the buffer located entirely before
  the origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end' and ‘replace-match'.

(fn STRING &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* search-backward-regexp ()
  #M"Search backward from point for regular expression REGEXP.
This function is almost identical to ‘re-search-forward', except that
by default it searches backward instead of forward, and the sign of
COUNT also indicates exactly the opposite searching direction.
See ‘re-search-forward' for details.

Note that searching backwards may give a shorter match than expected,
because REGEXP is still matched in the forward direction.  See Info
anchor ‘(elisp) re-search-backward' for details.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* search-forward ()
  #M"Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
  The match found must not end after that position.  A value of nil
  means search to the end of the accessible portion of the buffer.
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument COUNT, if a positive number, means to search
  for COUNT successive occurrences.  If COUNT is negative, search
  backward, instead of forward, for -COUNT occurrences.  A value of
  nil means the same as 1.
With COUNT positive, the match found is the COUNTth one (or first,
  if COUNT is 1 or nil) in the buffer located entirely after the
  origin of the search; correspondingly with COUNT negative.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end' and ‘replace-match'.

(fn STRING &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* search-forward-regexp ()
  #M"Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional third argument NOERROR indicates how errors are handled
  when the search fails.  If it is nil or omitted, emit an error; if
  it is t, simply return nil and do nothing; if it is neither nil nor
  t, move to the limit of search and return nil.
The optional fourth argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for.  If it
  is positive, search forward for COUNT successive occurrences; if it
  is negative, search backward, instead of forward, for -COUNT
  occurrences.  A value of nil means the same as 1.
With COUNT positive/negative, the match found is the COUNTth/-COUNTth
  one in the buffer located entirely after/before the origin of the
  search.

Search case-sensitivity is determined by the value of the variable
‘case-fold-search', which see.

See also the functions ‘match-beginning', ‘match-end', ‘match-string',
and ‘replace-match'.

(fn REGEXP &optional BOUND NOERROR COUNT)"
  (error 'unimplemented-error))
(defun* set-match-data ()
  #M"Set internal data on last search match from elements of LIST.
LIST should have been created by calling ‘match-data' previously.

If optional arg RESEAT is non-nil, make markers on LIST point nowhere.

(fn LIST &optional RESEAT)"
  (error 'unimplemented-error))
(defun* string-match ()
  #M"Return index of start of first match for REGEXP in STRING, or nil.
Matching ignores case if ‘case-fold-search' is non-nil.
If third arg START is non-nil, start search at that index in STRING.

If INHIBIT-MODIFY is non-nil, match data is not changed.

If INHIBIT-MODIFY is nil or missing, match data is changed, and
‘match-end' and ‘match-beginning' give indices of substrings matched
by parenthesis constructs in the pattern.  You can use the function
‘match-string' to extract the substrings matched by the parenthesis
constructions in REGEXP.  For index of first char beyond the match, do
(match-end 0).

(fn REGEXP STRING &optional START INHIBIT-MODIFY)"
  (error 'unimplemented-error))
