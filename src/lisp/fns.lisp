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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/fns
    (:use
     :defstar
     :cl-emacs/log
     :cl-emacs/data
     :alexandria
     :fiveam
     :cl-custom-hash-table
     :cl-emacs/editfns
     :cl-emacs/alloc
     :cl-emacs/commons)
  (:import-from #:cl
                #:copy-alist
                #:clrhash
                #:gethash
                #:hash-table-count
                #:hash-table-p
                #:hash-table-rehash-size
                #:hash-table-rehash-threshold
                #:hash-table-size
                #:hash-table-test
                #:identity
                #:nreverse
                #:nth
                #:remhash
                #:sort
                )
  (:import-from #:cl-user
                #:hash-table-weak-p)
  (:import-from #:serapeum
                #:memq
                )
  (:export #:clrhash
           #:copy-alist
           #:copy-hash-table
           #:eql
           #:equal
           #:equal-including-properties
           #:gethash
           #:hash-table-count
           #:hash-table-p
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test
           #:identity
           #:memq
           #:make-hash-table
           #:nreverse
           #:nth
           #:puthash
           #:remhash
           #:sxhash-eq
           #:sxhash-eql
           #:sxhash-equal
           #:sxhash-equal-including-properties
           #:sort
           )

  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    ))
(in-package :cl-emacs/fns)
(log-enable :cl-emacs/fns :debug2)
(def-suite cl-emacs/fns)
(in-suite cl-emacs/fns)
(named-readtables:in-readtable pstrings:pstring-syntax)


;;; equality functions

(defun* sxhash-eq (obj)
  #M"Return an integer hash code for OBJ suitable for ‘eq'.
     If (eq A B), then (= (sxhash-eq A) (sxhash-eq B)).

     Hash codes are not guaranteed to be preserved across Emacs sessions.
     "
  (float-features:with-float-traps-masked (:invalid)
    (cl:sxhash obj)))
(test test-sxhash-eq
  (is (= (sxhash-eq 340) (sxhash-eq 340)))
  (is-false (= (sxhash-eq cl-emacs/data::*infinity-positive*)
               (sxhash-eq cl-emacs/data::*infinity-negative*)))
  (is (= (sxhash-eq cl-emacs/data::*nan*)
         (sxhash-eq cl-emacs/data::*nan*)))
  (is (= (sxhash-eq '#:symbol) (sxhash-eq '#:symbol)))
  (is-false (= (sxhash-eq 345) (sxhash-eq 346))))

(defun* eql (obj1 obj2)
  #M"Return t if the two args are ‘eq' or are indistinguishable numbers.
     Integers with the same value are ‘eql'.
     Floating-point values with the same sign, exponent and fraction are ‘eql'.
     This differs from numeric comparison: (eql 0.0 -0.0) returns nil and
     (eql 0.0e+NaN 0.0e+NaN) returns t, whereas ‘=' does the opposite.

     (fn OBJ1 OBJ2)"
  (eq obj1 obj2))

(defun* sxhash-eql (obj)
  #M"Return an integer hash code for OBJ suitable for ‘eql'.
     If (eql A B), then (= (sxhash-eql A) (sxhash-eql B)), but the opposite
     isn't necessarily true.

     Hash codes are not guaranteed to be preserved across Emacs sessions.
     "
  (* (sxhash-eq obj)
     ;; awkward emulation of emacs binary access to sign bit
     (if (and (floatp obj) (< (cl:float-sign obj) 0))
         -1 1))
  )

(test test-eql
  (is (eql 2 2))
  (is (= (sxhash-eql 2) (sxhash-eql 2)))

  (is (eql 'el::sym 'el::sym))
  (is (= (sxhash-eql 'el::sym) (sxhash-eql 'el::sym)))

  (is (eql 10e2 1000.0))
  (is (= (sxhash-eql 10e2) (sxhash-eql 1000.0)))

  (is-false (eql 10 10.0))
  (is-false (= (sxhash-eql 10) (sxhash-eql 10.0)))

  (is-false (cl:eql 0.0 -0.0))
  (is-false (= (sxhash-eql 0.0) (sxhash-eql -0.0)))

  (is (eql cl-emacs/data::*nan* (/ 0.0 0.0)))
  (is (= (sxhash-eql cl-emacs/data::*nan*)
         (sxhash-eql (/ 0.0 0.0))))
  )

(defun* equal (x y)
  #M"Return t if two Lisp objects have similar structure and contents.
     They must have the same data type.
     Conses are compared by comparing the cars and the cdrs.
     Vectors and strings are compared element by element.
     Numbers are compared via ‘eql', so integers do not equal floats.
     (Use ‘=' if you want integers and floats to be able to be equal.)
     Symbols must match exactly."
  (cond ((eql x y) t)
        ((and (consp x) (consp y))
         (and (equal (car x) (car y))
              (equal (cdr x) (cdr y))))
        ;; TODO: use emacs vector functions
        ((and (vectorp x) (vectorp y))
         (let ((length (cl:length x)))
           (when (eq length (cl:length y))
             (dotimes (i length t)
               (declare (fixnum i))
               (let ((x-el (cl:aref x i))
                     (y-el (cl:aref y i)))
                 (unless (or (eq x-el y-el) (equal x-el y-el))
                   (return nil)))))))
        ((and (pstrings:pstring-p x) (pstrings:pstring-p y))
         (pstrings:pstring-char= x y))
        (t nil)))
(defun* sxhash-equal (obj)
  #M"Return an integer hash code for OBJ suitable for ‘equal'.
     If (equal A B), then (= (sxhash-equal A) (sxhash-equal B)), but the
     opposite isn't necessarily true.

     Hash codes are not guaranteed to be preserved across Emacs sessions.

     "
  (if (pstrings:pstring-p obj)
      (pstrings:compute-hash obj nil)
      (sxhash-eql obj))
  )
(test test-equal
  (is (equal 'foo 'foo))
  (is (= (sxhash-equal 'foo) (sxhash-equal 'foo)))

  (is (equal 456 456))
  (is (= (sxhash-equal 456) (sxhash-equal 456)))

  (is (equal #P"asdf" #P"asdf"))
  (is (= (sxhash-equal #P"asdf")
         (sxhash-equal #P"asdf")))
  (is-false (eq #P"asdf" #P"asdf"))

  (is (equal '(1 (2 (3))) '(1 (2 (3)))))
  (is (= (sxhash-equal '(1 (2 (3))))
         (sxhash-equal '(1 (2 (3))))))
  (is-false (eq '(1 (2 (3))) '(1 (2 (3)))))

  (is (equal #((1 2) 3) #((1 2) 3)))
  (is (= (sxhash-equal #((1 2) 3))
         (sxhash-equal #((1 2) 3))))
  (is-false (eq #((1 2) 3) #((1 2) 3)))

  (is-false (equal "asdf" "ASDF"))
  (is-false (= (sxhash-equal "asdf")
               (sxhash-equal "ASDF")))

  (is (equal #P"asdf" (propertize #P"asdf" 'asdf t)))
  (is (equal (sxhash-equal #P"asdf")
             (sxhash-equal (propertize #P"asdf" 'asdf t))))

  ;; TODO: add tests for markers
  ;; (is (equal (point-marker) (point-marker)))
  ;; (is-false (eq (point-marker) (point-marker)))
  )

(defun* equal-including-properties (x y)
  #M"Return t if two Lisp objects have similar structure and contents.
     This is like ‘equal' except that it compares the text properties
     of strings.  (‘equal' ignores text properties.)
     "
  (cond
    ((and (pstrings:pstring-p x) (pstrings:pstring-p y))
     (pstrings:pstring= x y))
    (t (equal x y))))

(defun* sxhash-equal-including-properties (obj)
  #M"Return an integer hash code for OBJ suitable for
     ‘equal-including-properties'.
     If (sxhash-equal-including-properties A B), then
     (= (sxhash-equal-including-properties A) (sxhash-equal-including-properties B)).

     Hash codes are not guaranteed to be preserved across Emacs sessions.
     "
  (if (pstrings:pstring-p obj)
      (pstrings:compute-hash obj t)
      (sxhash-equal obj)
      )

  )

(test test-equal-including-properties
  (is-false (equal-including-properties
             #P"asdf"
             (propertize #P"asdf" 'asdf t)))
  (is-false (=
             (sxhash-equal-including-properties #P"asdf")
             (sxhash-equal-including-properties (propertize #P"asdf" 'asdf t))))
  (is (equal-including-properties
       (pstrings:build-pstring "asdf" '((asdf . t)))
       (propertize #P"asdf" 'asdf t)))
  (is (=
       (sxhash-equal-including-properties (pstrings:build-pstring "asdf" '((asdf . t))))
       (sxhash-equal-including-properties (propertize #P"asdf" 'asdf t)))))

;;; hashtables

(define-custom-hash-table-constructor make-hash-table-eq
  :test eq :hash-function sxhash-eq)
(define-custom-hash-table-constructor make-hash-table-eql
  :test eql :hash-function sxhash-eql)
(define-custom-hash-table-constructor make-hash-table-equal
  :test equal :hash-function sxhash-equal)

;; container to store user-defined hash-table-test-functions
(defparameter *hash-constructors*
  '((eq . make-hash-table-eq)
    (eql . make-hash-table-eql)
    (equal . make-hash-table-equal)
    ))

(define-condition hash-table-error (error-with-description)
  ())

(defun* make-hash-table (&key (test 'eql)
                              (size 65)
                              (rehash-size 1.5)
                              (rehash-threshold 0.8125)
                              (weakness))
  #M"Create and return a new hash table.

     Arguments are specified as keyword/argument pairs.  The following
     arguments are defined:

     :test TEST -- TEST must be a symbol that specifies how to compare
     keys.  Default is ‘eql'.  Predefined are the tests ‘eq', ‘eql', and
     ‘equal'.  User-supplied test and hash functions can be specified via
     ‘define-hash-table-test'.

     :size SIZE -- A hint as to how many elements will be put in the table.
     Default is 65.

     :rehash-size REHASH-SIZE - Indicates how to expand the table when it
     fills up.  If REHASH-SIZE is an integer, increase the size by that
     amount.  If it is a float, it must be > 1.0, and the new size is the
     old size multiplied by that factor.  Default is 1.5.

     :rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
     Resize the hash table when the ratio (table entries / table size)
     exceeds an approximation to THRESHOLD.  Default is 0.8125.

     :weakness WEAK -- WEAK must be one of nil, t, ‘key', ‘value',
     ‘key-or-value', or ‘key-and-value'.  If WEAK is not nil, the table
     returned is a weak table.  Key/value pairs are removed from a weak
     hash table when there are no non-weak references pointing to their
     key, value, one of key or value, or both key and value, depending on
     WEAK.  WEAK t is equivalent to ‘key-and-value'.  Default value of WEAK
     is nil.

     :purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied
     to pure storage when Emacs is being dumped, making the contents of the
     table read only. Any further changes to purified tables will result
     in an error.
     "
  (unless (position weakness '(nil t el::key el::value el::key-or-value el::key-and-value))
    (error 'hash-table-error :details
           (cl:format nil "weakness value incorrect ~s" weakness)))
  (let ((cl-weakness nil)
        (constructor-sym (assoc-value *hash-constructors* test)))
    (unless constructor-sym
      (error 'hash-table-error :details
             (cl:format nil "bad test function ~s" test)))
    ;; CCL limitation: we support weakness only when test = eq
    (when (and weakness (eq test 'el::eq))
      (cond
        ((null weakness) nil)
        ((or (eq weakness t) (eq weakness 'el::value)) t)
        ((eq weakness 'el::key) :key)
        ((eq weakness 'el::key-and-value) :both)
        ((eq weakness 'el::key-or-value) :one)
        (t (error "unknown weakness ~s" weakness))
        ))
    (apply constructor-sym (list :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold
                                 :weak cl-weakness))))

(defun* puthash (key value (table hash-table))
  #M"Associate KEY with VALUE in hash table TABLE.
     If KEY is already present in table, replace its current value with
     VALUE.  In any case, return VALUE."
  (setf (gethash key table) value))

(test test-hash-table
  (let ((table (make-hash-table :test 'eq)))
    (puthash 'key1 'value1 table)
    (is (eq 'value1 (gethash 'key1 table)))
    (puthash 'key2 'value2 table)
    (puthash 'key1 'value3 table)
    (is (eq 'value3 (gethash 'key1 table)))
    (is (eq 'value2 (gethash 'key2 table)))
    (is (= 2 (hash-table-count table)))
    (puthash #P"abc" 1 table)
    (puthash #P"abc" 1 table)
    (is (= 4 (hash-table-count table)))
    (is-false (gethash #P"abc" table))
    )
  (let ((table (make-hash-table :test 'equal)))
    (puthash #P"abc" 1 table)
    (puthash #P"abc" #P"mno" table)
    (is (= 1 (hash-table-count table)))
    (is (equal (gethash #P"abc" table) #P"mno"))
    )
  )

(defun* copy-hash-table ((table hash-table))
  #M"Return a copy of hash table TABLE."
  (alexandria:copy-hash-table table))

(defun* define-hash-table-test ()
  #M"Define a new hash table test with name NAME, a symbol.

In hash tables created with NAME specified as test, use TEST to
compare keys, and HASH for computing hash codes of keys.

TEST must be a function taking two arguments and returning non-nil if
both arguments are the same.  HASH must be a function taking one
argument and returning an object that is the hash code of the argument.
It should be the case that if (eq (funcall HASH x1) (funcall HASH x2))
returns nil, then (funcall TEST x1 x2) also returns nil.

(fn NAME TEST HASH)"
  (error 'unimplemented-error))

(defun* hash-table-weakness ((table hash-table))
  #M"Return the weakness of TABLE."
  (hash-table-weak-p table))

(defun* append ()
  #M"Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.

All arguments except the last argument are copied.  The last argument
is just used as the tail of the new list.

(fn &rest SEQUENCES)"
  (error 'unimplemented-error))
(defun* assoc ()
  #M"Return non-nil if KEY is equal to the car of an element of ALIST.
The value is actually the first element of ALIST whose car equals KEY.

Equality is defined by the function TESTFN, defaulting to ‘equal'.
TESTFN is called with 2 arguments: a car of an alist element and KEY.

(fn KEY ALIST &optional TESTFN)"
  (error 'unimplemented-error))
(defun* assq ()
  #M"Return non-nil if KEY is ‘eq' to the car of an element of ALIST.
The value is actually the first element of ALIST whose car is KEY.
Elements of ALIST that are not conses are ignored.

(fn KEY ALIST)"
  (error 'unimplemented-error))
(defun* base64-decode-region ()
  #M"Base64-decode the region between BEG and END.
Return the length of the decoded data.

Note that after calling this function, the data in the region will
represent bytes, not text.  If you want to end up with text, you have
to call ‘decode-coding-region' afterwards with an appropriate coding
system.

If the region can't be decoded, signal an error and don't modify the buffer.
Optional third argument BASE64URL determines whether to use the URL variant
of the base 64 encoding, as defined in RFC 4648.
If optional fourth argument IGNORE-INVALID is non-nil invalid characters
are ignored instead of signaling an error.

(fn BEG END &optional BASE64URL IGNORE-INVALID)"
  (error 'unimplemented-error))
(defun* base64-decode-string ()
  #M"Base64-decode STRING and return the result as a string.
Optional argument BASE64URL determines whether to use the URL variant of
the base 64 encoding, as defined in RFC 4648.
If optional third argument IGNORE-INVALID is non-nil invalid characters are
ignored instead of signaling an error.

(fn STRING &optional BASE64URL IGNORE-INVALID)"
  (error 'unimplemented-error))
(defun* base64-encode-region ()
  #M"Base64-encode the region between BEG and END.
The data in the region is assumed to represent bytes, not text.  If
you want to base64-encode text, the text has to be converted into data
first by using ‘encode-coding-region' with the appropriate coding
system first.

Return the length of the encoded data.

Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.

(fn BEG END &optional NO-LINE-BREAK)"
  (error 'unimplemented-error))
(defun* base64-encode-string ()
  #M"Base64-encode STRING and return the result.
Optional second argument NO-LINE-BREAK means do not break long lines
into shorter lines.

(fn STRING &optional NO-LINE-BREAK)"
  (error 'unimplemented-error))
(defun* base64url-encode-region ()
  #M"Base64url-encode the region between BEG and END.
Return the length of the encoded text.
Optional second argument NO-PAD means do not add padding char =.

This produces the URL variant of base 64 encoding defined in RFC 4648.

(fn BEG END &optional NO-PAD)"
  (error 'unimplemented-error))
(defun* base64url-encode-string ()
  #M"Base64url-encode STRING and return the result.
Optional second argument NO-PAD means do not add padding char =.

This produces the URL variant of base 64 encoding defined in RFC 4648.

(fn STRING &optional NO-PAD)"
  (error 'unimplemented-error))
(defun* buffer-hash ()
  #M"Return a hash of the contents of BUFFER-OR-NAME.
This hash is performed on the raw internal format of the buffer,
disregarding any coding systems.  If nil, use the current buffer.

This function is useful for comparing two buffers running in the same
Emacs, but is not guaranteed to return the same hash between different
Emacs versions.  It should be somewhat more efficient on larger
buffers than ‘secure-hash' is, and should not allocate more memory.

It should not be used for anything security-related.  See
‘secure-hash' for these applications.

(fn &optional BUFFER-OR-NAME)"
  (error 'unimplemented-error))
(defun* buffer-line-statistics ()
  #M"Return data about lines in BUFFER.
The data is returned as a list, and the first element is the number of
lines in the buffer, the second is the length of the longest line, and
the third is the mean line length.  The lengths returned are in bytes, not
characters.

(fn &optional BUFFER-OR-NAME)"
  (error 'unimplemented-error))
(defun* clear-string ()
  #M"Clear the contents of STRING.
This makes STRING unibyte and may change its length.

(fn STRING)"
  (error 'unimplemented-error))

(defun* compare-strings ()
  #M"Compare the contents of two strings, converting to multibyte if needed.
The arguments START1, END1, START2, and END2, if non-nil, are
positions specifying which parts of STR1 or STR2 to compare.  In
string STR1, compare the part between START1 (inclusive) and END1
(exclusive).  If START1 is nil, it defaults to 0, the beginning of
the string; if END1 is nil, it defaults to the length of the string.
Likewise, in string STR2, compare the part between START2 and END2.
Like in ‘substring', negative values are counted from the end.

The strings are compared by the numeric values of their characters.
For instance, STR1 is \"less than\" STR2 if its first differing
character has a smaller numeric value.  If IGNORE-CASE is non-nil,
characters are converted to upper-case before comparing them.  Unibyte
strings are converted to multibyte for comparison.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning.

(fn STR1 START1 END1 STR2 START2 END2 &optional IGNORE-CASE)"
  (error 'unimplemented-error))
(defun* concat ()
  #M"Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters (integers).

Values of the ‘composition' property of the result are not guaranteed
to be ‘eq'.

(fn &rest SEQUENCES)"
  (error 'unimplemented-error))

(defun* copy-sequence ()
  #M"Return a copy of a list, vector, string, char-table or record.
The elements of a list, vector or record are not copied; they are
shared with the original.
If the original sequence is empty, this function may return
the same empty object instead of its copy.

(fn ARG)"
  (error 'unimplemented-error))

(defun* delete ()
  #M"Delete members of SEQ which are ‘equal' to ELT, and return the result.
SEQ must be a sequence (i.e. a list, a vector, or a string).
The return value is a sequence of the same type.

If SEQ is a list, this behaves like ‘delq', except that it compares
with ‘equal' instead of ‘eq'.  In particular, it may remove elements
by altering the list structure.

If SEQ is not a list, deletion is never performed destructively;
instead this function creates and returns a new vector or string.

Write ‘(setq foo (delete element foo))' to be sure of correctly
changing the value of a sequence ‘foo'.  See also ‘remove', which
does not modify the argument.

(fn ELT SEQ)"
  (error 'unimplemented-error))
(defun* delq ()
  #M"Delete members of LIST which are ‘eq' to ELT, and return the result.
More precisely, this function skips any members ‘eq' to ELT at the
front of LIST, then removes members ‘eq' to ELT from the remaining
sublist by modifying its list structure, then returns the resulting
list.

Write ‘(setq foo (delq element foo))' to be sure of correctly changing
the value of a list ‘foo'.  See also ‘remq', which does not modify the
argument.

(fn ELT LIST)"
  (error 'unimplemented-error))
(defun* elt ()
  #M"Return element of SEQUENCE at index N.

(fn SEQUENCE N)"
  (error 'unimplemented-error))

(defun* featurep ()
  #M"Return t if FEATURE is present in this Emacs.

Use this to conditionalize execution of lisp code based on the
presence or absence of Emacs or environment extensions.
Use ‘provide' to declare that a feature is available.  This function
looks at the value of the variable ‘features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE.

(fn FEATURE &optional SUBFEATURE)"
  (error 'unimplemented-error))
(defun* fillarray ()
  #M"Store each element of ARRAY with ITEM.
ARRAY is a vector, string, char-table, or bool-vector.

(fn ARRAY ITEM)"
  (error 'unimplemented-error))
(defun* get ()
  #M"Return the value of SYMBOL's PROPNAME property.
This is the last value stored with ‘(put SYMBOL PROPNAME VALUE)'.

(fn SYMBOL PROPNAME)"
  (error 'unimplemented-error))


(defun* length ()
  #M"Return the length of vector, list or string SEQUENCE.
A byte-code function object is also allowed.

If the string contains multibyte characters, this is not necessarily
the number of bytes in the string; it is the number of characters.
To get the number of bytes, use ‘string-bytes'.

If the length of a list is being computed to compare to a (small)
number, the ‘length<', ‘length>' and ‘length=' functions may be more
efficient.

(fn SEQUENCE)"
  (error 'unimplemented-error))

(defun* length< ()
  #M"Return non-nil if SEQUENCE is shorter than LENGTH.
See ‘length' for allowed values of SEQUENCE and how elements are
counted.

(fn SEQUENCE LENGTH)"
  (error 'unimplemented-error))

(defun* length= ()
  #M"Return non-nil if SEQUENCE has length equal to LENGTH.
See ‘length' for allowed values of SEQUENCE and how elements are
counted.

(fn SEQUENCE LENGTH)"
  (error 'unimplemented-error))

(defun* length> ()
  #M"Return non-nil if SEQUENCE is longer than LENGTH.
See ‘length' for allowed values of SEQUENCE and how elements are
counted.

(fn SEQUENCE LENGTH)"
  (error 'unimplemented-error))

(defun* line-number-at-pos ()
  #M"Return the line number at POSITION in the current buffer.
If POSITION is nil or omitted, it defaults to point's position in the
current buffer.

If the buffer is narrowed, the return value by default counts the lines
from the beginning of the accessible portion of the buffer.  But if the
second optional argument ABSOLUTE is non-nil, the value counts the lines
from the absolute start of the buffer, disregarding the narrowing.

(fn &optional POSITION ABSOLUTE)"
  (error 'unimplemented-error))

(defun* load-average ()
  #M"Return list of 1 minute, 5 minute and 15 minute load averages.

Each of the three load averages is multiplied by 100, then converted
to integer.

When USE-FLOATS is non-nil, floats will be used instead of integers.
These floats are not multiplied by 100.

If the 5-minute or 15-minute load averages are not available, return a
shortened list, containing only those averages which are available.

An error is thrown if the load average can't be obtained.  In some
cases making it work would require Emacs being installed setuid or
setgid so that it can read kernel information, and that usually isn't
advisable.

(fn &optional USE-FLOATS)"
  (error 'unimplemented-error))
(defun* locale-info ()
  #M"Access locale data ITEM for the current C locale, if available.
ITEM should be one of the following:

‘codeset', returning the character set as a string (locale item CODESET);

‘days', returning a 7-element vector of day names (locale items DAY_n);

‘months', returning a 12-element vector of month names (locale items MON_n);

‘paper', returning a list of 2 integers (WIDTH HEIGHT) for the default
  paper size, both measured in millimeters (locale items _NL_PAPER_WIDTH,
  _NL_PAPER_HEIGHT).

If the system can't provide such information through a call to
‘nl_langinfo', or if ITEM isn't from the list above, return nil.

See also Info node ‘(libc)Locales'.

The data read from the system are decoded using ‘locale-coding-system'.

(fn ITEM)"
  (error 'unimplemented-error))

(defun* mapc ()
  #M"Apply FUNCTION to each element of SEQUENCE for side effects only.
Unlike ‘mapcar', don't accumulate the results.  Return SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.

(fn FUNCTION SEQUENCE)"
  (error 'unimplemented-error))
(defun* mapcan ()
  #M"Apply FUNCTION to each element of SEQUENCE, and concatenate
the results by altering them (using ‘nconc').
SEQUENCE may be a list, a vector, a bool-vector, or a string.

(fn FUNCTION SEQUENCE)"
  (error 'unimplemented-error))
(defun* mapcar ()
  #M"Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.

(fn FUNCTION SEQUENCE)"
  (error 'unimplemented-error))
(defun* mapconcat ()
  #M"Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
In between each pair of results, stick in SEPARATOR.  Thus, \" \" as
  SEPARATOR results in spaces between the values returned by FUNCTION.

SEQUENCE may be a list, a vector, a bool-vector, or a string.

Optional argument SEPARATOR must be a string, a vector, or a list of
characters; nil stands for the empty string.

FUNCTION must be a function of one argument, and must return a value
  that is a sequence of characters: either a string, or a vector or
  list of numbers that are valid character codepoints.

(fn FUNCTION SEQUENCE &optional SEPARATOR)"
  (error 'unimplemented-error))
(defun* maphash ()
  #M"Call FUNCTION for all entries in hash table TABLE.
FUNCTION is called with two arguments, KEY and VALUE.
‘maphash' always returns nil.

(fn FUNCTION TABLE)"
  (error 'unimplemented-error))
(defun* md5 ()
  #M"Return MD5 message digest of OBJECT, a buffer or string.

A message digest is the string representation of the cryptographic checksum
of a document, and the algorithm to calculate it is defined in RFC 1321.
The MD5 digest is 32-character long.

The two optional arguments START and END are character positions
specifying for which part of OBJECT the message digest should be
computed.  If nil or omitted, the digest is computed for the whole
OBJECT.

The MD5 message digest is computed from the result of encoding the
text in a coding system, not directly from the internal Emacs form of
the text.  The optional fourth argument CODING-SYSTEM specifies which
coding system to encode the text with.  It should be the same coding
system that you used or will use when actually writing the text into a
file.

If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
system would be chosen by default for writing this text into a file.

If OBJECT is a string, the most preferred coding system (see the
command ‘prefer-coding-system') is used.

If NOERROR is non-nil, silently assume the ‘raw-text' coding if the
guesswork fails.  Normally, an error is signaled in such case.

Note that MD5 is not collision resistant and should not be used for
anything security-related.  See ‘secure-hash' for alternatives.

(fn OBJECT &optional START END CODING-SYSTEM NOERROR)"
  (error 'unimplemented-error))
(defun* member ()
  #M"Return non-nil if ELT is an element of LIST.  Comparison done with ‘equal'.
The value is actually the tail of LIST whose car is ELT.

(fn ELT LIST)"
  (error 'unimplemented-error))

(defun* memql ()
  #M"Return non-nil if ELT is an element of LIST.  Comparison done with ‘eql'.
The value is actually the tail of LIST whose car is ELT.

(fn ELT LIST)"
  (error 'unimplemented-error))
(defun* nconc ()
  #M"Concatenate any number of lists by altering them.
Only the last argument is not altered, and need not be a list.

(fn &rest LISTS)"
  (error 'unimplemented-error))

(defun* ntake ()
  #M"Modify LIST to keep only the first N elements.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST unmodified.
Otherwise, return LIST after truncating it.

(fn N LIST)"
  (error 'unimplemented-error))

(defun* nthcdr ()
  #M"Take cdr N times on LIST, return the result.

(fn N LIST)"
  (error 'unimplemented-error))
(defun* object-intervals ()
  #M"Return a copy of the text properties of OBJECT.
OBJECT must be a buffer or a string.

Altering this copy does not change the layout of the text properties
in OBJECT.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* plist-get ()
  #M"Extract a value from a property list.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2...).

This function returns the value corresponding to the given PROP, or
nil if PROP is not one of the properties on the list.  The comparison
with PROP is done using PREDICATE, which defaults to ‘eq'.

This function doesn't signal an error if PLIST is invalid.

(fn PLIST PROP &optional PREDICATE)"
  (error 'unimplemented-error))
(defun* plist-member ()
  #M"Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2 ...).

The comparison with PROP is done using PREDICATE, which defaults to
‘eq'.

Unlike ‘plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP.

(fn PLIST PROP &optional PREDICATE)"
  (error 'unimplemented-error))
(defun* plist-put ()
  #M"Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2 ...).

The comparison with PROP is done using PREDICATE, which defaults to ‘eq'.

If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use ‘(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.

(fn PLIST PROP VAL &optional PREDICATE)"
  (error 'unimplemented-error))
(defun* proper-list-p ()
  #M"Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr is nil).

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* provide ()
  #M"Announce that FEATURE is a feature of the current Emacs.
The optional argument SUBFEATURES should be a list of symbols listing
particular subfeatures supported in this version of FEATURE.

(fn FEATURE &optional SUBFEATURES)"
  (error 'unimplemented-error))
(defun* put ()
  #M"Store SYMBOL's PROPNAME property with value VALUE.
It can be retrieved with ‘(get SYMBOL PROPNAME)'.

(fn SYMBOL PROPNAME VALUE)"
  (error 'unimplemented-error))

(defun* random ()
  #M"Return a pseudo-random integer.
By default, return a fixnum; all fixnums are equally likely.
With positive integer LIMIT, return random integer in interval [0,LIMIT).
With argument t, set the random number seed from the system's entropy
pool if available, otherwise from less-random volatile data such as the time.
With a string argument, set the seed based on the string's contents.

See Info node ‘(elisp)Random Numbers' for more details.

(fn &optional LIMIT)"
  (error 'unimplemented-error))
(defun* rassoc ()
  #M"Return non-nil if KEY is ‘equal' to the cdr of an element of ALIST.
The value is actually the first element of ALIST whose cdr equals KEY.

(fn KEY ALIST)"
  (error 'unimplemented-error))
(defun* rassq ()
  #M"Return non-nil if KEY is ‘eq' to the cdr of an element of ALIST.
The value is actually the first element of ALIST whose cdr is KEY.

(fn KEY ALIST)"
  (error 'unimplemented-error))

(defun* require ()
  #M"If FEATURE is not already loaded, load it from FILENAME.
If FEATURE is not a member of the list ‘features', then the feature was
not yet loaded; so load it from file FILENAME.

If FILENAME is omitted, the printname of FEATURE is used as the file
name, and ‘load' is called to try to load the file by that name, after
appending the suffix ‘.elc', ‘.el', or the system-dependent suffix for
dynamic module files, in that order; but the function will not try to
load the file without any suffix.  See ‘get-load-suffixes' for the
complete list of suffixes.

To find the file, this function searches the directories in ‘load-path'.

If the optional third argument NOERROR is non-nil, then, if
the file is not found, the function returns nil instead of signaling
an error.  Normally the return value is FEATURE.

The normal messages issued by ‘load' at start and end of loading
FILENAME are suppressed.

(fn FEATURE &optional FILENAME NOERROR)"
  (error 'unimplemented-error))
(defun* reverse ()
  #M"Return the reversed copy of list, vector, or string SEQ.
See also the function ‘nreverse', which is used more often.

(fn SEQ)"
  (error 'unimplemented-error))
(defun* safe-length ()
  #M"Return the length of a list, but avoid error or infinite loop.
This function never gets an error.  If LIST is not really a list,
it returns 0.  If LIST is circular, it returns an integer that is at
least the number of distinct elements.

(fn LIST)"
  (error 'unimplemented-error))
(defun* secure-hash ()
  #M"Return the secure hash of OBJECT, a buffer or string.
ALGORITHM is a symbol specifying the hash to use:
- md5    corresponds to MD5, produces a 32-character signature
- sha1   corresponds to SHA-1, produces a 40-character signature
- sha224 corresponds to SHA-2 (SHA-224), produces a 56-character signature
- sha256 corresponds to SHA-2 (SHA-256), produces a 64-character signature
- sha384 corresponds to SHA-2 (SHA-384), produces a 96-character signature
- sha512 corresponds to SHA-2 (SHA-512), produces a 128-character signature

The two optional arguments START and END are positions specifying for
which part of OBJECT to compute the hash.  If nil or omitted, uses the
whole OBJECT.

The full list of algorithms can be obtained with ‘secure-hash-algorithms'.

If BINARY is non-nil, returns a string in binary form.

Note that MD5 and SHA-1 are not collision resistant and should not be
used for anything security-related.  For these applications, use one
of the other hash types instead, e.g. sha256 or sha512.

(fn ALGORITHM OBJECT &optional START END BINARY)"
  (error 'unimplemented-error))
(defun* secure-hash-algorithms ()
  #M"Return a list of all the supported ‘secure-hash' algorithms.

(fn)"
  (error 'unimplemented-error))

(defun* string-as-multibyte ()
  #M"Return a multibyte string with the same individual bytes as STRING.
If STRING is multibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.

If STRING is unibyte and contains an individual 8-bit byte (i.e. not
part of a correct utf-8 sequence), it is converted to the corresponding
multibyte character of charset ‘eight-bit'.
See also ‘string-to-multibyte'.

Beware, this often doesn't really do what you think it does.
It is similar to (decode-coding-string STRING \\='utf-8-emacs).
If you're not sure, whether to use ‘string-as-multibyte' or
‘string-to-multibyte', use ‘string-to-multibyte'.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-as-unibyte ()
  #M"Return a unibyte string with the same individual bytes as STRING.
If STRING is unibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.
If STRING is multibyte and contains a character of charset
‘eight-bit', it is converted to the corresponding single byte.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-bytes ()
  #M"Return the number of bytes in STRING.
If STRING is multibyte, this may be greater than the length of STRING.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-collate-equalp ()
  #M"Return t if two strings have identical contents.
Symbols are also allowed; their print names are used instead.

This function obeys the conventions for collation order in your locale
settings.  For example, characters with different coding points but
the same meaning might be considered as equal, like different grave
accent Unicode characters:

(string-collate-equalp (string ?\\uFF40) (string ?\\u1FEF))
  => t

The optional argument LOCALE, a string, overrides the setting of your
current locale identifier for collation.  The value is system
dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
while it would be \"enu_USA.1252\" on MS Windows systems.

If IGNORE-CASE is non-nil, characters are converted to lower-case
before comparing them.

To emulate Unicode-compliant collation on MS-Windows systems,
bind ‘w32-collate-ignore-punctuation' to a non-nil value, since
the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.

If your system does not support a locale environment, this function
behaves like ‘string-equal', and in that case the IGNORE-CASE argument
is ignored.

Do NOT use this function to compare file names for equality.

(fn S1 S2 &optional LOCALE IGNORE-CASE)"
  (error 'unimplemented-error))
(defun* string-collate-lessp ()
  #M"Return t if first arg string is less than second in collation order.
Symbols are also allowed; their print names are used instead.

This function obeys the conventions for collation order in your
locale settings.  For example, punctuation and whitespace characters
might be considered less significant for sorting:

(sort \\='(\"11\" \"12\" \"1 1\" \"1 2\" \"1.1\" \"1.2\") \\='string-collate-lessp)
  => (\"11\" \"1 1\" \"1.1\" \"12\" \"1 2\" \"1.2\")

The optional argument LOCALE, a string, overrides the setting of your
current locale identifier for collation.  The value is system
dependent; a LOCALE \"en_US.UTF-8\" is applicable on POSIX systems,
while it would be, e.g., \"enu_USA.1252\" on MS-Windows systems.

If IGNORE-CASE is non-nil, characters are converted to lower-case
before comparing them.

To emulate Unicode-compliant collation on MS-Windows systems,
bind ‘w32-collate-ignore-punctuation' to a non-nil value, since
the codeset part of the locale cannot be \"UTF-8\" on MS-Windows.

Some operating systems do not implement correct collation (in specific
locale environments or at all).  Then, this functions falls back to
case-sensitive ‘string-lessp' and IGNORE-CASE argument is ignored.

(fn S1 S2 &optional LOCALE IGNORE-CASE)"
  (error 'unimplemented-error))
(defun* string-distance ()
  #M"Return Levenshtein distance between STRING1 and STRING2.
The distance is the number of deletions, insertions, and substitutions
required to transform STRING1 into STRING2.
If BYTECOMPARE is nil or omitted, compute distance in terms of characters.
If BYTECOMPARE is non-nil, compute distance in terms of bytes.
Letter-case is significant, but text properties are ignored.

(fn STRING1 STRING2 &optional BYTECOMPARE)"
  (error 'unimplemented-error))
(defun* string-equal ()
  #M"Return t if two strings have identical contents.
Case is significant, but text properties are ignored.
Symbols are also allowed; their print names are used instead.

See also ‘string-equal-ignore-case'.

(fn S1 S2)"
  (error 'unimplemented-error))
(defun* string-lessp ()
  #M"Return non-nil if STRING1 is less than STRING2 in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead.

(fn STRING1 STRING2)"
  (error 'unimplemented-error))
(defun* string-make-multibyte ()
  #M"Return the multibyte equivalent of STRING.
If STRING is unibyte and contains non-ASCII characters, the function
‘unibyte-char-to-multibyte' is used to convert each unibyte character
to a multibyte character.  In this case, the returned string is a
newly created string with no text properties.  If STRING is multibyte
or entirely ASCII, it is returned unchanged.  In particular, when
STRING is unibyte and entirely ASCII, the returned string is unibyte.
(When the characters are all ASCII, Emacs primitives will treat the
string the same way whether it is unibyte or multibyte.)

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-make-unibyte ()
  #M"Return the unibyte equivalent of STRING.
Multibyte character codes above 255 are converted to unibyte
by taking just the low 8 bits of each character's code.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-search ()
  #M"Search for the string NEEDLE in the string HAYSTACK.
The return value is the position of the first occurrence of NEEDLE in
HAYSTACK, or nil if no match was found.

The optional START-POS argument says where to start searching in
HAYSTACK and defaults to zero (start at the beginning).
It must be between zero and the length of HAYSTACK, inclusive.

Case is always significant and text properties are ignored.

(fn NEEDLE HAYSTACK &optional START-POS)"
  (error 'unimplemented-error))
(defun* string-to-multibyte ()
  #M"Return a multibyte string with the same individual chars as STRING.
If STRING is multibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties.

If STRING is unibyte and contains an 8-bit byte, it is converted to
the corresponding multibyte character of charset ‘eight-bit'.

This differs from ‘string-as-multibyte' by converting each byte of a correct
utf-8 sequence to an eight-bit character, not just bytes that don't form a
correct sequence.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-to-unibyte ()
  #M"Return a unibyte string with the same individual chars as STRING.
If STRING is unibyte, the result is STRING itself.
Otherwise it is a newly created string, with no text properties,
where each ‘eight-bit' character is converted to the corresponding byte.
If STRING contains a non-ASCII, non-‘eight-bit' character,
an error is signaled.

(fn STRING)"
  (error 'unimplemented-error))
(defun* string-version-lessp ()
  #M"Return non-nil if S1 is less than S2, as version strings.

This function compares version strings S1 and S2:
   1) By prefix lexicographically.
   2) Then by version (similarly to version comparison of Debian's dpkg).
      Leading zeros in version numbers are ignored.
   3) If both prefix and version are equal, compare as ordinary strings.

For example, \"foo2.png\" compares less than \"foo12.png\".
Case is significant.
Symbols are also allowed; their print names are used instead.

(fn STRING1 STRING2)"
  (error 'unimplemented-error))
(defun* substring ()
  #M"Return a new string whose contents are a substring of STRING.
The returned string consists of the characters between index FROM
(inclusive) and index TO (exclusive) of STRING.  FROM and TO are
zero-indexed: 0 means the first character of STRING.  Negative values
are counted from the end of STRING.  If TO is nil, the substring runs
to the end of STRING.

The STRING argument may also be a vector.  In that case, the return
value is a new vector that contains the elements between index FROM
(inclusive) and index TO (exclusive) of that vector argument.

With one argument, just copy STRING (with properties, if any).

(fn STRING &optional FROM TO)"
  (error 'unimplemented-error))
(defun* substring-no-properties ()
  #M"Return a substring of STRING, without text properties.
It starts at index FROM and ends before TO.
TO may be nil or omitted; then the substring runs to the end of STRING.
If FROM is nil or omitted, the substring starts at the beginning of STRING.
If FROM or TO is negative, it counts from the end.

With one argument, just copy STRING without its properties.

(fn STRING &optional FROM TO)"
  (error 'unimplemented-error))

(defun* take ()
  #M"Return the first N elements of LIST.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST (or a copy).

(fn N LIST)"
  (error 'unimplemented-error))
(defun* vconcat ()
  #M"Concatenate all the arguments and make the result a vector.
The result is a vector whose elements are the elements of all the arguments.
Each argument may be a list, vector or string.

(fn &rest SEQUENCES)"
  (error 'unimplemented-error))
(defun* widget-apply ()
  #M"Apply the value of WIDGET's PROPERTY to the widget itself.
Return the result of applying the value of PROPERTY to WIDGET.
ARGS are passed as extra arguments to the function.

(fn WIDGET PROPERTY &rest ARGS)"
  (error 'unimplemented-error))
(defun* widget-get ()
  #M"In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with ‘widget-put'.

(fn WIDGET PROPERTY)"
  (error 'unimplemented-error))
(defun* widget-put ()
  #M"In WIDGET, set PROPERTY to VALUE.
The value can later be retrieved with ‘widget-get'.

(fn WIDGET PROPERTY VALUE)"
  (error 'unimplemented-error))
(defun* yes-or-no-p ()
  #M"Ask user a yes-or-no question.
Return t if answer is yes, and nil if the answer is no.

PROMPT is the string to display to ask the question; ‘yes-or-no-p'
adds \"(yes or no) \" to it.

The user must confirm the answer with RET, and can edit it until it
has been confirmed.

If the ‘use-short-answers' variable is non-nil, instead of asking for
\"yes\" or \"no\", this function will ask for \"y\" or \"n\".

If dialog boxes are supported, this function will use a dialog box
if ‘use-dialog-box' is non-nil and the last input event was produced
by a mouse, or by some window-system gesture, or via a menu.

(fn PROMPT)"
  (error 'unimplemented-error))
(defun test-me ()
  (run! 'cl-emacs/fns))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/fns)
