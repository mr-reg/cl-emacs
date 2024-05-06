#|
Copyright (C) 2024 by Gleb Borodulia
Author: Gleb Borodulia <mr.reg@mail.ru>

This file is part of cl-emacs.

cl-emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

cl-emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.
|#


(uiop:define-package :cl-emacs/elisp/fns
    (:use :common-lisp :alexandria :cl-emacs/log
     :cl-emacs/elisp/internals)
  (:import-from :common-lisp-user
                #:memq
                #:hash-table-weak-p)
  )
(in-package :cl-emacs/elisp/fns)


;; (defun-elisp elisp/memq () (arg/elt arg/list)
;;   "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
;; The value is actually the tail of LIST whose car is ELT."
;;   ;; memq is macros in common lisp, but in elisp we need function
;;   (memq arg/elt arg/list))

;; (defun-elisp elisp/sxhash-eq '() (arg/obj)
;;   "Return an integer hash code for OBJ suitable for `eq'.
;; If (eq A B), then (= (sxhash-eq A) (sxhash-eq B)).

;; Hash codes are not guaranteed to be preserved across Emacs sessions. "
;;   (sxhash arg/obj))

;; (defun-elisp elisp/sxhash-eql '() (arg/obj)
;;   "Return an integer hash code for OBJ suitable for `eql'.
;; If (eql A B), then (= (sxhash-eql A) (sxhash-eql B)), but the opposite
;; isn't necessarily true.

;; Hash codes are not guaranteed to be preserved across Emacs sessions."
;;   (sxhash arg/obj))

;; (defun-elisp elisp/sxhash-equal '() (arg/obj)
;;   "Return an integer hash code for OBJ suitable for `equal'.
;; If (equal A B), then (= (sxhash-equal A) (sxhash-equal B)), but the
;; opposite isn't necessarily true.

;; Hash codes are not guaranteed to be preserved across Emacs sessions. "
;;   (sxhash arg/obj))

;; (defun-elisp elisp/sxhash-equal-including-properties '() (arg/obj)
;;   "Return an integer hash code for OBJ suitable for
;; `equal-including-properties'.
;; If (sxhash-equal-including-properties A B), then
;; (= (sxhash-equal-including-properties A) (sxhash-equal-including-properties B)).

;; Hash codes are not guaranteed to be preserved across Emacs sessions. "
;;   (sxhash arg/obj))

;; (defun-elisp elisp/sxhash-case-insensitive '(:internal :rpc-debug) (arg/obj)
;;   (sxhash (str:downcase arg/obj)))

;; (defun-elisp elisp/make-hash-table '(:rpc-debug) (&key (test 'eql)
;;                                                        (size 65)
;;                                                        (rehash-size 1.5)
;;                                                        (rehash-threshold 0.8125)
;;                                                        (weakness))
;;   "Create and return a new hash table.

;; Arguments are specified as keyword/argument pairs.  The following
;; arguments are defined:

;; :test TEST -- TEST must be a symbol that specifies how to compare
;; keys.  Default is `eql'.  Predefined are the tests `eq', `eql', and
;; `equal'.  User-supplied test and hash functions can be specified via
;; `define-hash-table-test'.

;; :size SIZE -- A hint as to how many elements will be put in the table.
;; Default is 65.

;; :rehash-size REHASH-SIZE - Indicates how to expand the table when it
;; fills up.  If REHASH-SIZE is an integer, increase the size by that
;; amount.  If it is a float, it must be > 1.0, and the new size is the
;; old size multiplied by that factor.  Default is 1.5.

;; :rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
;; Resize the hash table when the ratio (table entries / table size)
;; exceeds an approximation to THRESHOLD.  Default is 0.8125.

;; :weakness WEAK -- WEAK must be one of nil, t, `key', `value',
;; `key-or-value', or `key-and-value'.  If WEAK is not nil, the table
;; returned is a weak table.  Key/value pairs are removed from a weak
;; hash table when there are no non-weak references pointing to their
;; key, value, one of key or value, or both key and value, depending on
;; WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
;; is nil.

;; :purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied
;; to pure storage when Emacs is being dumped, making the contents of the
;; table read only. Any further changes to purified tables will result
;; in an error.

;; usage: (make-hash-table &rest KEYWORD-ARGS) "
;;   (assert (position test '(cl-emacs/elisp/symbols::eq cl-emacs/elisp/symbols::eql cl-emacs/elisp/symbols::equal cl-emacs/elisp/symbols::equalp)))
;;   (assert (position weakness '(nil t cl-emacs/elisp/symbols::key cl-emacs/elisp/symbols::value cl-emacs/elisp/symbols::key-or-value cl-emacs/elisp/symbols::key-and-value)))
;;   (let ((cl-weakness nil))
;;     (when (and weakness (eq test 'cl-emacs/elisp/symbols::eq)) 
;;       (cond
;;         ((null weakness) nil)
;;         ((or (eq weakness t) (eq weakness 'cl-emacs/elisp/symbols::value)) t)
;;         ((eq weakness 'cl-emacs/elisp/symbols::key) :key)
;;         ((eq weakness 'cl-emacs/elisp/symbols::key-and-value) :both)
;;         ((eq weakness 'cl-emacs/elisp/symbols::key-or-value) :one)
;;         (t (error "unknown weakness ~s" weakness))
;;         ))
;;     (make-hash-table :test (symbol-function test) :size size :rehash-size rehash-size
;;                      :rehash-threshold rehash-threshold :weak cl-weakness))

;;   )


;; (defun-elisp elisp/copy-hash-table '(:rpc-debug) (arg/hashtable)
;;   "Return a copy of hash table TABLE."
;;   (copy-hash-table arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-count '(:rpc-debug) (arg/hashtable)
;;   "Return the number of elements in TABLE."
;;   (hash-table-count arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-rehash-size '(:rpc-debug) (arg/hashtable)
;;   "Return the current rehash size of TABLE."
;;   (hash-table-rehash-size arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-rehash-threshold '(:rpc-debug) (arg/hashtable)
;;   "Return the current rehash threshold of TABLE."
;;   (hash-table-rehash-threshold arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-size '(:rpc-debug) (arg/hashtable)
;;   "Return the size of TABLE.
;; The size can be used as an argument to `make-hash-table' to create
;; a hash table than can hold as many elements as TABLE holds
;; without need for resizing."
;;   (hash-table-size arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-test '(:rpc-debug) (arg/hashtable)
;;   "Return the test TABLE uses"
;;   (hash-table-test arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-weakness '(:rpc-debug) (arg/hashtable)
;;   "Return the weakness of TABLE."
;;   (hash-table-weak-p arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-p '(:rpc-debug) (arg/hashtable)
;;   "Return t if OBJ is a Lisp hash table object."
;;   (hash-table-p arg/hashtable)
;;   )

;; (defun-elisp elisp/clrhash '(:rpc-debug) (arg/hashtable)
;;   "Clear hash table TABLE and return it."
;;   (clrhash arg/hashtable)
;;   )

;; (defun-elisp elisp/gethash '(:rpc-debug) (arg/key arg/hashtable &optional arg/dflt)
;;   "Look up KEY in TABLE and return its associated value.
;; If KEY is not found, return DFLT which defaults to nil."
;;   (gethash arg/key arg/hashtable arg/dflt)
;;   )

;; (defun-elisp elisp/puthash '(:rpc-debug) (arg/key arg/value arg/hashtable)
;;   "Associate KEY with VALUE in hash table TABLE.
;; If KEY is already present in table, replace its current value with
;; VALUE.  In any case, return VALUE."
;;   (setf (gethash arg/key arg/hashtable) arg/value)
;;   )

;; (defun-elisp elisp/remhash '(:rpc-debug) (arg/key arg/hashtable)
;;   "Remove KEY from TABLE."
;;   (remhash arg/key arg/hashtable)
;;   )

;; (defun-elisp elisp/hash-table-keys-as-list '(:rpc-debug :internal) (arg/hashtable)
;;   ""
;;   (hash-table-keys arg/hashtable))
