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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/fns-test
  
  )
(in-package :cl-emacs/tests/fns-test)
(log-enable :cl-emacs/tests/fns-test :debug2)
(def-suite cl-emacs/tests/fns-test)
(in-suite cl-emacs/tests/fns-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-sxhash-eq
  (is (@= (@sxhash-eq 340) (@sxhash-eq 340)))
  (is (@= (@sxhash-eq cl-emacs/data::*nan*)
          (@sxhash-eq cl-emacs/data::*nan*)))
  (is (@= (@sxhash-eq '#:symbol) (@sxhash-eq '#:symbol)))
  (is-false (@= (@sxhash-eq 345) (@sxhash-eq 346))))

(test test-eql
  (is (@eql 2 2))
  (is (@= (@sxhash-eql 2) (@sxhash-eql 2)))

  (is (@eql 'el::sym 'el::sym))
  (is (@= (@sxhash-eql 'el::sym) (@sxhash-eql 'el::sym)))

  (is (@eql 10e2 1000.0))
  (is (@= (@sxhash-eql 10e2) (@sxhash-eql 1000.0)))

  (is-false (@eql 10 10.0))
  (is-false (@= (@sxhash-eql 10) (@sxhash-eql 10.0)))

  (is-false (cl:eql 0.0 -0.0))
  (is-false (@= (@sxhash-eql 0.0) (@sxhash-eql -0.0)))

  (is (@eql cl-emacs/data::*nan* (@/ 0.0 0.0)))
  (is (@= (@sxhash-eql cl-emacs/data::*nan*)
          (@sxhash-eql (@/ 0.0 0.0))))
  )
(test test-equal
  (is (@equal 'foo 'foo))
  (is (@= (@sxhash-equal 'foo) (@sxhash-equal 'foo)))

  (is (@equal 456 456))
  (is (@= (@sxhash-equal 456) (@sxhash-equal 456)))

  (is (@equal (pstrings:build-pstring "asdf") (pstrings:build-pstring "asdf")))
  (is (@= (@sxhash-equal (pstrings:build-pstring "asdf"))
          (@sxhash-equal (pstrings:build-pstring "asdf"))))
  (is-false (@eq (pstrings:build-pstring "asdf") (pstrings:build-pstring "asdf")))

  (is (@equal (@list 1 '(2 (3))) (@list 1 '(2 (3)))))
  (is (@= (@sxhash-equal (@list 1 '(2 (3))))
          (@sxhash-equal (@list 1 '(2 (3))))))
  (is-false (@eq (@list 1 '(2 (3))) (@list 1 '(2 (3)))))

  (is (@equal #((1 2) 3) #((1 2) 3)))
  (is (@= (@sxhash-equal #((1 2) 3))
          (@sxhash-equal #((1 2) 3))))
  (is-false (@eq #((1 2) 3) #((1 2) 3)))

  (is-false (@equal "asdf" "ASDF"))
  (is-false (@= (@sxhash-equal "asdf")
                (@sxhash-equal "ASDF")))

  (is (@equal (pstrings:build-pstring "asdf") 
              (@propertize (pstrings:build-pstring "asdf") 'asdf t)))
  (is (@equal (@sxhash-equal (pstrings:build-pstring "asdf"))
              (@sxhash-equal (@propertize (pstrings:build-pstring "asdf") 'asdf t))))

  (is (@equal cl-emacs/data::*nan*
              (@/ 0.0 0.0)))
  (is (@= (@sxhash-equal cl-emacs/data::*nan*)
          (@sxhash-equal (@/ 0.0 0.0))))

  (is-false (@equal 0.0 -0.0))
  (is-false (@= (@sxhash-equal 0.0)
                (@sxhash-equal -0.0)))
  ;; TODO: add tests for markers
  ;; (is (equal (point-marker) (point-marker)))
  ;; (is-false (eq (point-marker) (point-marker)))
  )
(test test-equal-including-properties
  (is-false (@equal-including-properties
             (pstrings:build-pstring "asdf")
             (@propertize (pstrings:build-pstring "asdf") 'asdf t)))
  (is-false (=
             (@sxhash-equal-including-properties (pstrings:build-pstring "asdf"))
             (@sxhash-equal-including-properties (@propertize (pstrings:build-pstring "asdf") 'asdf t))))
  (is (@equal-including-properties
       (pstrings:build-pstring "asdf" '((asdf . t)))
       (@propertize (pstrings:build-pstring "asdf") 'asdf t)))
  (is (@=
       (@sxhash-equal-including-properties (pstrings:build-pstring "asdf" '((asdf . t))))
       (@sxhash-equal-including-properties (@propertize (pstrings:build-pstring "asdf") 'asdf t)))))

(test test-hash-table
  (let ((table (@make-hash-table :test 'eq)))
    (@puthash 'key1 'value1 table)
    (is (@eq 'value1 (@gethash 'key1 table)))
    (@puthash 'key2 'value2 table)
    (@puthash 'key1 'value3 table)
    (is (@eq 'value3 (@gethash 'key1 table)))
    (is (@eq 'value2 (@gethash 'key2 table)))
    (is (@= 2 (@hash-table-count table)))
    (@puthash (pstrings:build-pstring "abc") 1 table)
    (@puthash (pstrings:build-pstring "abc") 1 table)
    (is (@= 4 (@hash-table-count table)))
    (is-false (@gethash (pstrings:build-pstring "abc") table))
    )
  (let ((table (@make-hash-table :test 'equal)))
    (@puthash (pstrings:build-pstring "abc") 1 table)
    (@puthash (pstrings:build-pstring "abc") (pstrings:build-pstring "mno") table)
    (is (@= 1 (@hash-table-count table)))
    (is (@equal (@gethash (pstrings:build-pstring "abc") table) 
                (pstrings:build-pstring "mno")))
    )
  )

(defun test-me ()
  (run! 'cl-emacs/tests/fns-test))
