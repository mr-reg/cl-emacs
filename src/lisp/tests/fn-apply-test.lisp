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

(uiop:define-package :cl-emacs/tests/fn-apply-test
    (:use
     :defstar
     :cl-emacs/lib/log
     :fiveam
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     :common-lisp
     )
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:reader #:cl-emacs/lib/reader))
  )
(in-package :cl-emacs/tests/fn-apply-test)
(log-enable :cl-emacs/tests/fn-apply-test :debug2)
(def-suite cl-emacs/tests/fn-apply-test)
(in-suite cl-emacs/tests/fn-apply-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-fn-apply
  (signals wrong-type-argument (apply 'el::+ 1))
  (is (equal 3 (apply 'el::+ '(1 2))))
  (is (equal 10 (apply 'el::+ 1 2 '(3 4))))
  (signals wrong-type-argument (apply 'el::+ 1 '(3 4) 2))
  (signals void-function (apply 'el::++ '(1)))
  (is (equal '(1 2 (5) 3 4) (apply 'el::list 1 2 '(5) '(3 4))))
  (signals wrong-type-argument (apply 'el::+))
  (is (equal 0 (apply 'el::+ '())))
  )

(defun test-me ()
  (run! 'cl-emacs/tests/fn-apply-test))
