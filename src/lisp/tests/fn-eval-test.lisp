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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/fn-eval-test

  )
(in-package :cl-emacs/tests/fn-eval-test)
(log-enable :cl-emacs/tests/fn-eval-test :debug2)
(def-suite cl-emacs/tests/fn-eval-test)
(in-suite cl-emacs/tests/fn-eval-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-fn-eval
  (is (@equal 3 (@eval (reader:read-simple "3"))))
  (is (@equal 0 (@eval (reader:read-simple "(+)"))))
  (is (@equal 1 (@eval (reader:read-simple "(+ 1)"))))
  (is (@equal 3 (@eval (reader:read-simple "(+ 1 2)"))))
  (signals wrong-type-argument (@eval (reader:read-simple "(+ . 2)")))
  (signals wrong-type-argument (@eval (reader:read-simple "(+ 1 . 2)")))
  (is (@equal 9 (@eval (reader:read-simple "(+ 1 (+ 3 3) 2)"))))
  )

(defun test-me ()
  (run! 'cl-emacs/tests/fn-eval-test))
