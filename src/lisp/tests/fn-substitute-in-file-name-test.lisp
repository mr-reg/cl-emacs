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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/tests/fn-substitute-in-file-name-test
    (:use
     :defstar
     :cl-emacs/lib/log
     :fiveam
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     :cl-emacs/fn-eval
     :cl-emacs/fn-substitute-in-file-name
     :cl-emacs/fns
     )
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:reader #:cl-emacs/lib/reader))
  )
(in-package :cl-emacs/tests/fn-substitute-in-file-name-test)
(log-enable :cl-emacs/tests/fn-substitute-in-file-name-test :debug2)
(def-suite cl-emacs/tests/fn-substitute-in-file-name-test)
(in-suite cl-emacs/tests/fn-substitute-in-file-name-test)
(named-readtables:in-readtable mstrings:mstring-syntax)

(test test-fn-substitute-in-file-name-test
  (is (equal 
       (reader:read-simple "\"loadup.el\"")
       (eval (reader:read-simple "(substitute-in-file-name \"loadup.el\")"))))
  
  )

(defun test-me ()
  (run! 'cl-emacs/tests/fn-substitute-in-file-name-test))
