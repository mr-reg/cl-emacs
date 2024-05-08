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
(uiop:define-package :cl-emacs/reader-utils
    (:use :common-lisp :cl-emacs/log :alexandria :fiveam)
  (:export #:parse-elisp-number)
  )
(in-package :cl-emacs/reader-utils)
(log-enable :cl-emacs/reader-utils :debug1)
(def-suite cl-emacs/reader-utils)
(in-suite cl-emacs/reader-utils)

(defun parse-elisp-number (chardata)
  (handler-case
      (parse-number:parse-real-number chardata)
    (parse-error ()
      nil)))
(test parse-elisp-number
  (is (= (parse-elisp-number "24") 24))
  (is (= (parse-elisp-number "-3") -3))
  (is (= (parse-elisp-number "0") 0))
  (is (= (parse-elisp-number "0.0") 0.0))
  (is (= (parse-elisp-number "-4.5") -4.5))
  (is (= (parse-elisp-number "2.71828") 2.71828))
  (is (= (parse-elisp-number "1.5e2") 150.0))
  )

