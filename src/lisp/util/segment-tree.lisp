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

(uiop:define-package :cl-emacs/util/prop-intervals
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam))
(in-package :cl-emacs/util/prop-intervals)
(log-enable :cl-emacs/util/prop-intervals :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defstruct prop-interval
  (start 0 :type fixnum)
  (len 0 :type fixnum)
  (plist nil :type list)
  )
(defun test-tree ()
  (let ((tree (trees:make-binary-tree :avl #'< :key #'prop-interval-start)))
    (loop for start from 0 to 20 by 4
          do (trees:insert (make-prop-interval :start start :len 3) tree))
    (trees:pprint-tree tree))
  )
