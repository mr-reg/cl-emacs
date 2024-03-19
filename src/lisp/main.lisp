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

(uiop:define-package :cl-emacs/main
    (:use :common-lisp)
  )
(in-package :cl-emacs/main)

(cffi:define-foreign-library temacs
  (t (:default "temacs")))

(defparameter *emacs-source-path* "../emacs/")
(pushnew (truename (concatenate 'string *emacs-source-path* "src/"))
         cffi:*foreign-library-directories*
         :test #'equalp)

(cffi:load-foreign-library 'temacs)
(cffi:defcfun ("emacs_main" emacs-main) :int
  (argc :int)
  (argv :pointer)
  )
(defun test ()
  ;; (loop for idx from 0 below 10
  ;;       do (format t "~s~%" (cffi:mem-aref exec-name :uchar idx )))
  (let* ((args (list
                (namestring (truename (concatenate 'string *emacs-source-path* "src/temacs")))
                ;; "-l" "~/configs/emacs-config-2/init.el"
                ))
         (argc (length args))
         (argv (cffi:foreign-alloc :pointer :count (1+ argc) :initial-element (cffi:null-pointer)))
         )
    (loop for argi from 0
          for arg in args
          do (let ((c (cffi:foreign-alloc :char :initial-element 0 :count (1+ (length arg)))))
               (cffi:lisp-string-to-foreign arg c (1+ (length arg)))
               (setf (cffi:mem-aref argv :pointer argi) c)))
    (setf (cffi:mem-aref argv :pointer argc) (cffi:null-pointer))
    (emacs-main argc argv)
    (loop for argi from 0 below argc
          do (cffi:foreign-free (cffi:mem-aref argv :pointer argi))) 
    (cffi:foreign-free argv)
    )
  (print "done")
  )

;; (cl-emacs/main::test)
