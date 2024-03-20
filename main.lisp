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
    (:use :common-lisp :cl-emacs/log)
  )
(in-package :cl-emacs/main)
(log-enable :cl-emacs/main)
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

(cffi:defcstruct alien-data-struct
  (type :char)
  (int-data :long)
  (char-data :pointer)
  (float-data :double)
  (counters :pointer))

(cffi:defcstruct intercomm-session-struct
  (message-id :long)
  (alien-input-length :long)
  (alien-input-array :pointer)
  (alien-output-length :long)
  (alien-output-array :pointer))

(cffi:defcfun ("lock_input_mutex" lock-input-mutex) :void)
(cffi:defcfun ("unlock_input_mutex" unlock-input-mutex) :void)
(cffi:defcfun ("lock_output_mutex" lock-output-mutex) :void)
(cffi:defcfun ("unlock_output_mutex" unlock-output-mutex) :void)

(cffi:defcfun ("notify_input_ready" notify-input-ready) :void)
(cffi:defcfun ("wait_for_input" wait-for-input) :void)
(cffi:defcfun ("notify_output_ready" notify-output-ready) :void)
(cffi:defcfun ("wait_for_output" wait-for-output) :void)

(cffi:defcvar ("message_counter" message-counter) :long)
(cffi:defcvar ("intercomm_message" intercomm-message) intercomm-session-struct)

(defun run-emacs ()
  (let* ((args (list
                (namestring (truename (concatenate 'string *emacs-source-path* "src/temacs")))
                ;; "-nw"
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
    (log-info "starting emacs")
    (emacs-main argc argv)
    (loop for argi from 0 below argc
          do (cffi:foreign-free (cffi:mem-aref argv :pointer argi))) 
    (cffi:foreign-free argv)
    ))
(defun intercomm ()
  (loop 
    (format t "lisp: here1~%")
    (lock-input-mutex)
    (format t "lisp: starting wait for input~%")
    (wait-for-input)
    (format t "lisp: here2~%")
    (cffi:with-foreign-slots ((message-id) intercomm-message intercomm-session-struct)
      (format t "lisp: input received ~a~%" message-id)
      )
    (format t "lisp: here3~%")
    (lock-output-mutex)
    (format t "lisp: here4~%")
    (notify-output-ready)
    (format t "lisp: here5~%")
    (unlock-output-mutex)
    (format t "lisp: here6~%")
    (unlock-input-mutex)
    (format t "lisp: here7~%")
    ))


(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
         (curr-thread-name (bt:thread-name curr-thread))
         (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(defun main ()
  (bt:make-thread
   #'intercomm :name "intercomm")
  (bt:make-thread
   #'run-emacs :name "emacs")

  ;; (loop for idx from 0 below 10
  ;;       do (format t "~s~%" (cffi:mem-aref exec-name :uchar idx )))

  (log-debug "main complete")
  )
(main)
;; (cl-emacs/main::test)
