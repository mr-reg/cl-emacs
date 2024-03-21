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

;; (cffi:defcfun ("lock_common_lisp_mutex" lock-common-lisp-mutex) :void)
;; (cffi:defcfun ("unlock_common_lisp_mutex" unlock-common-lisp-mutex) :void)
(cffi:defcfun ("lock_emacs_mutex" lock-emacs-mutex) :void)
(cffi:defcfun ("unlock_emacs_mutex" unlock-emacs-mutex) :void)
;; (cffi:defcfun ("notify_emacs_cond" notify-emacs-cond) :void)
;; (cffi:defcfun ("wait_for_emacs_cond" wait-for-emacs-cond) :void)
;; (cffi:defcfun ("notify_common_lisp_cond" notify-common-lisp-cond) :void)
;; (cffi:defcfun ("wait_for_common_lisp_cond" wait-for-common-lisp-cond) :void)

(cffi:defcvar ("message_counter" message-counter) :long)
(cffi:defcvar ("marker" marker) :int)
(cffi:defcvar ("intercomm_message" intercomm-message) intercomm-session-struct)

(defvar *emacs-thread* nil)
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
  (format t "starting intercomm~%")
  ;; (lock-common-lisp-mutex)
  ;; (lock-emacs-mutex)
  ;; (setf common-lisp-active 1) 
  ;; (notify-emacs-cond)
  ;; (unlock-emacs-mutex)
  
  (loop 
    (lock-emacs-mutex)
    (if (= marker 1)
        (progn
          (cffi:with-foreign-slots ((message-id) intercomm-message intercomm-session-struct)
            (format t "lisp: input received ~a~%" message-id)
            )
          (setq marker 2))
        (sleep 0.001)
        )
    (unlock-emacs-mutex)
    )

  )


(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
         (curr-thread-name (bt:thread-name curr-thread))
         (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(defun main ()
  (setq *emacs-thread*
        (bt:make-thread
         #'run-emacs :name "emacs"))
  (bt:make-thread
   #'intercomm :name "intercomm")

  ;; (loop for idx from 0 below 10
  ;;       do (format t "~s~%" (cffi:mem-aref exec-name :uchar idx )))

  (log-debug "main complete")
  )
(main)
;; (cl-emacs/main::test)
