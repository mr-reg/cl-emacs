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
(defparameter *intercomm-port* 7447)
(defparameter *intercomm-host* "127.0.0.1")

(cffi:define-foreign-library temacs
  (t (:default "temacs")))

(defparameter *emacs-source-path* "../emacs/")
(pushnew (truename (concatenate 'string *emacs-source-path* "src/"))
         cffi:*foreign-library-directories*
         :test #'equalp)

(cffi:load-foreign-library 'temacs)
;; (cffi:defcfun ("emacs_main" emacs-main) :int
;;   (argc :int)
;;   (argv :pointer)
;;   )

;; (cffi:defcstruct alien-data-struct
;;   (type :char)
;;   (int-data :long)
;;   (char-data :pointer)
;;   (float-data :double)
;;   (counters :pointer))

;; (cffi:defcstruct intercomm-session-struct
;;   (message-id :long)
;;   (alien-input-length :long)
;;   (alien-input-array :pointer)
;;   (alien-output-length :long)
;;   (alien-output-array :pointer))

;; ;; (cffi:defcfun ("lock_common_lisp_mutex" lock-common-lisp-mutex) :void)
;; ;; (cffi:defcfun ("unlock_common_lisp_mutex" unlock-common-lisp-mutex) :void)
;; (cffi:defcfun ("lock_emacs_mutex" lock-emacs-mutex) :void)
;; (cffi:defcfun ("unlock_emacs_mutex" unlock-emacs-mutex) :void)
;; ;; (cffi:defcfun ("notify_emacs_cond" notify-emacs-cond) :void)
;; ;; (cffi:defcfun ("wait_for_emacs_cond" wait-for-emacs-cond) :void)
;; ;; (cffi:defcfun ("notify_common_lisp_cond" notify-common-lisp-cond) :void)
;; ;; (cffi:defcfun ("wait_for_common_lisp_cond" wait-for-common-lisp-cond) :void)

;; (cffi:defcvar ("message_counter" message-counter) :long)
;; (cffi:defcvar ("marker" marker) :int)
;; (cffi:defcvar ("intercomm_message" intercomm-message) intercomm-session-struct)

;; (defvar *emacs-thread* nil)
;; (defun run-emacs ()
;;   (let* ((args (list
;;                 (namestring (truename (concatenate 'string *emacs-source-path* "src/temacs")))
;;                 ;; "-nw"
;;                 ;; "-l" "~/configs/emacs-config-2/init.el"
;;                 ))
;;          (argc (length args))
;;          (argv (cffi:foreign-alloc :pointer :count (1+ argc) :initial-element (cffi:null-pointer)))
;;          )
;;     (loop for argi from 0
;;           for arg in args
;;           do (let ((c (cffi:foreign-alloc :char :initial-element 0 :count (1+ (length arg)))))
;;                (cffi:lisp-string-to-foreign arg c (1+ (length arg)))
;;                (setf (cffi:mem-aref argv :pointer argi) c)))
;;     (setf (cffi:mem-aref argv :pointer argc) (cffi:null-pointer))
;;     (log-info "starting emacs")
;;     (emacs-main argc argv)
;;     (loop for argi from 0 below argc
;;           do (cffi:foreign-free (cffi:mem-aref argv :pointer argi))) 
;;     (cffi:foreign-free argv)
;;     ))
;; (defun intercomm ()
;;   (format t "starting intercomm~%")
;;   ;; (lock-common-lisp-mutex)
;;   ;; (lock-emacs-mutex)
;;   ;; (setf common-lisp-active 1) 
;;   ;; (notify-emacs-cond)
;;   ;; (unlock-emacs-mutex)

;;   (loop 
;;     (lock-emacs-mutex)
;;     (if (= marker 1)
;;         (progn
;;           (cffi:with-foreign-slots ((message-id) intercomm-message intercomm-session-struct)
;;             (format t "lisp: input received ~a~%" message-id)
;;             )
;;           (setq marker 2))
;;         (sleep 0.001)
;;         )
;;     (unlock-emacs-mutex)
;;     )

;;   )


;; (defun print-thread-info ()
;;   (let* ((curr-thread (bt:current-thread))
;;          (curr-thread-name (bt:thread-name curr-thread))
;;          (all-threads (bt:all-threads)))
;;     (format t "Current thread: ~a~%~%" curr-thread)
;;     (format t "Current thread name: ~a~%~%" curr-thread-name)
;;     (format t "All threads:~% ~{~a~%~}~%" all-threads))
;;   nil)

(defconstant +message-type/stop-server+ 0)
(defconstant +message-type/s-expr+ 1)
(defconstant +message-type/error+ 2)

(defun process-intercomm-message (input-type input-bytes)
  "returns (output-type output-bytes)"
  (cond
    ((= input-type +message-type/s-expr+)
     (let ((s-expr (babel:octets-to-string input-bytes)))
       (log-debug "we need to eval ~s" s-expr))
     (values +message-type/s-expr+ "\"done\""))
    
    (t (log-error "unsupported input-type ~a" input-type)
       (values +message-type/error+ "error")))
  (values +message-type/error+ "somebody forgot to add result")
  )

(defparameter *intercomm-server-socket* nil)

(defun run-intercomm-server ()
  (log-debug "run-intercomm-server")
  (when *intercomm-server-socket*
    (ignore-errors
     (usocket:socket-shutdown *intercomm-server-socket* :both)
     (usocket:socket-close *intercomm-server-socket*)))
  (setq *intercomm-server-socket* nil)
  (loop
    until *intercomm-server-socket*
    do (handler-case
           (progn
             (setq *intercomm-server-socket* (usocket:socket-listen *intercomm-host* *intercomm-port* :element-type '(unsigned-byte 8)))
             (log-debug "socket allocated"))
         (usocket:address-in-use-error ()
           (log-error "address in use")
           (sleep 1))
         ))
  (log-info "intercomm started")
  (unwind-protect
       (loop
         for stream-socket = (usocket:socket-accept *intercomm-server-socket* :element-type '(unsigned-byte 8))
         do (unwind-protect
                 (let* ((stream (usocket:socket-stream stream-socket))
                        (input-length (lisp-binary:read-integer 8 stream :signed nil))
                        (input-type (lisp-binary:read-integer 8 stream :signed nil))
                        (input-bytes (lisp-binary:read-bytes input-length stream)))
                   ;; (log-debug "intercomm message received")
                   (when (= input-type +message-type/stop-server+)
                     (log-info "received stop-server message")
                     (return-from run-intercomm-server))
                   (multiple-value-bind (output-type output-bytes)
                       (process-intercomm-message input-type input-bytes)
                     (lisp-binary:write-integer (length output-bytes) 8 stream :signed nil)
                     (lisp-binary:write-integer output-type 8 stream :signed nil)
                     (lisp-binary:write-bytes output-bytes stream))
                   ;; (log-debug "intercomm message completed")
                   )
              (usocket:socket-close stream-socket)))
    (usocket:socket-close *intercomm-server-socket*))
  (setq *intercomm-server-socket* nil))

(defun main ()

  (run-intercomm-server)
  (log-debug "main complete")
  )
;; (main)
;; (cl-emacs/main::test)
