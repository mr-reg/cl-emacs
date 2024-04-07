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
    (:use :common-lisp :cl-emacs/log :cl-emacs/elisp :cl-emacs/elisp/internals)
  (:import-from :common-lisp-user #:quit)
  )
(in-package :cl-emacs/main)
(log-enable :cl-emacs/main)
(defparameter *intercomm-listen-address* "tcp://*:7447")

(cffi:define-foreign-library temacs
  (t (:default "temacs")))

(defparameter *emacs-source-path* "../emacs/")
;; (pushnew (truename (concatenate 'string *emacs-source-path* "src/"))
;;          cffi:*foreign-library-directories*
;;          :test #'equalp)

;; (cffi:load-foreign-library 'temacs)

(defconstant +message-type/stop-server+ 100)
;; (defconstant +message-type/notify-s-expr+ 1)
(defconstant +message-type/signal+ 2)
(defconstant +message-type/rpc+ 3)

(defun process-intercomm-message (message-type argv)
  "returns (output-type output-bytes)"
  (let ((out-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
    (handler-case
        (cond
          ;; ((= message-type +message-type/notify-s-expr+)
          ;;  (write-lisp-binary-object +message-type/notify-s-expr+ out-stream)
          ;;  (write-lisp-binary-object "ack" out-stream)
          ;;  )
          ((= message-type +message-type/rpc+)
           (let ((result (cl-emacs/elisp:rpc-apply argv)))
             (log-debug "rpc result ~s" result)
             (write-lisp-binary-object +message-type/rpc+ out-stream)
             (write-lisp-binary-object result out-stream)
             )
           )        
          (t (error (format nil "unsupported message-type ~a" message-type))
             ))
      (error (e)
        (let ((signal (condition-to-elisp-signal e)))
          (log-debug "signal ~s" signal)
          (write-lisp-binary-object +message-type/signal+ out-stream)
          (write-lisp-binary-object signal out-stream))
        ;; (break)
        ;; (values +message-type/signal+ (with-output-to-string (stream)
        ;;                                 (cl-emacs/elisp/internals:write-lisp-binary-object "error" stream)))
        ;; (log-trace "#~a rpc signal: ~s" message-id (cl-emacs/elisp/internals:condition-to-elisp-signal e))
        ;; (break)
        ;; (values +message-type/signal+ (babel:string-to-octets
        ;;                                (cl-emacs/elisp/internals:condition-to-elisp-signal e)))
        )
      )
    (flexi-streams:get-output-stream-sequence out-stream)))

(defvar *intercomm-server-socket* nil)



(defun run-intercomm-server ()
  (log-reset)
  (log-debug "run-intercomm-server")
  (pzmq:with-context nil
    (pzmq:with-socket zmq-socket :rep
      (pzmq:bind zmq-socket *intercomm-listen-address*)
      (loop
        ;; (log-info "waiting for message")
        (restart-case
            (pzmq:with-message in-message
              (pzmq:msg-recv in-message zmq-socket)
              (let ((msg-bytes (cffi:foreign-array-to-lisp (pzmq:msg-data in-message)
                                                           (list :array :unsigned-char (pzmq:msg-size in-message))
                                                           :element-type '(unsigned-byte 8) )))
                ;; (log-info "message size ~a" (length msg-bytes))
                (handler-case
                    (let* ((in-stream (flexi-streams:make-in-memory-input-stream msg-bytes))
                           (message-id (read-lisp-binary-object in-stream))
                           (message-type (read-lisp-binary-object in-stream))
                           (nargs (read-lisp-binary-object in-stream))
                           (argv (loop for idx from 0 below nargs
                                       collect (read-lisp-binary-object in-stream))))
                      (log-debug "#~a intercomm message received ~s" message-id argv)
                      (when (= message-type +message-type/stop-server+)
                        (log-info "received stop-server message")
                        (return-from run-intercomm-server))
                      (let ((output-bytes (process-intercomm-message message-type argv)))
                        (pzmq:with-message out-message
                          (log-debug "sending response size ~a" (length output-bytes))
                          (pzmq:msg-init-size out-message (length output-bytes))
                          (let ((ptr (pzmq:msg-data out-message)))
                            (loop for idx from 0 below (length output-bytes)
                                  do (setf (cffi:mem-aref ptr :unsigned-char idx) (aref output-bytes idx))))
                          (pzmq:msg-send out-message zmq-socket)))
                      ;; (log-debug "intercomm message completed")
                      )
                  (end-of-file ()
                    (log-error "end-of-file detected"))))
              )
          (just-continue () nil)))
      )))


;; (pzmq:connect)

(defun main ()
  ;; (bt:make-thread
  ;;  #'run-intercomm-server)
  (run-intercomm-server)
  (log-debug "main complete")
  )

;; (main)
;; (cl-emacs/main::test)
