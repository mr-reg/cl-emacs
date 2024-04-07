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
    (:use :common-lisp :cl-emacs/log :cl-emacs/elisp)
  (:import-from :common-lisp-user #:quit)
  )
(in-package :cl-emacs/main)
(log-enable :cl-emacs/main)
(defparameter *intercomm-port* 7447)
(defparameter *intercomm-host* "127.0.0.1")

(cffi:define-foreign-library temacs
  (t (:default "temacs")))

(defparameter *emacs-source-path* "../emacs/")
;; (pushnew (truename (concatenate 'string *emacs-source-path* "src/"))
;;          cffi:*foreign-library-directories*
;;          :test #'equalp)

;; (cffi:load-foreign-library 'temacs)

(defconstant +message-type/stop-server+ 100)
(defconstant +message-type/notify-s-expr+ 1)
(defconstant +message-type/signal+ 2)
(defconstant +message-type/rpc+ 3)

(defun process-intercomm-message (message-id input-type input-bytes)
  "returns (output-type output-bytes)"
  (handler-case
      (let* ((stream (flexi-streams:make-in-memory-input-stream input-bytes))
             (argc (cl-emacs/elisp/internals:read-lisp-binary-object stream))
             (argv (loop for argi below argc
                         collect (cl-emacs/elisp/internals:read-lisp-binary-object stream)))
             (out-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
        (cond
          ((= input-type +message-type/notify-s-expr+)
           (log-debug "message argc=~a ~s" argc argv)
           (cl-emacs/elisp/internals:write-lisp-binary-object "ack" out-stream)
           (values +message-type/notify-s-expr+ (flexi-streams:get-output-stream-sequence out-stream))       
           )
          ((= input-type +message-type/rpc+)
           (log-debug "rpc argc=~a ~s" argc argv)
           ;; (unless (= argc 3)
           ;;   (error "unsupported rpc type"))
           (let ((result (cl-emacs/elisp:rpc-apply argv)))
             (log-debug "rpc result ~s" result)
             (cl-emacs/elisp/internals:write-lisp-binary-object result out-stream))
           (values +message-type/notify-s-expr+ (flexi-streams:get-output-stream-sequence out-stream))
           ;; (let ((s-expr (babel:octets-to-string input-bytes :errorp nil))
           ;;       result)
           ;;   (log-trace "#~a rpc: ~a" message-id s-expr)
           ;;   (setq result (cl-emacs/elisp::eval-string s-expr))
           ;;   (log-trace "#~a rpc result: ~s" message-id result)
           ;;   (values +message-type/rpc+ (babel:string-to-octets (cl-emacs/elisp/internals:serialize-to-elisp result t)  :errorp nil))
           ;;   )
           )        
          (t (error (format nil "#~a unsupported message-type ~a" message-id input-type))
             ))
        )
    
    
    (error (e)
      (let ((signal (cl-emacs/elisp/internals:condition-to-elisp-signal e))
            (out-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
        (log-debug "signal ~s" signal)
        (cl-emacs/elisp/internals:write-lisp-binary-object signal out-stream)
        (values +message-type/signal+ (flexi-streams:get-output-stream-sequence out-stream))
        )
      ;; (break)
      ;; (values +message-type/signal+ (with-output-to-string (stream)
      ;;                                 (cl-emacs/elisp/internals:write-lisp-binary-object "error" stream)))
      ;; (log-trace "#~a rpc signal: ~s" message-id (cl-emacs/elisp/internals:condition-to-elisp-signal e))
      ;; (break)
      ;; (values +message-type/signal+ (babel:string-to-octets
      ;;                                (cl-emacs/elisp/internals:condition-to-elisp-signal e)))
      )
    ))

(defvar *intercomm-server-socket* nil)



(defun run-intercomm-server ()
  (log-debug "run-intercomm-server")
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to hello world server...")
      (pzmq:connect requester server-address)
      (dotimes (i 10)
        (format t "Sending Hello ~d...~%" i)
        (pzmq:send requester "Hello")
        (write-string "Receiving... ")
        (write-line (pzmq:recv-string requester)))))
  
  (when *intercomm-server-socket*
    (ignore-errors
     (usocket:socket-shutdown *intercomm-server-socket* :both)
     (usocket:socket-close *intercomm-server-socket*)))
  (setq *intercomm-server-socket* nil)
  (log-reset)
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
       (let ((message-id 0))
         (loop
           for stream-socket = (usocket:socket-accept *intercomm-server-socket* :element-type '(unsigned-byte 8))
           do
              (restart-case
                  (unwind-protect
                       (progn
                         (handler-case
                             (let* ((stream (usocket:socket-stream stream-socket))
                                    (input-length (lisp-binary:read-integer 8 stream :signed nil))
                                    (input-type (lisp-binary:read-integer 8 stream :signed nil))
                                    (input-bytes (lisp-binary:read-bytes input-length stream)))
                               ;; (incf message-id)
                               ;; (log-debug "intercomm message received")
                               (when (= input-type +message-type/stop-server+)
                                 (log-info "received stop-server message")
                                 (return-from run-intercomm-server))
                               (multiple-value-bind (output-type output-bytes)
                                   (process-intercomm-message message-id
                                                              input-type input-bytes)
                                 (lisp-binary:write-integer (length output-bytes) 8 stream :signed nil)
                                 (lisp-binary:write-integer output-type 8 stream :signed nil)
                                 (lisp-binary:write-bytes output-bytes stream))
                               ;; (log-debug "intercomm message completed")
                               )
                           (end-of-file ()
                             (log-error "end-of-file detected"))))
                    (progn
                      ;; (log-info "closing client socket")
                      (usocket:socket-close stream-socket)))
                (just-continue () nil))))
    (progn
      (log-info "closing intercomm socket")
      (usocket:socket-close *intercomm-server-socket*)
      (setq *intercomm-server-socket* nil)))
  )


;; (pzmq:connect)

(defun main ()
  (bt:make-thread
   #'run-intercomm-server)
  (log-debug "main complete")
  )

;; (main)
;; (cl-emacs/main::test)
