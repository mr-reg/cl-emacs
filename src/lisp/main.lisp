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
(defparameter *intercomm-connect-address* "tcp://127.0.0.1:7447")

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
(defparameter *full-rpc-debug* t)
(defparameter *rpc-filter-time* 0.1)
(defun process-intercomm-message (message-id message-type argv)
  "returns (output-type output-bytes)"
  (let ((out-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
        (write-stack (make-hash-table)))
    (handler-case
        (cond
          ;; ((= message-type +message-type/notify-s-expr+)
          ;;  (write-lisp-binary-object +message-type/notify-s-expr+ out-stream)
          ;;  (write-lisp-binary-object "ack" out-stream)
          ;;  )
          ((= message-type +message-type/rpc+)
           (let ((result (cl-emacs/elisp:rpc-apply argv))
                 (sym (string-to-elisp-symbol (car argv))))
             (when (or *full-rpc-debug* (find :rpc-debug (gethash sym *defun-flags*)))
               (let ((time (set-timer)))
                 (when (or (null *rpc-filter-time*) (>= time *rpc-filter-time*))
                   (log-debug "#~a ~a rpc ~s -> ~s" message-id time argv result)
                   ))
               )
             (write-lisp-binary-object +message-type/rpc+ out-stream write-stack)
             (write-lisp-binary-object result out-stream write-stack)
             )
           )        
          (t (error (format nil "unsupported message-type ~a" message-type))
             ))
      (error (e)
        ;; (break)
        (let ((signal (condition-to-elisp-signal e)))
          (log-debug "#~a ~a signal ~s -> ~s" message-id (set-timer) argv signal)
          (write-lisp-binary-object +message-type/signal+ out-stream write-stack)
          (write-lisp-binary-object signal out-stream write-stack))
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

(defvar *timer* 0)
(defun set-timer ()
  (let* ((new-time (get-internal-real-time))
         (time (- new-time *timer*)))
    (setq *timer* new-time)
    (* 1.0 (/ time internal-time-units-per-second))))


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
              (set-timer)
              (let ((msg-bytes (cffi:foreign-array-to-lisp (pzmq:msg-data in-message)
                                                           (list :array :unsigned-char (pzmq:msg-size in-message))
                                                           :element-type '(unsigned-byte 8) )))
                ;; (log-info "message size ~a" (length msg-bytes))
                (handler-case
                    (let* ((in-stream (flexi-streams:make-in-memory-input-stream msg-bytes))
                           (stack (make-hash-table))
                           (message-id (read-lisp-binary-object in-stream stack))
                           (message-type (read-lisp-binary-object in-stream stack))
                           (nargs (read-lisp-binary-object in-stream stack))
                           (argv (loop for idx from 0 below nargs
                                       collect (read-lisp-binary-object in-stream stack))))
                      (when (= message-type +message-type/stop-server+)
                        (log-info "received stop-server message")
                        (return-from run-intercomm-server))
                      (let ((output-bytes (process-intercomm-message message-id message-type argv)))
                        (pzmq:with-message out-message
                          ;; (log-debug "sending response size ~a" (length output-bytes))
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

(defun stop-intercomm-server ()
  (pzmq:with-context nil
    (pzmq:with-socket zmq-socket :req
      (pzmq:connect zmq-socket *intercomm-connect-address*)
      (let ((out-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
            (write-stack (make-hash-table)))
        (write-lisp-binary-object 0 out-stream write-stack)
        (write-lisp-binary-object +message-type/stop-server+ out-stream write-stack)
        (write-lisp-binary-object 0 out-stream write-stack)
        (let ((output-bytes (flexi-streams:get-output-stream-sequence out-stream)))
          (pzmq:with-message out-message
            ;; (log-debug "sending response size ~a" (length output-bytes))
            (pzmq:msg-init-size out-message (length output-bytes))
            (let ((ptr (pzmq:msg-data out-message)))
              (loop for idx from 0 below (length output-bytes)
                    do (setf (cffi:mem-aref ptr :unsigned-char idx) (aref output-bytes idx))))
            (pzmq:msg-send out-message zmq-socket)))))))


;; (pzmq:connect)

(defun main ()
  (bt:make-thread
   #'run-intercomm-server)
  ;; (run-intercomm-server)
  (log-debug "main complete")
  )

;; (main)
;; (cl-emacs/main::test)
