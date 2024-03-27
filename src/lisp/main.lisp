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
  (:export :generate-elisp-block)
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
      (cond
        ((= input-type +message-type/notify-s-expr+)
         (let ((s-expr (babel:octets-to-string input-bytes :errorp nil)))
           (log-trace "#~a alien message: ~a" message-id s-expr))
         (values +message-type/notify-s-expr+ (babel:string-to-octets "\"done\"")))
        ((= input-type +message-type/rpc+)
         (let ((s-expr (babel:octets-to-string input-bytes :errorp nil))
               result)
           (log-trace "#~a rpc: ~a" message-id s-expr)
           (setq result (cl-emacs/elisp::eval-string s-expr))
           (log-trace "#~a rpc result: ~s" message-id result)
           (values +message-type/rpc+ (babel:string-to-octets (cl-emacs/elisp/internals:serialize-to-elisp result t)  :errorp nil))
           )
         )        
        (t (log-error "#~a unsupported message-type ~a" message-id input-type)
           (values +message-type/signal+ (babel:string-to-octets "error"))))
    (error (e)
      (log-trace "#~a rpc signal: ~s" message-id (cl-emacs/elisp/internals:condition-to-elisp-signal e))
      (values +message-type/signal+ (babel:string-to-octets
                                     (cl-emacs/elisp/internals:condition-to-elisp-signal e))))))

(defvar *intercomm-server-socket* nil)



(defun run-intercomm-server ()
  (log-debug "run-intercomm-server")
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
           do (unwind-protect
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
                (usocket:socket-close stream-socket))))
    (progn
      (log-info "closing intercomm socket")
      (usocket:socket-close *intercomm-server-socket*)
      (setq *intercomm-server-socket* nil)))
  )

(defun generate-elisp-block ()
  (with-output-to-string (stream)
    (format stream "(progn~%")
    (do-external-symbols (symbol :cl-emacs/elisp)
      (handler-case
          (let* ((function (symbol-function symbol))
                 (name (string-downcase (symbol-name symbol)))
                 (alias (cl-emacs/elisp/internals:get-elisp-alias symbol))
                 (args (mapcar #'string-downcase (ccl:arglist function)))
                 (docstring (documentation symbol 'function)))
            (when alias
              (format stream "(defalias '~a #'(lambda (" alias)
              (dolist (arg args)
                (format stream "~a " arg))
              (format stream ")~%")
              (format stream "~s~%" docstring)
              (format stream "  (common-lisp-apply 'cl-emacs/elisp:~a (list" name)
              (dolist (arg args)
                (unless (string= arg "&optional")
                  (format stream " ~a" arg)))
              (format stream "))))~%~%")))
        (undefined-function ()
          ;; skip non-function exports
          )))
    (format stream ")~%")))

(defun main ()
  (bt:make-thread
   #'run-intercomm-server)
  (log-debug "main complete")
  )

;; (main)
;; (cl-emacs/main::test)
