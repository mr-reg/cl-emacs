(uiop:define-package :cl-emacs/log
    (:use :common-lisp)  
  (:export #:log-debug
           #:log-error
           #:log-info
           #:log-trace
           #:log-enable
           #:log-disable))
(in-package :cl-emacs/log)

(setf org.shirakumo.verbose:*timestamp-format* NIL)

(setf (org.shirakumo.verbose:repl-level) :debug)
;; (setf (org.shirakumo.verbose:repl-level) :trace)

(defparameter *logging-packages* nil)
(defun log-enable (pkg)
  (pushnew pkg *logging-packages*)
  (setf (org.shirakumo.verbose:repl-categories) *logging-packages*)  
  )
(defun log-disable (pkg)
  (setq *logging-packages* (delete pkg *logging-packages*))
  (setf (org.shirakumo.verbose:repl-categories) *logging-packages*)  
  )

(defmacro log-debug (&rest args)
  `(org.shirakumo.verbose:log :debug ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-error (&rest args)
  `(org.shirakumo.verbose:log :error ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-info (&rest args)
  `(org.shirakumo.verbose:log :info ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-trace (&rest args)
  `(org.shirakumo.verbose:log :trace ,(intern (package-name *package*) :keyword) ,@args))

(log-enable :cl-emacs/log)
