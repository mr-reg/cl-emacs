(defpackage :cl-emacs/log
  (:use :common-lisp)  
  (:export #:log-debug
           #:log-error
           #:log-info
           #:log-trace
           #:log-enable
           #:log-disable
           #:log-reset
           )
  (:local-nicknames (#:v #:org.shirakumo.verbose)))
(in-package :cl-emacs/log)
(setf (v:repl-level) :info)

(defun log-reset ()
  (uiop:delete-file-if-exists "cl-emacs.log")
  (v:define-pipe ()
    (v:category-tree-filter :name 'repl-category-filter)
    (v:file-faucet :file "cl-emacs.log")
    )

  ;; force disable ansi colors in repl
  (loop for pipeline across (v::pipeline v:*global-controller*)
        do (loop for element across pipeline
                 when (typep element 'v:repl-faucet)
                   do (setf (v:ansi-colors element) nil)))
  (setf org.shirakumo.verbose:*timestamp-format* NIL)
  )


;; (setf (v:repl-level) :trace)

(defparameter *logging-packages* nil)
(defun log-enable (pkg)
  (pushnew pkg *logging-packages*)
  (setf (v:repl-categories) *logging-packages*)  
  )
(defun log-disable (pkg)
  (setq *logging-packages* (delete pkg *logging-packages*))
  (setf (v:repl-categories) *logging-packages*)  
  )

(defmacro log-debug (&rest args)
  `(v:log :debug ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-error (&rest args)
  `(v:log :error ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-info (&rest args)
  `(v:log :info ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-trace (&rest args)
  `(v:log :trace ,(intern (package-name *package*) :keyword) ,@args))

(log-reset)
(log-enable :cl-emacs/log)
(log-enable :common-lisp-user)

