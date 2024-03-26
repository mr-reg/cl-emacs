(uiop:define-package :cl-emacs/log
    (:use :common-lisp :org.shirakumo.verbose)
  (:shadow #:log #:debug #:trace #:error #:warn)
  (:export #:log-debug
           #:log-error
           #:log-info
           #:log-trace
           #:log-enable
           #:log-disable
           #:log-reset
           )
  )
(in-package :cl-emacs/log)

(defun log-reset ()
  (uiop:delete-file-if-exists "cl-emacs.log")
  (define-pipe ()
    (category-tree-filter :name 'repl-category-filter)
    (file-faucet :file "cl-emacs.log")
    )
  (setf (repl-level) :info)

  ;; force disable ansi colors in repl
  (loop for pipeline across (org.shirakumo.verbose::pipeline *global-controller*)
        do (loop for element across pipeline
                 when (typep element 'repl-faucet)
                   do (setf (ansi-colors element) nil)))
  (setf *timestamp-format* NIL)
  )


;; (setf (repl-level) :trace)

(defparameter *logging-packages* nil)
(defun log-enable (pkg)
  (pushnew pkg *logging-packages*)
  (setf (repl-categories) *logging-packages*)  
  )
(defun log-disable (pkg)
  (setq *logging-packages* (delete pkg *logging-packages*))
  (setf (repl-categories) *logging-packages*)  
  )

(defmacro log-debug (&rest args)
  `(org.shirakumo.verbose:log :debug ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-error (&rest args)
  `(org.shirakumo.verbose:log :error ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-info (&rest args)
  `(org.shirakumo.verbose:log :info ,(intern (package-name *package*) :keyword) ,@args))

(defmacro log-trace (&rest args)
  `(org.shirakumo.verbose:log :trace ,(intern (package-name *package*) :keyword) ,@args))

(log-reset)
(log-enable :cl-emacs/log)
(log-enable :common-lisp-user)

