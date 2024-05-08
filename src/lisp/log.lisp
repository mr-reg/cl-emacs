(uiop:define-package :cl-emacs/log
    (:use :common-lisp)
  (:shadow #:log #:debug #:trace #:error #:warn)
  (:export #:log-debug
           #:log-error
           #:log-info
           #:log-debug1
           #:log-enable
           #:log-disable
           #:log-reset
           )
  )
(in-package :cl-emacs/log)

(defparameter *log-filename* "cl-emacs.log")
(defvar *log-file-stream* nil)
(defvar *log-config-lock* (bt:make-lock "log-config-hook"))
(defun log-reset ()
  (bt:with-lock-held (*log-config-lock*)
    (when *log-file-stream*
      (ignore-errors
       (close *log-file-stream*))
      )
    (setq *log-file-stream* (open *log-filename* :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create
                                                 :sharing :lock)))

  (setf vom:*log-hook*
        (lambda (level package package-level)
          (declare (ignore package package-level))
          (bt:with-lock-held (*log-config-lock*)
            (let (streams)
              (when *log-file-stream* (push *log-file-stream* streams))
              (when (<= level 9) ;; 8 = debug, 9 = trace
                (push *standard-output* streams))
              (values-list streams)
              ))
          ))
  (format t "opened log file ~s~%" *log-file-stream*)

  ;; (uiop:delete-file-if-exists "cl-emacs.log")
  ;; (define-pipe ()
  ;;   (category-tree-filter :name 'repl-category-filter)
  ;;   (file-faucet :file "cl-emacs.log")
  ;;   )
  ;; (setf (repl-level) :info)

  ;; ;; force disable ansi colors in repl
  ;; (loop for pipeline across (org.shirakumo.verbose::pipeline *global-controller*)
  ;;       do (loop for element across pipeline
  ;;                when (typep element 'repl-faucet)
  ;;                  do (setf (ansi-colors element) nil)))
  ;; (setf *timestamp-format* NIL)
  )
;; log everything to file, non-debug to repl


(defparameter *default-loglevel* :emerg)
(vom:config t *default-loglevel*)

(defun log-enable (pkg &optional (level :debug))
  (vom:config pkg level))
(defun log-disable (pkg)
  (vom:config pkg *default-loglevel*)
  )

(defmacro log-debug1 (format &rest args)
  `(let ((*print-circle* t))
     (vom:debug1 ,format ,@args)))

(defmacro log-debug (format &rest args)
  `(let ((*print-circle* t))
     (vom:debug ,format ,@args)))

(defmacro log-error (format &rest args)
  `(let ((*print-circle* t))
     (vom:error ,format ,@args)))

(defmacro log-info (format &rest args)
  `(let ((*print-circle* t))
     (vom:info ,format ,@args)))

(log-reset)
(log-enable :cl-emacs/log)
(log-enable :common-lisp-user)

