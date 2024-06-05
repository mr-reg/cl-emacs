(uiop:define-package :cl-emacs/log
    (:use :common-lisp)
  (:shadow #:log #:debug #:trace #:error #:warn)
  (:export #:log-debug
           #:log-error
           #:log-info
           #:log-debug1
           #:log-debug2
           #:log-enable
           #:log-disable
           #:log-reset
           #:*loglevel-file*
           #:*loglevel-stdout*
           )
  )
(in-package :cl-emacs/log)

(defparameter *log-filename* "cl-emacs.log")
(defvar *log-file-stream* nil)
;; 4 = error, 7 = info, 8 = debug, 9 = debug1, 10 = debug2
(defparameter *loglevel-file* 10)
(defparameter *loglevel-stdout* 8)

(defmacro get-max-loglevel ()
  '(vom::find-package-level *package*)
  ;; (max *loglevel-file* *loglevel-stdout*)
  )
;; (setq *log-file-stream* nil)
;; (defvar *log-config-lock* (bt:make-lock "log-config-hook"))
;; (setq *real-log-file-stream* nil)
(defun file-log-syncer ()
  (format t "file log syncer started")
  (with-open-file (file *log-filename* :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :sharing :lock)
    (loop
      for char = (read-char-no-hang *log-file-stream*)
      with last-sync = 0
      do (if char
             (progn
               (write-char char file)
               (incf last-sync)
               (when (> last-sync 64000)
                 (setq last-sync 0)
                 (finish-output file)))
             (progn
               (finish-output file)
               (write-char (read-char *log-file-stream*) file))))))
(defvar *log-sync-process* nil)
(defun log-reset ()
  (unless *log-file-stream*
    (setq *log-file-stream* (cl-plumbing:make-pipe)))
  (when *log-sync-process*
    (bt:destroy-thread *log-sync-process*))
  (setq *log-sync-process* (bt:make-thread #'file-log-syncer :name "file-log-syncer"))
  (setf vom:*log-hook*
        (lambda (level package package-level)
          (declare (fixnum level))
          (declare (ignore package package-level))
          (progn ;; bt:with-lock-held (*log-config-lock*)
            (let (streams)
              (when (and *log-file-stream*
                         (<= level *loglevel-file*))
                (push *log-file-stream* streams))
              (when (<= level *loglevel-stdout*)
                (push *standard-output* streams))
              (values-list streams)
              )
            )
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

(defmacro log-debug2 (format &rest args)
  `(when (>= ,(get-max-loglevel) 10)
     (let ((*print-circle* t))
       (vom:debug2 ,format ,@args))))
(defmacro log-debug1 (format &rest args)
  `(when (>= ,(get-max-loglevel) 9)
     (let ((*print-circle* t))
       (vom:debug1 ,format ,@args))))

(defmacro log-debug (format &rest args)
  `(when (>= ,(get-max-loglevel) 8)
     (let ((*print-circle* t))
       (vom:debug ,format ,@args))))

(defmacro log-error (format &rest args)
  `(when (>= ,(get-max-loglevel) 4)
     (let ((*print-circle* t))
       (vom:error ,format ,@args))))

(defmacro log-info (format &rest args)
  `(when (>= ,(get-max-loglevel) 7)
     (let ((*print-circle* t))
       (vom:info ,format ,@args))))

(log-reset)
(log-enable :cl-emacs/log)
(log-enable :common-lisp-user :debug1)
