(add-to-list 'load-path "~/.emacs.d/straight/repos/f.el/")
(add-to-list 'load-path "~/.emacs.d/straight/repos/s.el/")
(add-to-list 'load-path "~/.emacs.d/straight/repos/dash.el/")
(require 'f)
(setq float-output-format "%.6f")
;; (setq float-output-format )
;; (print 2.105)
;; (print 0.0001)
;; (print 2.0)
(defun make-read-test-data (filename)
  (let ((contents (f-read-text filename))
        (output-filename (concat (replace-regexp-in-string "[/~]" "_" filename) ".result"))
        (position 0)
        body)
    (f-write "" 'utf-8 output-filename)
    (message output-filename)
    (ignore-errors
      (cl-loop
       (let ((expr (read-from-string contents position)))
         (setq position (cdr expr))
         (setq expr (car expr))
         (setq body (with-output-to-string
                      (print expr)))

         )))
    (f-append
     body
     'utf-8 output-filename)
    ))
(defun recurse-process-dir ()
  (let ((dir-name "~/github/emacs/"))
    (f-files dir-name
             (lambda (name)
               (when (string-suffix-p ".el" name t)
                 (make-read-test-data name)
                 (message name))
               )
             t)
    nil
    ))
;; (recurse-process-dir)
;; (make-read-test-date "~/github/emacs/lisp/master.el")

(defun read-princ-el-file (filename output-filename)
  (let ((contents (f-read-text filename))
        (position 0)
        body)
    (setq body (with-output-to-string
                 (ignore-errors
                   (while t
                     (let ((expr (read-from-string contents position)))
                       (setq position (cdr expr))
                       (setq expr (car expr))
                       (princ expr)
                       (princ (format "%c" 10))
                       )))))
    ;; (princ (format "writing to %s" output-filename) )
    (princ body)
    ;; (f-write body 'utf-8 output-filename)
    ))
;; (read-princ-el-file "/root/github/emacs/lisp/calc/calc-ext.el")
;; (prin)
;; (read-princ-el-file "/data/data/com.termux/files/home/github/emacs/.dir-locals.el")
;; (princ "\353")
;; (princ (terminal-coding-system))
;; (set-terminal-coding-system)

;; (defun to-multibyte (obj)
;;   (cond
;;    ((consp obj) (cons (to-multibyte (car obj)) (to-multibyte (cdr obj))))
;;    ((multibyte-string-p obj) obj)
;;    ((stringp obj) (string-to-multibyte obj))
;;    (t (error "unsupported object type %s" (type-of obj)))))
;; #o360
;; (princ (string-to-multibyte "\300"))
;; (print (to-multibyte "\360"))
