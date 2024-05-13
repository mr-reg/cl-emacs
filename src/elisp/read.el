(add-to-list 'load-path "~/.emacs.d/straight/repos/f.el/")
(add-to-list 'load-path "~/.emacs.d/straight/repos/s.el/")
(add-to-list 'load-path "~/.emacs.d/straight/repos/dash.el/")
(require 'f)

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
(recurse-process-dir)
;; (make-read-test-date "~/github/emacs/lisp/master.el")


