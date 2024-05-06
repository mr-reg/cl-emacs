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
(uiop:define-package :cl-emacs/elisp/editfns
    (:use :common-lisp :alexandria :cl-emacs/log
     :cl-emacs/elisp/internals)
  )
(in-package :cl-emacs/elisp/editfns)
(log-enable :cl-emacs/elisp/editfns)
(defun-elisp elisp/styled-format '(:internal :c-native) (message &rest arg/args)
  "Implement ‘format-message’ if MESSAGE is true, ‘format’ otherwise.
args: (message format args)"
  (declare (ignore message))
  (let ((format-str (car arg/args))
        (raw-args (cdr arg/args))
        new-args new-format)
    ;; (log-info "~s ~s" message arg/args)
    (setq new-format (with-output-to-string (stream)
                       (let ((position 0) arg)
                         (cl-ppcre:do-matches (start end "%.*?[sSoxXcefgd%]|~" format-str)
                           (setq arg (car raw-args))
                           (when (symbolp arg)
                             (setq arg (elisp-symbol-to-string arg)))
                           (when (> start position)
                             (loop for idx from position below start
                                   do (write-char (aref format-str idx) stream)))
                           (let ((expr (subseq format-str start end)))
                             (cond
                               ((string= expr "~")
                                (format stream "~~~~"))
                               ((string= expr "%%")
                                (format stream "%"))
                               ((string= expr "%s")
                                (format stream "~~a"))
                               ((cl-ppcre:scan "^%.*f$" expr)
                                (format stream "~~~a" (str:replace-all "." "," (subseq expr 1)))
                                )
                               ((str:starts-with-p "%" expr)
                                (when (string= expr "%c")
                                  (setq arg (code-char arg)))
                                (format stream "~~~a" (subseq expr 1)))
                               (t (format stream "~a" expr))))
                           (push arg new-args)
                           (setq raw-args (cdr raw-args))
                           ;; (log-info "match ~a ~a" start end)
                           (setq position end)
                           )
                         (loop for idx from position below (length format-str)
                               do (write-char (aref format-str idx) stream)))))
    (setq new-args (nreverse new-args))
    ;; (log-info "~s ~s" new-format new-args)
    (let ((*print-pretty* nil))
      (apply 'format (append (list 'nil new-format) new-args)))))

