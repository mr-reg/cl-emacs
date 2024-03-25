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
(in-package :cl-emacs/elisp)

;; (CL-EMACS/ELISP:EXPAND-FILE-NAME "zg.zip:g.gz" NIL)
(export 'expand-file-name)
(defun expand-file-name (arg/name &optional arg/default-directory)
  "Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
\(does not start with slash or tilde); both the directory name and
a directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or
missing, the current buffer's value of `default-directory' is used.
NAME should be a string that is a valid file name for the underlying
filesystem.

File name components that are `.' are removed, and so are file name
components followed by `..', along with the `..' itself; note that
these simplifications are done without checking the resulting file
names in the file system.

Multiple consecutive slashes are collapsed into a single slash, except
at the beginning of the file name when they are significant (e.g., UNC
file names on MS-Windows.)

An initial \"~\" in NAME expands to your home directory.
"
  (let* ((buffer-default-directory (assoc-value (assoc-value *context* :buffer) :default-directory))
         (invocation-directory (assoc-value *context* :invocation-directory))
         (default-directory-final (uiop:make-pathname*
                                   :directory
                                   (cond
                                     ((and arg/default-directory (not (str:ends-with-p "/" arg/default-directory)))
                                      (concatenate 'string arg/default-directory "/"))
                                     (t (or arg/default-directory
                                            buffer-default-directory
                                            invocation-directory
                                            "/"
                                            )))))
         (path (uiop:merge-pathnames* arg/name default-directory-final))
         absolute-namestring)
    (handler-case (setq absolute-namestring (namestring (truename path)))
      (file-error ()
        (setq absolute-namestring (namestring path))))
    (when (and (str:ends-with-p "/" absolute-namestring)
               (not (str:ends-with-p "/"  arg/name)))
      (setq absolute-namestring (str:substring 0 -1 absolute-namestring)))
    (when (str:starts-with-p "~/" absolute-namestring)
      (setq absolute-namestring (concatenate 'string
                                             (namestring (user-homedir-pathname))
                                             (str:substring 2 (length absolute-namestring) absolute-namestring))))
    absolute-namestring))

;; (export 'test-rpc)
;; (defun test-rpc (x &optional y)
;;   "my docstring"
;;   (+ x (or y 4)))

