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

(export 'expand-file-name)
(defun expand-file-name (name &optional default-directory)
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

An initial \"~USER\" in NAME expands to USER's home directory.  If
USER doesn't exist, \"~USER\" is not expanded.

To do other file name substitutions, see `substitute-in-file-name'.

For technical reasons, this function can return correct but
non-intuitive results for the root directory; for instance,
\(expand-file-name \"..\" \"/\") returns \"/..\".  For this reason, use
\(directory-file-name (file-name-directory dirname)) to traverse a
filesystem tree, not (expand-file-name \"..\" dirname).  Note: make
sure DIRNAME in this example doesn't end in a slash, unless it's
the root directory. "
  (let* ((buffer-default-directory (assoc-value (assoc-value *context* :buffer) :default-directory))
         (invocation-directory (assoc-value *context* :invocation-directory))
         (default-directory-final (cond
                                    ((and default-directory (not (str:ends-with-p "/" default-directory)))
                                     (concatenate 'string default-directory "/"))
                                    (t (or default-directory
                                           buffer-default-directory
                                           invocation-directory
                                           "/"
                                           ))))
         (path (uiop:merge-pathnames* name default-directory-final)))
    (namestring path)))

(export 'test-rpc)
(defun test-rpc (x &optional y)
  "my docstring"
  (+ x (or y 4)))

