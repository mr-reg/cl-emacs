;; Copyright (C) 2024 by Gleb Borodulia
;; Author: Gleb Borodulia <mr.reg@mail.ru>

;; This file is part of cl-emacs.

;; cl-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; cl-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.

(uiop:define-package :cl-emacs/filelock
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/filelock)
(log-enable :cl-emacs/filelock :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* file-locked-p ()
  #M"Return a value indicating whether FILENAME is locked.
The value is nil if the FILENAME is not locked,
t if it is locked by you, else a string saying which user has locked it.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* lock-buffer ()
  #M"Lock FILE, if current buffer is modified.
FILE defaults to current buffer's visited file,
or else nothing is done if current buffer isn't visiting a file.

If the option ‘create-lockfiles' is nil, this does nothing.

(fn &optional FILE)"
  (error 'unimplemented-error))
(defun* lock-file ()
  #M"Lock FILE.
If the option ‘create-lockfiles' is nil, this does nothing.

(fn FILE)"
  (error 'unimplemented-error))
(defun* unlock-buffer ()
  #M"Unlock the file visited in the current buffer.
If the buffer is not modified, this does nothing because the file
should not be locked in that case.  It also does nothing if the
current buffer is not visiting a file, or is not locked.  Handles file
system errors by calling ‘display-warning' and continuing as if the
error did not occur.

(fn)"
  (error 'unimplemented-error))
(defun* unlock-file ()
  #M"Unlock FILE.

(fn FILE)"
  (error 'unimplemented-error))
