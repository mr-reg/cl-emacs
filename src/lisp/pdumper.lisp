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

(uiop:define-package :cl-emacs/pdumper
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/pdumper)
(log-enable :cl-emacs/pdumper :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* dump-emacs-portable () "Dump current state of Emacs into dump file FILENAME.
If TRACK-REFERRERS is non-nil, keep additional debugging information
that can help track down the provenance of unsupported object
types.

(fn FILENAME &optional TRACK-REFERRERS)"
  (error ’unimplemented-error))
(defun* dump-emacs-portable--sort-predicate () "Internal relocation sorting function.

(fn A B)"
  (error ’unimplemented-error))
(defun* dump-emacs-portable--sort-predicate-copied () "Internal relocation sorting function.

(fn A B)"
  (error ’unimplemented-error))
(defun* pdumper-stats () "Return statistics about portable dumping used by this session.
If this Emacs session was started from a dump file,
the return value is an alist of the form:

  ((dumped-with-pdumper . t) (load-time . TIME) (dump-file-name . FILE))

where TIME is the time in seconds it took to restore Emacs state
from the dump file, and FILE is the name of the dump file.
Value is nil if this session was not started using a dump file.

(fn)"
  (error ’unimplemented-error))
