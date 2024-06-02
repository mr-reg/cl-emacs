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

(uiop:define-package :cl-emacs/marker
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/marker)
(log-enable :cl-emacs/marker :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* copy-marker ()
  #M"Return a new marker pointing at the same place as MARKER.
If argument is a number, makes a new marker pointing
at that position in the current buffer.
If MARKER is not specified, the new marker does not point anywhere.
The optional argument TYPE specifies the insertion type of the new marker;
see ‘marker-insertion-type'.

(fn &optional MARKER TYPE)"
  (error 'unimplemented-error))
(defun* marker-buffer ()
  #M"Return the buffer that MARKER points into, or nil if none.
Returns nil if MARKER points into a dead buffer.

(fn MARKER)"
  (error 'unimplemented-error))
(defun* marker-insertion-type ()
  #M"Return insertion type of MARKER: t if it stays after inserted text.
The value nil means the marker stays before text inserted there.

(fn MARKER)"
  (error 'unimplemented-error))
(defun* marker-position ()
  #M"Return the position of MARKER, or nil if it points nowhere.

(fn MARKER)"
  (error 'unimplemented-error))
(defun* set-marker ()
  #M"Position MARKER before character number POSITION in BUFFER.
If BUFFER is omitted or nil, it defaults to the current buffer.  If
POSITION is nil, makes marker point nowhere so it no longer slows down
editing in any buffer.  Returns MARKER.

(fn MARKER POSITION &optional BUFFER)"
  (error 'unimplemented-error))
(defun* set-marker-insertion-type ()
  #M"Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it.

(fn MARKER TYPE)"
  (error 'unimplemented-error))
