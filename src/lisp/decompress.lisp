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

(uiop:define-package :cl-emacs/decompress
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/decompress)
(log-enable :cl-emacs/decompress :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* zlib-available-p ()
  #M"Return t if zlib decompression is available in this instance of Emacs.

(fn)"
  (error 'unimplemented-error))
(defun* zlib-decompress-region ()
  #M"Decompress a gzip- or zlib-compressed region.
Replace the text in the region by the decompressed data.

If optional parameter ALLOW-PARTIAL is nil or omitted, then on
failure, return nil and leave the data in place.  Otherwise, return
the number of bytes that were not decompressed and replace the region
text by whatever data was successfully decompressed (similar to gzip).
If decompression is completely successful return t.

This function can be called only in unibyte buffers.

(fn START END &optional ALLOW-PARTIAL)"
  (error 'unimplemented-error))
