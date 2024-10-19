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

(uiop:define-package :cl-emacs/xml
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/xml)
(log-enable :cl-emacs/xml :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* libxml-available-p ()
  #M"Return t if libxml2 support is available in this instance of Emacs.

(fn)"
  (error 'unimplemented-error))
(defun* libxml-parse-html-region ()
  #M"Parse the region as an HTML document and return the parse tree.
If START is nil, it defaults to ‘point-min'.  If END is nil, it
defaults to ‘point-max'.

If BASE-URL is non-nil, it is used if and when reporting errors and
warnings from the underlying libxml2 library.  Currently, errors and
warnings from the library are suppressed, so this argument is largely
ignored.

If you want comments to be stripped, use the ‘xml-remove-comments'
function to strip comments before calling this function.

(fn &optional START END BASE-URL DISCARD-COMMENTS)"
  (error 'unimplemented-error))
(defun* libxml-parse-xml-region ()
  #M"Parse the region as an XML document and return the parse tree.
If START is nil, it defaults to ‘point-min'.  If END is nil, it
defaults to ‘point-max'.

If BASE-URL is non-nil, it is used if and when reporting errors and
warnings from the underlying libxml2 library.  Currently, errors and
warnings from the library are suppressed, so this argument is largely
ignored.

If you want comments to be stripped, use the ‘xml-remove-comments'
function to strip comments before calling this function.

(fn &optional START END BASE-URL DISCARD-COMMENTS)"
  (error 'unimplemented-error))
