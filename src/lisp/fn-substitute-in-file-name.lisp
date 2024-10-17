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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/fn-substitute-in-file-name
    (:use
     :defstar
     :cl-emacs/lib/log
     :cl-emacs/fns
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     )
  (:export #:substitute-in-file-name)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    )
  )
(in-package :cl-emacs/fn-substitute-in-file-name)
(log-enable :cl-emacs/fn-substitute-in-file-name :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* substitute-in-file-name ((filename pstrings:pstring))
  #M"Substitute environment variables referred to in FILENAME.
     ‘$FOO' where FOO is an environment variable name means to substitute
     the value of that variable.  The variable name should be terminated
     with a character not a letter, digit or underscore; otherwise, enclose
     the entire variable name in braces.
     
     If FOO is not defined in the environment, ‘$FOO' is left unchanged in
     the value of this function.
     
     If ‘/~' appears, all of FILENAME through that ‘/' is discarded.
     If ‘//' appears, everything up to and including the first of
     those ‘/' is discarded."
  filename
  )

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/fn-substitute-in-file-name)
