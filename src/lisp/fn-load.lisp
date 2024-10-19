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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/fn-load
    (:use
     :defstar
     :cl-emacs/lib/log
     :cl-emacs/fns
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     )
  
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    )
  )
(in-package :cl-emacs/fn-load)
(log-enable :cl-emacs/fn-load :debug2)
(named-readtables:in-readtable elisp-function-syntax)

(cl:defparameter load-suffixes (list (pstrings:build-pstring "el")))
(cl:defparameter load-path (list (pstrings:build-pstring (cl:namestring (uiop/os:getcwd)))))

(defun* load (file &optional noerror nomessage nosuffix must-suffix)
  #M"Execute a file of Lisp code named FILE.
     First try ' try file with ‘.el', then try
     with a system-dependent suffix of dynamic modules (see ‘load-suffixes'),
     then try FILE unmodified (the exact suffixes in the exact order are
     determined by ‘load-suffixes').  Environment variable references in
     FILE are replaced with their values by calling ‘substitute-in-file-name'.
     This function searches the directories in ‘load-path'.
     
     If optional second arg NOERROR is non-nil,
     report no error if FILE doesn't exist.
     Print messages at start and end of loading unless
     optional third arg NOMESSAGE is non-nil (but ‘force-load-messages'
     overrides that).
     If optional fourth arg NOSUFFIX is non-nil, don't try adding
     suffixes to the specified name FILE.
     If optional fifth arg MUST-SUFFIX is non-nil, insist on
     the suffix ‘.elc' or ‘.el' or the module suffix; don't accept just
     FILE unless it ends in one of those suffixes or includes a directory name.
     
     If NOSUFFIX is nil, then if a file could not be found, try looking for
     a different representation of the file by adding non-empty suffixes to
     its name, before trying another file.  Emacs uses this feature to find
     compressed versions of files when Auto Compression mode is enabled.
     If NOSUFFIX is non-nil, disable this feature.
     
     The suffixes that this function tries out, when NOSUFFIX is nil, are
     given by the return value of ‘get-load-suffixes' and the values listed
     in ‘load-file-rep-suffixes'.  If MUST-SUFFIX is non-nil, only the
     return value of ‘get-load-suffixes' is used, i.e. the file name is
     required to have a non-empty suffix.
     
     When searching suffixes, this function normally stops at the first
     one that exists.  If the option ‘load-prefer-newer' is non-nil,
     however, it tries all suffixes, and uses whichever file is the newest.
     
     Loading a file records its definitions, and its ‘provide' and
     ‘require' calls, in an element of ‘load-history' whose
     car is the file name loaded.  See ‘load-history'.
     
     While the file is in the process of being loaded, the variable
     ‘load-in-progress' is non-nil and the variable ‘load-file-name'
     is bound to the file's name.
     
     Return t if the file exists and loads successfully."
  (error 'unimplemented-error))
