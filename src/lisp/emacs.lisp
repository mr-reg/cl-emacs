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

(uiop:define-package :cl-emacs/emacs
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/emacs)
(log-enable :cl-emacs/emacs :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* daemon-initialized ()
  #M"Mark the Emacs daemon as being initialized.
This finishes the daemonization process by doing the other half of detaching
from the parent process and its tty file descriptors.

(fn)"
  (error 'unimplemented-error))
(defun* daemonp ()
  #M"Return non-nil if the current emacs process is a daemon.
If the daemon was given a name argument, return that name.

(fn)"
  (error 'unimplemented-error))
(defun* invocation-directory ()
  #M"Return the directory name in which the Emacs executable was located.

(fn)"
  (error 'unimplemented-error))
(defun* invocation-name ()
  #M"Return the program name that was used to run Emacs.
Any directory names are omitted.

(fn)"
  (error 'unimplemented-error))
(defun* kill-emacs ()
  #M"Exit the Emacs job and kill it.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.
Any other value of ARG, or ARG omitted, means return an
exit code that indicates successful program termination.

If RESTART is non-nil, instead of just exiting at the end, start a new
Emacs process, using the same command line arguments as the currently
running Emacs process.

This function is called upon receipt of the signals SIGTERM
or SIGHUP, and upon SIGINT in batch mode.

The value of ‘kill-emacs-hook', if not void, is a list of functions
(of no args), all of which are called before Emacs is actually
killed.

(fn &optional ARG RESTART)"
  (error 'unimplemented-error))
