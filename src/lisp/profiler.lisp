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

(uiop:define-package :cl-emacs/profiler
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/profiler)
(log-enable :cl-emacs/profiler :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* function-equal ()
  #M"Return non-nil if F1 and F2 come from the same source.
Used to determine if different closures are just different instances of
the same lambda expression, or are really unrelated function.

(fn F1 F2)"
  (error ’unimplemented-error))
(defun* profiler-cpu-log ()
  #M"Return the current cpu profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of time spent at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.
Before returning, a new log is allocated for future samples.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-cpu-running-p ()
  #M"Return non-nil if cpu profiler is running.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-cpu-start ()
  #M"Start or restart the cpu profiler.
It takes call-stack samples each SAMPLING-INTERVAL nanoseconds, approximately.
See also ‘profiler-log-size’ and ‘profiler-max-stack-depth’.

(fn SAMPLING-INTERVAL)"
  (error ’unimplemented-error))
(defun* profiler-cpu-stop ()
  #M"Stop the cpu profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-memory-log ()
  #M"Return the current memory profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of memory allocated at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.
Before returning, a new log is allocated for future samples.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-memory-running-p ()
  #M"Return non-nil if memory profiler is running.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-memory-start ()
  #M"Start/restart the memory profiler.
The memory profiler will take samples of the call-stack whenever a new
allocation takes place.  Note that most small allocations only trigger
the profiler occasionally.
See also ‘profiler-log-size’ and ‘profiler-max-stack-depth’.

(fn)"
  (error ’unimplemented-error))
(defun* profiler-memory-stop ()
  #M"Stop the memory profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.

(fn)"
  (error ’unimplemented-error))
