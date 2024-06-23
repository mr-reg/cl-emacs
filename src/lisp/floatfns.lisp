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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/floatfns
    (:use
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/data
     :cl-emacs/lib/commons)
  (:import-from #:cl
                #:abs
                #:acos
                #:asin
                #:atan
                #:ceiling
                #:cos
                #:exp
                #:expt
                #:fceiling
                #:ffloor
                #:float
                #:floor
                #:fround
                #:ftruncate
                #:log
                #:round
                #:sin
                #:sqrt
                #:tan
                #:truncate

                )
  (:export
   #:abs
   #:acos
   #:asin
   #:atan
   #:ceiling
   #:cos
   #:exp
   #:expt
   #:fceiling
   #:ffloor
   #:float
   #:floor
   #:fround
   #:ftruncate
   #:log
   #:round
   #:sin
   #:sqrt
   #:tan
   #:truncate
   )  )
(in-package :cl-emacs/floatfns)
(log-enable :cl-emacs/floatfns :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/floatfns)
(in-suite cl-emacs/floatfns)


(defun* copysign ()
  #M"Copy sign of X2 to value of X1, and return the result.
Cause an error if X1 or X2 is not a float.

(fn X1 X2)"
  (error 'unimplemented-error))

(defun* frexp ()
  #M"Get significand and exponent of a floating point number.
Breaks the floating point number X into its binary significand SGNFCAND
(a floating point value between 0.5 (included) and 1.0 (excluded))
and an integral exponent EXP for 2, such that:

  X = SGNFCAND * 2^EXP

The function returns the cons cell (SGNFCAND . EXP).
If X is zero, both parts (SGNFCAND and EXP) are zero.

(fn X)"
  (error 'unimplemented-error))




(defun* ldexp ()
  #M"Return SGNFCAND * 2**EXPONENT, as a floating point number.
EXPONENT must be an integer.

(fn SGNFCAND EXPONENT)"
  (error 'unimplemented-error))
(defun* logb ()
  #M"Returns largest integer <= the base 2 log of the magnitude of ARG.
This is the same as the exponent of a float.

(fn ARG)"
  (error 'unimplemented-error))

(defun test-me ()
  (run! 'cl-emacs/fns))

;; (in-package :cl-emacs/elisp)
;; (reexport-symbols :cl-emacs/floatfns)
