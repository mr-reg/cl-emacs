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

(uiop:define-package :cl-emacs/floatfns
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/floatfns)
(log-enable :cl-emacs/floatfns :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* abs () "Return the absolute value of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* acos () "Return the inverse cosine of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* asin () "Return the inverse sine of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* atan () "Return the inverse tangent of the arguments.
If only one argument Y is given, return the inverse tangent of Y.
If two arguments Y and X are given, return the inverse tangent of Y
divided by X, i.e. the angle in radians between the vector (X, Y)
and the x-axis.

(fn Y &optional X)"
  (error ’unimplemented-error))
(defun* ceiling () "Return the smallest integer no less than ARG.
This rounds the value towards +inf.
With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR.

(fn ARG &optional DIVISOR)"
  (error ’unimplemented-error))
(defun* copysign () "Copy sign of X2 to value of X1, and return the result.
Cause an error if X1 or X2 is not a float.

(fn X1 X2)"
  (error ’unimplemented-error))
(defun* cos () "Return the cosine of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* exp () "Return the exponential base e of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* expt () "Return the exponential ARG1 ** ARG2.

(fn ARG1 ARG2)"
  (error ’unimplemented-error))
(defun* fceiling () "Return the smallest integer no less than ARG, as a float.
(Round toward +inf.)

(fn ARG)"
  (error ’unimplemented-error))
(defun* ffloor () "Return the largest integer no greater than ARG, as a float.
(Round toward -inf.)

(fn ARG)"
  (error ’unimplemented-error))
(defun* float () "Return the floating point number equal to ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* floor () "Return the largest integer no greater than ARG.
This rounds the value towards -inf.
With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.

(fn ARG &optional DIVISOR)"
  (error ’unimplemented-error))
(defun* frexp () "Get significand and exponent of a floating point number.
Breaks the floating point number X into its binary significand SGNFCAND
(a floating point value between 0.5 (included) and 1.0 (excluded))
and an integral exponent EXP for 2, such that:

  X = SGNFCAND * 2^EXP

The function returns the cons cell (SGNFCAND . EXP).
If X is zero, both parts (SGNFCAND and EXP) are zero.

(fn X)"
  (error ’unimplemented-error))
(defun* fround () "Return the nearest integer to ARG, as a float.

(fn ARG)"
  (error ’unimplemented-error))
(defun* ftruncate () "Truncate a floating point number to an integral float value.
(Round toward zero.)

(fn ARG)"
  (error ’unimplemented-error))
(defun* isnan () "Return non-nil if argument X is a NaN.

(fn X)"
  (error ’unimplemented-error))
(defun* ldexp () "Return SGNFCAND * 2**EXPONENT, as a floating point number.
EXPONENT must be an integer.

(fn SGNFCAND EXPONENT)"
  (error ’unimplemented-error))
(defun* log () "Return the natural logarithm of ARG.
If the optional argument BASE is given, return log ARG using that base.

(fn ARG &optional BASE)"
  (error ’unimplemented-error))
(defun* logb () "Returns largest integer <= the base 2 log of the magnitude of ARG.
This is the same as the exponent of a float.

(fn ARG)"
  (error ’unimplemented-error))
(defun* round () "Return the nearest integer to ARG.
With optional DIVISOR, return the nearest integer to ARG/DIVISOR.

Rounding a value equidistant between two integers may choose the
integer closer to zero, or it may prefer an even integer, depending on
your machine.  For example, (round 2.5) can return 3 on some
systems, but 2 on others.

(fn ARG &optional DIVISOR)"
  (error ’unimplemented-error))
(defun* sin () "Return the sine of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* sqrt () "Return the square root of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* tan () "Return the tangent of ARG.

(fn ARG)"
  (error ’unimplemented-error))
(defun* truncate () "Truncate a floating point number to an int.
Rounds ARG toward zero.
With optional DIVISOR, truncate ARG/DIVISOR.

(fn ARG &optional DIVISOR)"
  (error ’unimplemented-error))
