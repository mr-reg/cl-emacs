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

(uiop:define-package :cl-emacs/data
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/data)
(log-enable :cl-emacs/data :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* * () "Return product of any number of arguments, which are numbers or markers.

(fn &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* + () "Return sum of any number of arguments, which are numbers or markers.

(fn &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* - () "Negate number or subtract numbers or markers and return the result.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.

(fn &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* / () "Divide number by divisors and return the result.
With two or more arguments, return first argument divided by the rest.
With one argument, return 1 divided by the argument.
The arguments must be numbers or markers.

(fn NUMBER &rest DIVISORS)"
  (error ’unimplemented-error))
(defun* /= () "Return t if first arg is not equal to second arg.  Both must be numbers or markers.

(fn NUM1 NUM2)"
  (error ’unimplemented-error))
(defun* 1+ () "Return NUMBER plus one.  NUMBER may be a number or a marker.
Markers are converted to integers.

(fn NUMBER)"
  (error ’unimplemented-error))
(defun* 1- () "Return NUMBER minus one.  NUMBER may be a number or a marker.
Markers are converted to integers.

(fn NUMBER)"
  (error ’unimplemented-error))
(defun* < () "Return t if each arg (a number or marker), is less than the next arg.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* <= () "Return t if each arg (a number or marker) is less than or equal to the next.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* = () "Return t if args, all numbers or markers, are equal.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* > () "Return t if each arg (a number or marker) is greater than the next arg.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* >= () "Return t if each arg (a number or marker) is greater than or equal to the next.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* add-variable-watcher () "Cause WATCH-FUNCTION to be called when SYMBOL is about to be set.

It will be called with 4 arguments: (SYMBOL NEWVAL OPERATION WHERE).
SYMBOL is the variable being changed.
NEWVAL is the value it will be changed to.  (The variable still has
the old value when WATCH-FUNCTION is called.)
OPERATION is a symbol representing the kind of change, one of: ‘set’,
‘let’, ‘unlet’, ‘makunbound’, and ‘defvaralias’.
WHERE is a buffer if the buffer-local value of the variable is being
changed, nil otherwise.

All writes to aliases of SYMBOL will call WATCH-FUNCTION too.

(fn SYMBOL WATCH-FUNCTION)"
  (error ’unimplemented-error))
(defun* aref () "Return the element of ARRAY at index IDX.
ARRAY may be a vector, a string, a char-table, a bool-vector, a record,
or a byte-code object.  IDX starts at 0.

(fn ARRAY IDX)"
  (error ’unimplemented-error))
(defun* arrayp () "Return t if OBJECT is an array (string or vector).

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* aset () "Store into the element of ARRAY at index IDX the value NEWELT.
Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
bool-vector.  IDX starts at 0.

(fn ARRAY IDX NEWELT)"
  (error ’unimplemented-error))
(defun* ash () "Return integer VALUE with its bits shifted left by COUNT bit positions.
If COUNT is negative, shift VALUE to the right instead.
VALUE and COUNT must be integers.
Mathematically, the return value is VALUE multiplied by 2 to the
power of COUNT, rounded down.  If the result is non-zero, its sign
is the same as that of VALUE.
In terms of bits, when COUNT is positive, the function moves
the bits of VALUE to the left, adding zero bits on the right; when
COUNT is negative, it moves the bits of VALUE to the right,
discarding bits.

(fn VALUE COUNT)"
  (error ’unimplemented-error))
(defun* atom () "Return t if OBJECT is not a cons cell.  This includes nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* bare-symbol () "Extract, if need be, the bare symbol from SYM, a symbol.

(fn SYM)"
  (error ’unimplemented-error))
(defun* bare-symbol-p () "Return t if OBJECT is a symbol, but not a symbol together with position.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* bool-vector-count-consecutive () "Count how many consecutive elements in A equal B starting at I.
A is a bool vector, B is t or nil, and I is an index into A.

(fn A B I)"
  (error ’unimplemented-error))
(defun* bool-vector-count-population () "Count how many elements in A are t.
A is a bool vector.  To count A’s nil elements, subtract the return
value from A’s length.

(fn A)"
  (error ’unimplemented-error))
(defun* bool-vector-exclusive-or () "Return A ^ B, bitwise exclusive or.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.

(fn A B &optional C)"
  (error ’unimplemented-error))
(defun* bool-vector-intersection () "Return A & B, bitwise and.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.

(fn A B &optional C)"
  (error ’unimplemented-error))
(defun* bool-vector-not () "Compute ~A, set complement.
If optional second argument B is given, store result into B.
A and B must be bool vectors of the same length.
Return the destination vector.

(fn A &optional B)"
  (error ’unimplemented-error))
(defun* bool-vector-p () "Return t if OBJECT is a bool-vector.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* bool-vector-set-difference () "Return A &~ B, set difference.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.

(fn A B &optional C)"
  (error ’unimplemented-error))
(defun* bool-vector-subsetp () "Return t if every t value in A is also t in B, nil otherwise.
A and B must be bool vectors of the same length.

(fn A B)"
  (error ’unimplemented-error))
(defun* bool-vector-union () "Return A | B, bitwise or.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.

(fn A B &optional C)"
  (error ’unimplemented-error))
(defun* boundp () "Return t if SYMBOL’s value is not void.
Note that if ‘lexical-binding’ is in effect, this refers to the
global value outside of any lexical scope.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* bufferp () "Return t if OBJECT is an editor buffer.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* byte-code-function-p () "Return t if OBJECT is a byte-compiled function object.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* byteorder () "Return the byteorder for the machine.
Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
lowercase l) for small endian machines.

(fn)"
  (error ’unimplemented-error))
(defun* car () "Return the car of LIST.  If LIST is nil, return nil.
Error if LIST is not nil and not a cons cell.  See also ‘car-safe’.

See Info node ‘(elisp)Cons Cells’ for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.

(fn LIST)"
  (error ’unimplemented-error))
(defun* car-safe () "Return the car of OBJECT if it is a cons cell, or else nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* cdr () "Return the cdr of LIST.  If LIST is nil, return nil.
Error if LIST is not nil and not a cons cell.  See also ‘cdr-safe’.

See Info node ‘(elisp)Cons Cells’ for a discussion of related basic
Lisp concepts such as cdr, car, cons cell and list.

(fn LIST)"
  (error ’unimplemented-error))
(defun* cdr-safe () "Return the cdr of OBJECT if it is a cons cell, or else nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* char-or-string-p () "Return t if OBJECT is a character or a string.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* char-table-p () "Return t if OBJECT is a char-table.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* command-modes () "Return the modes COMMAND is defined for.
If COMMAND is not a command, the return value is nil.
The value, if non-nil, is a list of mode name symbols.

(fn COMMAND)"
  (error ’unimplemented-error))
(defun* condition-variable-p () "Return t if OBJECT is a condition variable.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* consp () "Return t if OBJECT is a cons cell.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* defalias () "Set SYMBOL’s function definition to DEFINITION.
Associates the function with the current load file, if any.
The optional third argument DOCSTRING specifies the documentation string
for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
determined by DEFINITION.

Internally, this normally uses ‘fset’, but if SYMBOL has a
‘defalias-fset-function’ property, the associated value is used instead.

The return value is undefined.

(fn SYMBOL DEFINITION &optional DOCSTRING)"
  (error ’unimplemented-error))
(defun* default-boundp () "Return t if SYMBOL has a non-void default value.
A variable may have a buffer-local value.  This function says whether
the variable has a non-void value outside of the current buffer
context.  Also see ‘default-value’.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* default-value () "Return SYMBOL’s default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  The default value is meaningful for variables with
local bindings in certain buffers.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* eq () "Return t if the two args are the same Lisp object.

(fn OBJ1 OBJ2)"
  (error ’unimplemented-error))
(defun* fboundp () "Return t if SYMBOL’s function definition is not void.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* floatp () "Return t if OBJECT is a floating point number.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* fmakunbound () "Make SYMBOL’s function definition be void.
Return SYMBOL.

If a function definition is void, trying to call a function by that
name will cause a ‘void-function’ error.  For more details, see Info
node ‘(elisp) Function Cells’.

See also ‘makunbound’.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* fset () "Set SYMBOL’s function definition to DEFINITION, and return DEFINITION.

(fn SYMBOL DEFINITION)"
  (error ’unimplemented-error))
(defun* get-variable-watchers () "Return a list of SYMBOL’s active watchers.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* indirect-function () "Return the function at the end of OBJECT’s function chain.
If OBJECT is not a symbol, just return it.  Otherwise, follow all
function indirections to find the final function binding and return it.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.

(fn OBJECT &optional NOERROR)"
  (error ’unimplemented-error))
(defun* indirect-variable () "Return the variable at the end of OBJECT’s variable chain.
If OBJECT is a symbol, follow its variable indirections (if any), and
return the variable at the end of the chain of aliases.  See Info node
‘(elisp)Variable Aliases’.

If OBJECT is not a symbol, just return it.  If there is a loop in the
chain of aliases, signal a ‘cyclic-variable-indirection’ error.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* integer-or-marker-p () "Return t if OBJECT is an integer or a marker (editor pointer).

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* integerp () "Return t if OBJECT is an integer.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* interactive-form () "Return the interactive form of CMD or nil if none.
If CMD is not a command, the return value is nil.
Value, if non-nil, is a list (interactive SPEC).

(fn CMD)"
  (error ’unimplemented-error))
(defun* keywordp () "Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with ‘:’
interned in the initial obarray.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* kill-local-variable () "Make VARIABLE no longer have a separate value in the current buffer.
From now on the default value will apply in this buffer.  Return VARIABLE.

(fn VARIABLE)"
  (error ’unimplemented-error))
(defun* listp () "Return t if OBJECT is a list, that is, a cons cell or nil.
Otherwise, return nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* local-variable-if-set-p () "Non-nil if VARIABLE is local in buffer BUFFER when set there.
BUFFER defaults to the current buffer.

More precisely, return non-nil if either VARIABLE already has a local
value in BUFFER, or if VARIABLE is automatically buffer-local (see
‘make-variable-buffer-local’).

(fn VARIABLE &optional BUFFER)"
  (error ’unimplemented-error))
(defun* local-variable-p () "Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer.

Also see ‘buffer-local-boundp’.

(fn VARIABLE &optional BUFFER)"
  (error ’unimplemented-error))
(defun* logand () "Return bitwise-and of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* logcount () "Return population count of VALUE.
This is the number of one bits in the two’s complement representation
of VALUE.  If VALUE is negative, return the number of zero bits in the
representation.

(fn VALUE)"
  (error ’unimplemented-error))
(defun* logior () "Return bitwise-or of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* lognot () "Return the bitwise complement of NUMBER.  NUMBER must be an integer.

(fn NUMBER)"
  (error ’unimplemented-error))
(defun* logxor () "Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* make-local-variable () "Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.)
Return VARIABLE.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

This function returns VARIABLE, and therefore
  (set (make-local-variable \\=’VARIABLE) VALUE-EXP)
works.

See also ‘make-variable-buffer-local’.

Do not use ‘make-local-variable’ to make a hook variable buffer-local.
Instead, use ‘add-hook’ and specify t for the LOCAL argument.

(fn VARIABLE)"
  (error ’unimplemented-error))
(defun* make-variable-buffer-local () "Make VARIABLE become buffer-local whenever it is set.
At any time, the value for the current buffer is in effect,
unless the variable has never been set in this buffer,
in which case the default value is in effect.
Note that binding the variable with ‘let’, or setting it while
a ‘let’-style binding made in this buffer is in effect,
does not make the variable buffer-local.  Return VARIABLE.

This globally affects all uses of this variable, so it belongs together with
the variable declaration, rather than with its uses (if you just want to make
a variable local to the current buffer for one particular use, use
‘make-local-variable’).  Buffer-local bindings are normally cleared
while setting up a new major mode, unless they have a ‘permanent-local’
property.

The function ‘default-value’ gets the default value and ‘set-default’ sets it.

See also ‘defvar-local’.

(fn VARIABLE)"
  (error ’unimplemented-error))
(defun* makunbound () "Empty out the value cell of SYMBOL, making it void as a variable.
Return SYMBOL.

If a variable is void, trying to evaluate the variable signals a
‘void-variable’ error, instead of returning a value.  For more
details, see Info node ‘(elisp) Void Variables’.

See also ‘fmakunbound’.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* markerp () "Return t if OBJECT is a marker (editor pointer).

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* max () "Return largest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* min () "Return smallest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)"
  (error ’unimplemented-error))
(defun* mod () "Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.

(fn X Y)"
  (error ’unimplemented-error))
(defun* module-function-p () "Return t if OBJECT is a function loaded from a dynamic module.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* multibyte-string-p () "Return t if OBJECT is a multibyte string.
Return nil if OBJECT is either a unibyte string, or not a string.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* mutexp () "Return t if OBJECT is a mutex.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* natnump () "Return t if OBJECT is a nonnegative integer.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* nlistp () "Return t if OBJECT is not a list.  Lists include nil.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* null () "Return t if OBJECT is nil, and return nil otherwise.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* number-or-marker-p () "Return t if OBJECT is a number or a marker.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* number-to-string () "Return the decimal representation of NUMBER as a string.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.

(fn NUMBER)"
  (error ’unimplemented-error))
(defun* numberp () "Return t if OBJECT is a number (floating point or integer).

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* position-symbol () "Create a new symbol with position.
SYM is a symbol, with or without position, the symbol to position.
POS, the position, is either a fixnum or a symbol with position from which
the position will be taken.

(fn SYM POS)"
  (error ’unimplemented-error))
(defun* recordp () "Return t if OBJECT is a record.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* remove-pos-from-symbol () "If ARG is a symbol with position, return it without the position.
Otherwise, return ARG unchanged.  Compare with ‘bare-symbol’.

(fn ARG)"
  (error ’unimplemented-error))
(defun* remove-variable-watcher () "Undo the effect of ‘add-variable-watcher’.
Remove WATCH-FUNCTION from the list of functions to be called when
SYMBOL (or its aliases) are set.

(fn SYMBOL WATCH-FUNCTION)"
  (error ’unimplemented-error))
(defun* sequencep () "Return t if OBJECT is a sequence (list or array).

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* set () "Set SYMBOL’s value to NEWVAL, and return NEWVAL.

(fn SYMBOL NEWVAL)"
  (error ’unimplemented-error))
(defun* set-default () "Set SYMBOL’s default value to VALUE.  SYMBOL and VALUE are evaluated.
The default value is seen in buffers that do not have their own values
for this variable.

(fn SYMBOL VALUE)"
  (error ’unimplemented-error))
(defun* setcar () "Set the car of CELL to be NEWCAR.  Returns NEWCAR.

(fn CELL NEWCAR)"
  (error ’unimplemented-error))
(defun* setcdr () "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.

(fn CELL NEWCDR)"
  (error ’unimplemented-error))
(defun* setplist () "Set SYMBOL’s property list to NEWPLIST, and return NEWPLIST.

(fn SYMBOL NEWPLIST)"
  (error ’unimplemented-error))
(defun* string-to-number () "Parse STRING as a decimal number and return the number.
Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
STRING cannot be parsed as an integer or floating point number.

If BASE, interpret STRING as a number in that base.  If BASE isn’t
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, STRING is always parsed as an integer.

(fn STRING &optional BASE)"
  (error ’unimplemented-error))
(defun* stringp () "Return t if OBJECT is a string.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* subr-arity () "Return minimum and maximum number of args allowed for SUBR.
SUBR must be a built-in function.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol ‘many’, for a
function with ‘&rest’ args, or ‘unevalled’ for a special form.

(fn SUBR)"
  (error ’unimplemented-error))
(defun* subr-name () "Return name of subroutine SUBR.
SUBR must be a built-in function.

(fn SUBR)"
  (error ’unimplemented-error))
(defun* subr-native-elisp-p () "Return t if the object is native compiled lisp
function, nil otherwise.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* subr-native-lambda-list () "Return the lambda list for a native compiled lisp/d
function or t otherwise.

(fn SUBR)"
  (error ’unimplemented-error))
(defun* subr-type () "Return the type of SUBR.

(fn SUBR)"
  (error ’unimplemented-error))
(defun* subrp () "Return t if OBJECT is a built-in function.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* symbol-function () "Return SYMBOL’s function definition, or nil if that is void.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* symbol-name () "Return SYMBOL’s name, a string.

Warning: never alter the string returned by ‘symbol-name’.
Doing that might make Emacs dysfunctional, and might even crash Emacs.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* symbol-plist () "Return SYMBOL’s property list.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* symbol-value () "Return SYMBOL’s value.  Error if that is void.
Note that if ‘lexical-binding’ is in effect, this returns the
global value outside of any lexical scope.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* symbol-with-pos-p () "Return t if OBJECT is a symbol together with position.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* symbol-with-pos-pos () "Extract the position from a symbol with position.

(fn LS)"
  (error ’unimplemented-error))
(defun* symbolp () "Return t if OBJECT is a symbol.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* threadp () "Return t if OBJECT is a thread.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* type-of () "Return a symbol representing the type of OBJECT.
The symbol returned names the object’s basic type;
for example, (type-of 1) returns ‘integer’.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* user-ptrp () "Return t if OBJECT is a module user pointer.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* variable-binding-locus () "Return a value indicating where VARIABLE’s current binding comes from.
If the current binding is buffer-local, the value is the current buffer.
If the current binding is global (the default), the value is nil.

(fn VARIABLE)"
  (error ’unimplemented-error))
(defun* vector-or-char-table-p () "Return t if OBJECT is a char-table or vector.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* vectorp () "Return t if OBJECT is a vector.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* wholenump () "Return t if OBJECT is a nonnegative integer.

(fn OBJECT)"
  (error ’unimplemented-error))
