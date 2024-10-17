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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/alloc
    (:use
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons)
  (:import-from #:cl
                #:cons
                #:list
                )
  (:export
   #:cons
   #:list
   #:intern
   #:make-symbol))
(in-package :cl-emacs/alloc)
(log-enable :cl-emacs/alloc :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* bool-vector ()
  #M"Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.

(fn &rest OBJECTS)"
  (error 'unimplemented-error))

(defun* garbage-collect ()
  #M"Reclaim storage for Lisp objects no longer needed.
Garbage collection happens automatically if you cons more than
‘gc-cons-threshold' bytes of Lisp data since previous garbage collection.
‘garbage-collect' normally returns a list with info on amount of space in use,
where each entry has the form (NAME SIZE USED FREE), where:
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs
  keeps around for future allocations (maybe because it does not know how
  to return them to the OS).

However, if there was overflow in pure space, and Emacs was dumped
using the \"unexec\" method, ‘garbage-collect' returns nil, because
real GC can't be done.

Note that calling this function does not guarantee that absolutely all
unreachable objects will be garbage-collected.  Emacs uses a
mark-and-sweep garbage collector, but is conservative when it comes to
collecting objects in some circumstances.

For further details, see Info node ‘(elisp)Garbage Collection'.

(fn)"
  (error 'unimplemented-error))
(defun* garbage-collect-maybe ()
  #M"Call ‘garbage-collect' if enough allocation happened.
FACTOR determines what \"enough\" means here:
If FACTOR is a positive number N, it means to run GC if more than
1/Nth of the allocations needed to trigger automatic allocation took
place.
Therefore, as N gets higher, this is more likely to perform a GC.
Returns non-nil if GC happened, and nil otherwise.

(fn FACTOR)"
  (error 'unimplemented-error))

(defun* make-bool-vector ()
  #M"Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.

(fn LENGTH INIT)"
  (error 'unimplemented-error))
(defun* make-byte-code ()
  #M"Create a byte-code object with specified arguments as elements.
The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
and (optional) INTERACTIVE-SPEC.
The first four arguments are required; at most six have any
significance.
The ARGLIST can be either like the one of ‘lambda', in which case the arguments
will be dynamically bound before executing the byte code, or it can be an
integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
of arguments (ignoring &rest) and the R bit specifies whether there is a &rest
argument to catch the left-over arguments.  If such an integer is used, the
arguments will not be dynamically bound but will be instead pushed on the
stack before executing the byte-code.

(fn ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)"
  (error 'unimplemented-error))
(defun* make-closure ()
  #M"Create a byte-code closure from PROTOTYPE and CLOSURE-VARS.
Return a copy of PROTOTYPE, a byte-code object, with CLOSURE-VARS
replacing the elements in the beginning of the constant-vector.

(fn PROTOTYPE &rest CLOSURE-VARS)"
  (error 'unimplemented-error))
(defun* make-finalizer ()
  #M"Make a finalizer that will run FUNCTION.
FUNCTION will be called after garbage collection when the returned
finalizer object becomes unreachable.  If the finalizer object is
reachable only through references from finalizer objects, it does not
count as reachable for the purpose of deciding whether to run
FUNCTION.  FUNCTION will be run once per finalizer object.

(fn FUNCTION)"
  (error 'unimplemented-error))
(defun* make-list ()
  #M"Return a newly created list of length LENGTH, with each element being INIT.

(fn LENGTH INIT)"
  (error 'unimplemented-error))
(defun* make-marker ()
  #M"Return a newly allocated marker which does not point at any place.

(fn)"
  (error 'unimplemented-error))
(defun* make-record ()
  #M"Create a new record.
TYPE is its type as returned by ‘type-of'; it should be either a
symbol or a type descriptor.  SLOTS is the number of non-type slots,
each initialized to INIT.

(fn TYPE SLOTS INIT)"
  (error 'unimplemented-error))
(defun* make-string ()
  #M"Return a newly created string of length LENGTH, with INIT in each element.
LENGTH must be an integer.
INIT must be an integer that represents a character.
If optional argument MULTIBYTE is non-nil, the result will be
a multibyte string even if INIT is an ASCII character.

(fn LENGTH INIT &optional MULTIBYTE)"
  (error 'unimplemented-error))
(defun* make-symbol ()
  #M"Return a newly allocated uninterned symbol whose name is NAME.
Its value is void, and its function definition and property list are nil.

(fn NAME)"
  (error 'unimplemented-error))
(defun* make-vector ()
  #M"Return a newly created vector of length LENGTH, with each element being INIT.
See also the function ‘vector'.

(fn LENGTH INIT)"
  (error 'unimplemented-error))
(defun* malloc-info ()
  #M"Report malloc information to stderr.
This function outputs to stderr an XML-formatted
description of the current state of the memory-allocation
arenas.

(fn)"
  (error 'unimplemented-error))
(defun* malloc-trim ()
  #M"Release free heap memory to the OS.
This function asks libc to return unused heap memory back to the operating
system.  This function isn't guaranteed to do anything, and is mainly
meant as a debugging tool.

If LEAVE_PADDING is given, ask the system to leave that much unused
space in the heap of the Emacs process.  This should be an integer, and if
not given, it defaults to 0.

This function returns nil if no memory could be returned to the
system, and non-nil if some memory could be returned.

(fn &optional LEAVE-PADDING)"
  (error 'unimplemented-error))
(defun* memory-info ()
  #M"Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP).
All values are in Kbytes.  If there is no swap space,
last two values are zero.  If the system is not supported
or memory information can't be obtained, return nil.
If ‘default-directory' is remote, return memory information of the
respective remote host.

(fn)"
  (error 'unimplemented-error))
(defun* memory-use-counts ()
  #M"Return a list of counters that measure how much consing there has been.
Each of these counters increments for a certain kind of object.
The counters wrap around from the largest positive integer to zero.
Garbage collection does not decrease them.
The elements of the value are as follows:
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)
All are in units of 1 = one object consed
except for VECTOR-CELLS and STRING-CHARS, which count the total length of
objects consed.
Frames, windows, buffers, and subprocesses count as vectors
  (but the contents of a buffer's text do not count here).

(fn)"
  (error 'unimplemented-error))
(defun* purecopy ()
  #M"Make a copy of object OBJ in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.  Copies strings without text properties.

(fn OBJ)"
  (error 'unimplemented-error))
(defun* record ()
  #M"Create a new record.
TYPE is its type as returned by ‘type-of'; it should be either a
symbol or a type descriptor.  SLOTS is used to initialize the record
slots with shallow copies of the arguments.

(fn TYPE &rest SLOTS)"
  (error 'unimplemented-error))
(defun* suspicious-object ()
  #M"Return OBJ, maybe marking it for extra scrutiny.
If Emacs is compiled with suspicious object checking, capture
a stack trace when OBJ is freed in order to help track down
garbage collection bugs.  Otherwise, do nothing and return OBJ.

(fn OBJ)"
  (error 'unimplemented-error))
(defun* vector ()
  #M"Return a newly created vector with specified arguments as elements.
Allows any number of arguments, including zero.

(fn &rest OBJECTS)"
  (error 'unimplemented-error))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/alloc)
