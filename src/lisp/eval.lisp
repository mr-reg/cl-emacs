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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/eval
    (:use
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons)
  (:import-from #:cl
                #:and
                #:apply
                #:cond
                #:defvar
                #:funcall
                #:if
                #:let
                #:let*
                #:or
                #:progn
                #:quote
                #:setq
                )
  (:export #:and
           #:apply
           #:cond
           #:defvar
           #:funcall
           #:if
           #:let
           #:let*
           #:or
           #:progn
           #:quote
           #:setq
           ))
(in-package :cl-emacs/eval)
(log-enable :cl-emacs/eval :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* autoload ()
  #M"Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to ‘load'.

Third arg DOCSTRING is documentation for the function.

Fourth arg INTERACTIVE if non-nil says function can be called
interactively.  If INTERACTIVE is a list, it is interpreted as a list
of modes the function is applicable for.

Fifth arg TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   ‘keymap' says FUNCTION is really a keymap, and
   ‘macro' or t says FUNCTION is really a macro.

Third through fifth args give info about the real definition.
They default to nil.

If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil.

(fn FUNCTION FILE &optional DOCSTRING INTERACTIVE TYPE)"
  (error 'unimplemented-error))
(defun* autoload-do-load ()
  #M"Load FUNDEF which should be an autoload.
If non-nil, FUNNAME should be the symbol whose function value is FUNDEF,
in which case the function returns the new autoloaded function value.
If equal to ‘macro', MACRO-ONLY specifies that FUNDEF should only be loaded if
it defines a macro.

(fn FUNDEF &optional FUNNAME MACRO-ONLY)"
  (error 'unimplemented-error))
(defun* backtrace--frames-from-thread ()
  #M"Return the list of backtrace frames from current execution point in THREAD.
If a frame has not evaluated the arguments yet (or is a special form),
the value of the list element is (nil FUNCTION ARG-FORMS...).
If a frame has evaluated its arguments and called its function already,
the value of the list element is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.

(fn THREAD)"
  (error 'unimplemented-error))
(defun* backtrace--locals ()
  #M"Return names and values of local variables of a stack frame.
NFRAMES and BASE specify the activation frame to use, as in ‘backtrace-frame'.

(fn NFRAMES &optional BASE)"
  (error 'unimplemented-error))
(defun* backtrace-debug ()
  #M"Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
The debugger is entered when that frame exits, if the flag is non-nil.

(fn LEVEL FLAG)"
  (error 'unimplemented-error))
(defun* backtrace-eval ()
  #M"Evaluate EXP in the context of some activation frame.
NFRAMES and BASE specify the activation frame to use, as in ‘backtrace-frame'.

(fn EXP NFRAMES &optional BASE)"
  (error 'unimplemented-error))
(defun* backtrace-frame--internal ()
  #M"Call FUNCTION on stack frame NFRAMES away from BASE.
Return the result of FUNCTION, or nil if no matching frame could be found.

(fn FUNCTION NFRAMES BASE)"
  (error 'unimplemented-error))
(defun* catch ()
  #M"Eval BODY allowing nonlocal exits using ‘throw'.
TAG is evalled to get the tag to use; it must not be nil.

Then the BODY is executed.
Within BODY, a call to ‘throw' with the same TAG exits BODY and this ‘catch'.
If no throw happens, ‘catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from ‘catch'.

(fn TAG BODY...)"
  (error 'unimplemented-error))
(defun* commandp ()
  #M"Non-nil if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include strings and vectors (treated
as keyboard macros), lambda-expressions that contain a top-level call
to ‘interactive', autoload definitions made by ‘autoload' with non-nil
fourth argument, and some of the built-in functions of Lisp.

Also, a symbol satisfies ‘commandp' if its function definition does so.

If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
then strings and vectors are not accepted.

(fn FUNCTION &optional FOR-CALL-INTERACTIVELY)"
  (error 'unimplemented-error))

(defun* condition-case ()
  #M"Regain control when an error is signaled.
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
or (:success BODY...), where the BODY is made of Lisp expressions.

A handler is applicable to an error if CONDITION-NAME is one of the
error's condition names.  Handlers may also apply when non-error
symbols are signaled (e.g., ‘quit').  A CONDITION-NAME of t applies to
any symbol, including non-error symbols.  If multiple handlers are
applicable, only the first one runs.

The car of a handler may be a list of condition names instead of a
single condition name; then it handles all of them.  If the special
condition name ‘debug' is present in this list, it allows another
condition in the list to run the debugger if ‘debug-on-error' and the
other usual mechanisms say it should (otherwise, ‘condition-case'
suppresses the debugger).

When a handler handles an error, control returns to the ‘condition-case'
and it executes the handler's BODY...
with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
(If VAR is nil, the handler can't access that information.)
Then the value of the last BODY form is returned from the ‘condition-case'
expression.

The special handler (:success BODY...) is invoked if BODYFORM terminated
without signaling an error.  BODY is then evaluated with VAR bound to
the value returned by BODYFORM.

See also the function ‘signal' for more info.

(fn VAR BODYFORM &rest HANDLERS)"
  (error 'unimplemented-error))
(defun* default-toplevel-value ()
  #M"Return SYMBOL's toplevel default value.
\"Toplevel\" means outside of any let binding.

(fn SYMBOL)"
  (error 'unimplemented-error))
(defun* defconst ()
  #M"Define SYMBOL as a constant variable.
This declares that neither programs nor users should ever change the
value.  This constancy is not actually enforced by Emacs Lisp, but
SYMBOL is marked as a special variable so that it is never lexically
bound.

The ‘defconst' form always sets the value of SYMBOL to the result of
evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
what is set; buffer-local values are not affected.  If SYMBOL has a
local binding, then this form sets the local binding's value.
However, you should normally not make local bindings for variables
defined with this form.

The optional DOCSTRING specifies the variable's documentation string.

(fn SYMBOL INITVALUE [DOCSTRING])"
  (error 'unimplemented-error))
(defun* defconst-1 ()
  #M"Like ‘defconst' but as a function.
More specifically, behaves like (defconst SYM 'INITVALUE DOCSTRING).

(fn SYM INITVALUE &optional DOCSTRING)"
  (error 'unimplemented-error))

(defun* defvar-1 ()
  #M"Like ‘defvar' but as a function.
More specifically behaves like (defvar SYM 'INITVALUE DOCSTRING).

(fn SYM INITVALUE &optional DOCSTRING)"
  (error 'unimplemented-error))
(defun* defvaralias ()
  #M"Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
Aliased variables always have the same value; setting one sets the other.
Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
The return value is BASE-VARIABLE.

(fn NEW-ALIAS BASE-VARIABLE &optional DOCSTRING)"
  (error 'unimplemented-error))
(defun* eval ()
  #M"Evaluate FORM and return its value.
If LEXICAL is t, evaluate using lexical scoping.
LEXICAL can also be an actual lexical environment, in the form of an
alist mapping symbols to their value.

(fn FORM &optional LEXICAL)"
  (error 'unimplemented-error))
(defun* fetch-bytecode ()
  #M"If byte-compiled OBJECT is lazy-loaded, fetch it now.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* func-arity ()
  #M"Return minimum and maximum number of args allowed for FUNCTION.
FUNCTION must be a function of some kind.
The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number, or the symbol ‘many', for a
function with ‘&rest' args, or ‘unevalled' for a special form.

(fn FUNCTION)"
  (error 'unimplemented-error))

(defun* funcall-with-delayed-message ()
  #M"Like ‘funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.
TIMEOUT is a number of seconds, and can be an integer or a floating
point number.

If FUNCTION takes less time to execute than TIMEOUT seconds, MESSAGE
is not displayed.

(fn TIMEOUT MESSAGE FUNCTION)"
  (error 'unimplemented-error))
(defun* function ()
  #M"Like ‘quote', but preferred for objects which are functions.
In byte compilation, ‘function' causes its argument to be handled by
the byte compiler.  Similarly, when expanding macros and expressions,
ARG can be examined and possibly expanded.  If ‘quote' is used
instead, this doesn't happen.

(fn ARG)"
  (error 'unimplemented-error))
(defun* functionp ()
  #M"Return t if OBJECT is a function.

An object is a function if it is callable via ‘funcall'; this includes
symbols with function bindings, but excludes macros and special forms.

Ordinarily return nil if OBJECT is not a function, although t might be
returned in rare cases.

(fn OBJECT)"
  (error 'unimplemented-error))

(defun* internal--define-uninitialized-variable ()
  #M"Define SYMBOL as a variable, with DOC as its docstring.
This is like ‘defvar' and ‘defconst' but without affecting the variable's
value.

(fn SYMBOL &optional DOC)"
  (error 'unimplemented-error))
(defun* internal-make-var-non-special ()
  #M"Internal function.

(fn SYMBOL)"
  (error 'unimplemented-error))

(defun* macroexpand ()
  #M"Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.

(fn FORM &optional ENVIRONMENT)"
  (error 'unimplemented-error))
(defun* mapbacktrace ()
  #M"Call FUNCTION for each frame in backtrace.
If BASE is non-nil, it should be a function and iteration will start
from its nearest activation frame.
FUNCTION is called with 4 arguments: EVALD, FUNC, ARGS, and FLAGS.  If
a frame has not evaluated its arguments yet or is a special form,
EVALD is nil and ARGS is a list of forms.  If a frame has evaluated
its arguments and called its function already, EVALD is t and ARGS is
a list of values.
FLAGS is a plist of properties of the current frame: currently, the
only supported property is :debug-on-exit.  ‘mapbacktrace' always
returns nil.

(fn FUNCTION &optional BASE)"
  (error 'unimplemented-error))

(defun* prog1 ()
  #M"Eval FIRST and BODY sequentially; return value from FIRST.
The value of FIRST is saved during the evaluation of the remaining args,
whose values are discarded.

(fn FIRST BODY...)"
  (error 'unimplemented-error))

(defun* run-hook-with-args ()
  #M"Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS.  The final return value
is unspecified.

Do not use ‘make-local-variable' to make a hook variable buffer-local.
Instead, use ‘add-hook' and specify t for the LOCAL argument.

(fn HOOK &rest ARGS)"
  (error 'unimplemented-error))
(defun* run-hook-with-args-until-failure ()
  #M"Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns nil, and return nil.  Otherwise (if all functions
return non-nil, or if there are no functions to call), return non-nil
(do not rely on the precise return value in this case).

Do not use ‘make-local-variable' to make a hook variable buffer-local.
Instead, use ‘add-hook' and specify t for the LOCAL argument.

(fn HOOK &rest ARGS)"
  (error 'unimplemented-error))
(defun* run-hook-with-args-until-success ()
  #M"Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns non-nil, and return that value.  Otherwise (if
all functions return nil, or if there are no functions to call),
return nil.

Do not use ‘make-local-variable' to make a hook variable buffer-local.
Instead, use ‘add-hook' and specify t for the LOCAL argument.

(fn HOOK &rest ARGS)"
  (error 'unimplemented-error))
(defun* run-hook-wrapped ()
  #M"Run HOOK, passing each function through WRAP-FUNCTION.
I.e. instead of calling each function FUN directly with arguments ARGS,
it calls WRAP-FUNCTION with arguments FUN and ARGS.
As soon as a call to WRAP-FUNCTION returns non-nil, ‘run-hook-wrapped'
aborts and returns that value.

(fn HOOK WRAP-FUNCTION &rest ARGS)"
  (error 'unimplemented-error))
(defun* run-hooks ()
  #M"Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

Major modes should not use this function directly to run their mode
hook; they should use ‘run-mode-hooks' instead.

Do not use ‘make-local-variable' to make a hook variable buffer-local.
Instead, use ‘add-hook' and specify t for the LOCAL argument.

(fn &rest HOOKS)"
  (error 'unimplemented-error))
(defun* set-default-toplevel-value ()
  #M"Set SYMBOL's toplevel default value to VALUE.
\"Toplevel\" means outside of any let binding.

(fn SYMBOL VALUE)"
  (error 'unimplemented-error))

(defun* signal ()
  #M"Signal an error.  Args are ERROR-SYMBOL and associated DATA.
This function does not return.

An error symbol is a symbol with an ‘error-conditions' property
that is a list of condition names.  The symbol should be non-nil.
A handler for any of those names will get to handle this signal.
The symbol ‘error' should normally be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
See Info anchor ‘(elisp)Definition of signal' for some details on how this
error message is constructed.
If the signal is handled, DATA is made available to the handler.
See also the function ‘condition-case'.

(fn ERROR-SYMBOL DATA)"
  (error 'unimplemented-error))
(defun* special-variable-p ()
  #M"Return non-nil if SYMBOL's global binding has been declared special.
A special variable is one that will be bound dynamically, even in a
context where binding is lexical by default.

(fn SYMBOL)"
  (error 'unimplemented-error))
(defun* throw ()
  #M"Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.

(fn TAG VALUE)"
  (error 'unimplemented-error))
(defun* unwind-protect ()
  #M"Do BODYFORM, protecting with UNWINDFORMS.
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.

(fn BODYFORM UNWINDFORMS...)"
  (error 'unimplemented-error))
(defun* while ()
  #M"If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.

The value of a ‘while' form is always nil.

(fn TEST BODY...)"
  (error 'unimplemented-error))
