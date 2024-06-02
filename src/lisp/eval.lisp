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

(uiop:define-package :cl-emacs/eval
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/eval)
(log-enable :cl-emacs/eval :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* and ()
  #M"Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.

(fn CONDITIONS...)"
  (error 'unimplemented-error))
(defun* apply ()
  #M"Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
With a single argument, call the argument's first element using the
other elements as args.
Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10.

(fn FUNCTION &rest ARGUMENTS)"
  (error 'unimplemented-error))
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
(defun* cond ()
  #M"Try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If a clause has one element, as in (CONDITION), then the cond-form
returns CONDITION's value, if that is non-nil.
If no clause succeeds, cond returns nil.

(fn CLAUSES...)"
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
(defun* defvar ()
  #M"Define SYMBOL as a variable, and return SYMBOL.
You are not required to define a variable in order to use it, but
defining it lets you supply an initial value and documentation, which
can be referred to by the Emacs help facilities and other programming
tools.  The ‘defvar' form also declares the variable as \"special\",
so that it is always dynamically bound even if ‘lexical-binding' is t.

If SYMBOL's value is void and the optional argument INITVALUE is
provided, INITVALUE is evaluated and the result used to set SYMBOL's
value.  If SYMBOL is buffer-local, its default value is what is set;
buffer-local values are not affected.  If INITVALUE is missing,
SYMBOL's value is not set.

If SYMBOL is let-bound, then this form does not affect the local let
binding but the toplevel default binding instead, like
‘set-toplevel-default-binding‘.
(‘defcustom' behaves similarly in this respect.)

The optional argument DOCSTRING is a documentation string for the
variable.

To define a user option, use ‘defcustom' instead of ‘defvar'.

To define a buffer-local variable, use ‘defvar-local'.

(fn SYMBOL &optional INITVALUE DOCSTRING)"
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
(defun* funcall ()
  #M"Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall \\='cons \\='x \\='y) returns (x . y).

(fn FUNCTION &rest ARGUMENTS)"
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
(defun* if ()
  #M"If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.

(fn COND THEN ELSE...)"
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
(defun* let ()
  #M"Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.

(fn VARLIST BODY...)"
  (error 'unimplemented-error))
(defun* let* ()
  #M"Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.

(fn VARLIST BODY...)"
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
(defun* or ()
  #M"Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.

(fn CONDITIONS...)"
  (error 'unimplemented-error))
(defun* prog1 ()
  #M"Eval FIRST and BODY sequentially; return value from FIRST.
The value of FIRST is saved during the evaluation of the remaining args,
whose values are discarded.

(fn FIRST BODY...)"
  (error 'unimplemented-error))
(defun* progn ()
  #M"Eval BODY forms sequentially and return value of last one.

(fn BODY...)"
  (error 'unimplemented-error))
(defun* quote ()
  #M"Return the argument, without evaluating it.  ‘(quote x)' yields ‘x'.
Warning: ‘quote' does not construct its return value, but just returns
the value that was pre-constructed by the Lisp reader (see info node
‘(elisp)Printed Representation').
This means that \\='(a . b) is not identical to (cons \\='a \\='b): the former
does not cons.  Quoting should be reserved for constants that will
never be modified by side-effects, unless you like self-modifying code.
See the common pitfall in info node ‘(elisp)Rearrangement' for an example
of unexpected results when a quoted object is modified.

(fn ARG)"
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
(defun* setq ()
  #M"Set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets ‘x' to the value of ‘(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the ‘setq'.
The return value of the ‘setq' form is the value of the last VAL.

(fn [SYM VAL]...)"
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
