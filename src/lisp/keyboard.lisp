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

(uiop:define-package :cl-emacs/keyboard
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/keyboard)
(log-enable :cl-emacs/keyboard :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* abort-recursive-edit () "Abort the command that requested this recursive edit or minibuffer input.

(fn)"
  (error ’unimplemented-error))
(defun* clear-this-command-keys () "Clear out the vector that ‘this-command-keys’ returns.
Also clear the record of the last 300 input events, unless optional arg
KEEP-RECORD is non-nil.

(fn &optional KEEP-RECORD)"
  (error ’unimplemented-error))
(defun* command-error-default-function () "Produce default output for unhandled error message.
Default value of ‘command-error-function’.

(fn DATA CONTEXT SIGNAL)"
  (error ’unimplemented-error))
(defun* current-idle-time () "Return the current length of Emacs idleness, or nil.
The value when Emacs is idle is a Lisp timestamp in the style of
‘current-time’.

The value when Emacs is not idle is nil.

PSEC is a multiple of the system clock resolution.

(fn)"
  (error ’unimplemented-error))
(defun* current-input-mode () "Return information about the way Emacs currently reads keyboard input.
The value is a list of the form (INTERRUPT FLOW META QUIT), where
  INTERRUPT is non-nil if Emacs is using interrupt-driven input; if
    nil, Emacs is using CBREAK mode.
  FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the
    terminal; this does not apply if Emacs uses interrupt-driven input.
  META is t if accepting 8-bit unencoded input with 8th bit as Meta flag.
  META is ‘encoded’ if accepting 8-bit encoded input with 8th bit as
    Meta flag which has to be interpreted after decoding the input.
  META is nil if ignoring the top bit of input, on the assumption that
    it is a parity bit.
  META is neither t nor nil if accepting 8-bit input and using
    all 8 bits as the character code.
  QUIT is the character Emacs currently uses to quit.
The elements of this list correspond to the arguments of
‘set-input-mode’.

(fn)"
  (error ’unimplemented-error))
(defun* discard-input () "Discard the contents of the terminal input buffer.
Also end any kbd macro being defined.

(fn)"
  (error ’unimplemented-error))
(defun* event-convert-list () "Convert the event description list EVENT-DESC to an event type.
EVENT-DESC should contain one base event type (a character or symbol)
and zero or more modifier names (control, meta, hyper, super, shift, alt,
drag, down, double or triple).  The base must be last.

The return value is an event type (a character or symbol) which has
essentially the same base event type and all the specified modifiers.
(Some compatibility base types, like symbols that represent a
character, are not returned verbatim.)

(fn EVENT-DESC)"
  (error ’unimplemented-error))
(defun* exit-recursive-edit () "Exit from the innermost recursive edit or minibuffer.

(fn)"
  (error ’unimplemented-error))
(defun* input-pending-p () "Return t if command input is currently available with no wait.
Actually, the value is nil only if we can be sure that no input is available;
if there is a doubt, the value is t.

If CHECK-TIMERS is non-nil, timers that are ready to run will do so.

(fn &optional CHECK-TIMERS)"
  (error ’unimplemented-error))
(defun* internal--track-mouse () "Call BODYFUN with mouse movement events enabled.

(fn BODYFUN)"
  (error ’unimplemented-error))
(defun* internal-event-symbol-parse-modifiers () "Parse the event symbol.  For internal use.

(fn SYMBOL)"
  (error ’unimplemented-error))
(defun* internal-handle-focus-in () "Internally handle focus-in events.
This function potentially generates an artificial switch-frame event.

(fn EVENT)"
  (error ’unimplemented-error))
(defun* lossage-size () "Return or set the maximum number of keystrokes to save.
If called with a non-nil ARG, set the limit to ARG and return it.
Otherwise, return the current limit.

The saved keystrokes are shown by ‘view-lossage’.

(fn &optional ARG)"
  (error ’unimplemented-error))
(defun* open-dribble-file () "Start writing input events to a dribble file called FILE.
Any previously open dribble file will be closed first.  If FILE is
nil, just close the dribble file, if any.

If the file is still open when Emacs exits, it will be closed then.

The events written to the file include keyboard and mouse input
events, but not events from executing keyboard macros.  The events are
written to the dribble file immediately without line buffering.

Be aware that this records ALL characters you type!
This may include sensitive information such as passwords.

(fn FILE)"
  (error ’unimplemented-error))
(defun* posn-at-point () "Return position information for buffer position POS in WINDOW.
POS defaults to point in WINDOW; WINDOW defaults to the selected window.

If POS is in invisible text or is hidden by ‘display’ properties,
this function may report on buffer positions before or after POS.

Return nil if POS is not visible in WINDOW.  Otherwise,
the return value is similar to that returned by ‘event-start’ for
a mouse click at the upper left corner of the glyph corresponding
to POS:
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The ‘posn-’ functions access elements of such lists.

(fn &optional POS WINDOW)"
  (error ’unimplemented-error))
(defun* posn-at-x-y () "Return position information for pixel coordinates X and Y.
By default, X and Y are relative to text area of the selected window.
Note that the text area includes the header-line and the tab-line of
the window, if any of them are present.
Optional third arg FRAME-OR-WINDOW non-nil specifies frame or window.
If optional fourth arg WHOLE is non-nil, X is relative to the left
edge of the window.

The return value is similar to a mouse click position:
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The ‘posn-’ functions access elements of such lists.

(fn X Y &optional FRAME-OR-WINDOW WHOLE)"
  (error ’unimplemented-error))
(defun* read-key-sequence () "Read a sequence of keystrokes and return as a string or vector.
The sequence is sufficient to specify a non-prefix command in the
current local and global maps.

First arg PROMPT is a prompt string.  If nil, do not prompt specially.
Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos
as a continuation of the previous key.

The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not
convert the last event to lower case.  (Normally any upper case event
is converted to lower case if the original event is undefined and the lower
case equivalent is defined.)  A non-nil value is appropriate for reading
a key sequence to be defined.

A C-g typed while in this function is treated like any other character,
and ‘quit-flag’ is not set.

If the key sequence starts with a mouse click, then the sequence is read
using the keymaps of the buffer of the window clicked in, not the buffer
of the selected window as normal.

‘read-key-sequence’ drops unbound button-down events, since you normally
only care about the click or drag events which follow them.  If a drag
or multi-click event is unbound, but the corresponding click event would
be bound, ‘read-key-sequence’ turns the event into a click event at the
drag’s starting position.  This means that you don’t have to distinguish
between click and drag, double, or triple events unless you want to.

‘read-key-sequence’ prefixes mouse events on mode lines, the vertical
lines separating windows, and scroll bars with imaginary keys
‘mode-line’, ‘vertical-line’, and ‘vertical-scroll-bar’.

Optional fourth argument CAN-RETURN-SWITCH-FRAME non-nil means that this
function will process a switch-frame event if the user switches frames
before typing anything.  If the user switches frames in the middle of a
key sequence, or at the start of the sequence but CAN-RETURN-SWITCH-FRAME
is nil, then the event will be put off until after the current key sequence.

‘read-key-sequence’ checks ‘function-key-map’ for function key
sequences, where they wouldn’t conflict with ordinary bindings.  See
‘function-key-map’ for more details.

The optional fifth argument CMD-LOOP, if non-nil, means
that this key sequence is being read by something that will
read commands one after another.  It should be nil if the caller
will read just one key sequence.

(fn PROMPT &optional CONTINUE-ECHO DONT-DOWNCASE-LAST CAN-RETURN-SWITCH-FRAME CMD-LOOP)"
  (error ’unimplemented-error))
(defun* read-key-sequence-vector () "Like ‘read-key-sequence’ but always return a vector.

(fn PROMPT &optional CONTINUE-ECHO DONT-DOWNCASE-LAST CAN-RETURN-SWITCH-FRAME CMD-LOOP)"
  (error ’unimplemented-error))
(defun* recent-keys () "Return vector of last few events, not counting those from keyboard macros.
If INCLUDE-CMDS is non-nil, include the commands that were run,
represented as pseudo-events of the form (nil . COMMAND).

(fn &optional INCLUDE-CMDS)"
  (error ’unimplemented-error))
(defun* recursion-depth () "Return the current depth in recursive edits.

(fn)"
  (error ’unimplemented-error))
(defun* recursive-edit () "Invoke the editor command loop recursively.
To get out of the recursive edit, a command can throw to ‘exit’ -- for
instance (throw \\=’exit nil).

The following values (last argument to ‘throw’) can be used when
throwing to \\=’exit:

- t causes ‘recursive-edit’ to quit, so that control returns to the
  command loop one level up.

- A string causes ‘recursive-edit’ to signal an error, printing that
  string as the error message.

- A function causes ‘recursive-edit’ to call that function with no
  arguments, and then return normally.

- Any other value causes ‘recursive-edit’ to return normally to the
  function that called it.

This function is called by the editor initialization to begin editing.

(fn)"
  (error ’unimplemented-error))
(defun* set--this-command-keys () "Set the vector to be returned by ‘this-command-keys’.
The argument KEYS must be a string.
Internal use only.

(fn KEYS)"
  (error ’unimplemented-error))
(defun* set-input-interrupt-mode () "Set interrupt mode of reading keyboard input.
If INTERRUPT is non-nil, Emacs will use input interrupts;
otherwise Emacs uses CBREAK mode.

See also ‘current-input-mode’.

(fn INTERRUPT)"
  (error ’unimplemented-error))
(defun* set-input-meta-mode () "Enable or disable 8-bit input on TERMINAL.
If META is t, Emacs will accept 8-bit input, and interpret the 8th
bit as the Meta modifier before it decodes the characters.

If META is ‘encoded’, Emacs will interpret the 8th bit of single-byte
characters after decoding the characters.

If META is nil, Emacs will ignore the top bit, on the assumption it is
parity.

Otherwise, Emacs will accept and pass through 8-bit input without
specially interpreting the top bit.

This setting only has an effect on tty terminal devices.

Optional parameter TERMINAL specifies the tty terminal device to use.
It may be a terminal object, a frame, or nil for the terminal used by
the currently selected frame.

See also ‘current-input-mode’.

(fn META &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* set-input-mode () "Set mode of reading keyboard input.
First arg INTERRUPT non-nil means use input interrupts;
 nil means use CBREAK mode.
Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal
 (no effect except in CBREAK mode).
Third arg META t means accept 8-bit input (for a Meta key).
 META nil means ignore the top bit, on the assumption it is parity.
 META ‘encoded’ means accept 8-bit input and interpret Meta after
   decoding the input characters.
 Otherwise, accept 8-bit input and don’t use the top bit for Meta.
Optional fourth arg QUIT if non-nil specifies character to use for quitting.
See also ‘current-input-mode’.

(fn INTERRUPT FLOW META &optional QUIT)"
  (error ’unimplemented-error))
(defun* set-output-flow-control () "Enable or disable ^S/^Q flow control for output to TERMINAL.
If FLOW is non-nil, flow control is enabled and you cannot use C-s or
C-q in key sequences.

This setting only has an effect on tty terminals and only when
Emacs reads input in CBREAK mode; see ‘set-input-interrupt-mode’.

See also ‘current-input-mode’.

(fn FLOW &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* set-quit-char () "Specify character used for quitting.
QUIT must be an ASCII character.

This function only has an effect on the controlling tty of the Emacs
process.

See also ‘current-input-mode’.

(fn QUIT)"
  (error ’unimplemented-error))
(defun* suspend-emacs () "Stop Emacs and return to superior process.  You can resume later.
If ‘cannot-suspend’ is non-nil, or if the system doesn’t support job
control, run a subshell instead.

If optional arg STUFFSTRING is non-nil, its characters are stuffed
to be read as terminal input by Emacs’s parent, after suspension.

Before suspending, run the normal hook ‘suspend-hook’.
After resumption run the normal hook ‘suspend-resume-hook’.

Some operating systems cannot stop the Emacs process and resume it later.
On such systems, Emacs starts a subshell instead of suspending.

(fn &optional STUFFSTRING)"
  (error ’unimplemented-error))
(defun* this-command-keys () "Return the key sequence that invoked this command.
However, if the command has called ‘read-key-sequence’, it returns
the last key sequence that has been read.
The value is a string or a vector.

See also ‘this-command-keys-vector’.

(fn)"
  (error ’unimplemented-error))
(defun* this-command-keys-vector () "Return the key sequence that invoked this command, as a vector.
However, if the command has called ‘read-key-sequence’, it returns
the last key sequence that has been read.

See also ‘this-command-keys’.

(fn)"
  (error ’unimplemented-error))
(defun* this-single-command-keys () "Return the key sequence that invoked this command.
More generally, it returns the last key sequence read, either by
the command loop or by ‘read-key-sequence’.
The value is always a vector.

(fn)"
  (error ’unimplemented-error))
(defun* this-single-command-raw-keys () "Return the raw events that were read for this command.
More generally, it returns the last key sequence read, either by
the command loop or by ‘read-key-sequence’.
Unlike ‘this-single-command-keys’, this function’s value
shows the events before all translations (except for input methods).
The value is always a vector.

(fn)"
  (error ’unimplemented-error))
(defun* top-level () "Exit all recursive editing levels.
This also exits all active minibuffers.

(fn)"
  (error ’unimplemented-error))
