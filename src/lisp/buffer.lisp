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

(uiop:define-package :cl-emacs/buffer
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/buffer)
(log-enable :cl-emacs/buffer :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* barf-if-buffer-read-only ()
  #M"Signal a ‘buffer-read-only' error if the current buffer is read-only.
If the text under POSITION (which defaults to point) has the
‘inhibit-read-only' text property set, the error will not be raised.

(fn &optional POSITION)"
  (error 'unimplemented-error))
(defun* buffer-base-buffer ()
  #M"Return the base buffer of indirect buffer BUFFER.
If BUFFER is not indirect, return nil.
BUFFER defaults to the current buffer.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-chars-modified-tick ()
  #M"Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see ‘buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of ‘buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-enable-undo ()
  #M"Start keeping undo information for buffer BUFFER.
No argument or nil as argument means do this for the current buffer.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-file-name ()
  #M"Return name of file BUFFER is visiting, or nil if none.
No argument or nil as argument means use the current buffer.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-list ()
  #M"Return a list of all live buffers.
If the optional arg FRAME is a frame, return the buffer list in the
proper order for that frame: the buffers shown in FRAME come first,
followed by the rest of the buffers.

(fn &optional FRAME)"
  (error 'unimplemented-error))
(defun* buffer-live-p ()
  #M"Return t if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* buffer-local-value ()
  #M"Return the value of VARIABLE in BUFFER.
If VARIABLE does not have a buffer-local binding in BUFFER, the value
is the default binding of the variable.

(fn VARIABLE BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-local-variables ()
  #M"Return an alist of variables that are buffer-local in BUFFER.
Most elements look like (SYMBOL . VALUE), describing one variable.
For a symbol that is locally unbound, just the symbol appears in the value.
Note that storing new VALUEs in these elements doesn't change the variables.
No argument or nil as argument means use current buffer as BUFFER.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-modified-p ()
  #M"Return non-nil if BUFFER was modified since its file was last read or saved.
No argument or nil as argument means use current buffer as BUFFER.

If BUFFER was autosaved since it was last modified, this function
returns the symbol ‘autosaved'.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-modified-tick ()
  #M"Return BUFFER's tick counter, incremented for each change in text.
Each buffer has a tick counter which is incremented each time the
text in that buffer is changed.  No argument or nil as argument means
use current buffer as BUFFER.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-name ()
  #M"Return the name of BUFFER, as a string.
BUFFER defaults to the current buffer.
Return nil if BUFFER has been killed.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-swap-text ()
  #M"Swap the text between current buffer and BUFFER.
Using this function from ‘save-excursion' might produce surprising
results, see Info node ‘(elisp)Swapping Text'.

(fn BUFFER)"
  (error 'unimplemented-error))
(defun* bury-buffer-internal ()
  #M"Move BUFFER to the end of the buffer list.

(fn BUFFER)"
  (error 'unimplemented-error))
(defun* current-buffer ()
  #M"Return the current buffer as a Lisp object.

(fn)"
  (error 'unimplemented-error))
(defun* delete-all-overlays ()
  #M"Delete all overlays of BUFFER.
BUFFER omitted or nil means delete all overlays of the current
buffer.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* delete-overlay ()
  #M"Delete the overlay OVERLAY from its buffer.

(fn OVERLAY)"
  (error 'unimplemented-error))
(defun* erase-buffer ()
  #M"Delete the entire contents of the current buffer.
Any narrowing restriction in effect (see ‘narrow-to-region') is removed,
so the buffer is truly empty after this.

(fn)"
  (error 'unimplemented-error))
(defun* force-mode-line-update ()
  #M"Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines, tab lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title.

(fn &optional ALL)"
  (error 'unimplemented-error))
(defun* generate-new-buffer-name ()
  #M"Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending ‘<NUMBER>', incrementing NUMBER
(starting at 2) until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use (if
it is in the sequence to be tried) even if a buffer with that name exists.

If NAME begins with a space (i.e., a buffer that is not normally
visible to users), then if buffer NAME already exists a random number
is first appended to NAME, to speed up finding a non-existent buffer.

(fn NAME &optional IGNORE)"
  (error 'unimplemented-error))
(defun* get-buffer ()
  #M"Return the buffer named BUFFER-OR-NAME.
BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
is a string and there is no buffer with that name, return nil.  If
BUFFER-OR-NAME is a buffer, return it as given.

(fn BUFFER-OR-NAME)"
  (error 'unimplemented-error))
(defun* get-buffer-create ()
  #M"Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
If BUFFER-OR-NAME is a string and a live buffer with that name exists,
return that buffer.  If no such buffer exists, create a new buffer with
that name and return it.

If BUFFER-OR-NAME starts with a space, the new buffer does not keep undo
information.  If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the
new buffer does not run the hooks ‘kill-buffer-hook',
‘kill-buffer-query-functions', and ‘buffer-list-update-hook'.  This
avoids slowing down internal or temporary buffers that are never
presented to users or passed on to other applications.

If BUFFER-OR-NAME is a buffer instead of a string, return it as given,
even if it is dead.  The return value is never nil.

(fn BUFFER-OR-NAME &optional INHIBIT-BUFFER-HOOKS)"
  (error 'unimplemented-error))
(defun* get-file-buffer ()
  #M"Return the buffer visiting file FILENAME (a string).
The buffer's ‘buffer-file-name' must match exactly the expansion of FILENAME.
If there is no such live buffer, return nil.
See also ‘find-buffer-visiting'.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* internal--set-buffer-modified-tick ()
  #M"Set BUFFER's tick counter to TICK.
No argument or nil as argument means use current buffer as BUFFER.

(fn TICK &optional BUFFER)"
  (error 'unimplemented-error))
(defun* kill-all-local-variables ()
  #M"Switch to Fundamental mode by killing current buffer's local variables.
Most local variable bindings are eliminated so that the default values
become effective once more.  Also, the syntax table is set from
‘standard-syntax-table', the local keymap is set to nil,
and the abbrev table from ‘fundamental-mode-abbrev-table'.
This function also forces redisplay of the mode line.

Every function to select a new major mode starts by
calling this function.

As a special exception, local variables whose names have a non-nil
‘permanent-local' property are not eliminated by this function.  If
the optional KILL-PERMANENT argument is non-nil, clear out these local
variables, too.

The first thing this function does is run
the normal hook ‘change-major-mode-hook'.

(fn &optional KILL-PERMANENT)"
  (error 'unimplemented-error))
(defun* kill-buffer ()
  #M"Kill the buffer specified by BUFFER-OR-NAME.
The argument may be a buffer or the name of an existing buffer.
Argument nil or omitted means kill the current buffer.  Return t if the
buffer is actually killed, nil otherwise.

The functions in ‘kill-buffer-query-functions' are called with the
buffer to be killed as the current buffer.  If any of them returns nil,
the buffer is not killed.  The hook ‘kill-buffer-hook' is run before the
buffer is actually killed.  The buffer being killed will be current
while the hook is running.  Functions called by any of these hooks are
supposed to not change the current buffer.  Neither hook is run for
internal or temporary buffers created by ‘get-buffer-create' or
‘generate-new-buffer' with argument INHIBIT-BUFFER-HOOKS non-nil.

Any processes that have this buffer as the ‘process-buffer' are killed
with SIGHUP.  This function calls ‘replace-buffer-in-windows' for
cleaning up all windows currently displaying the buffer to be killed.

(fn &optional BUFFER-OR-NAME)"
  (error 'unimplemented-error))
(defun* make-indirect-buffer ()
  #M"Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
BASE-BUFFER should be a live buffer, or the name of an existing buffer.

NAME should be a string which is not the name of an existing buffer.
Optional argument CLONE non-nil means preserve BASE-BUFFER's state,
such as major and minor modes, in the indirect buffer.

CLONE nil means the indirect buffer's state is reset to default values.

If optional argument INHIBIT-BUFFER-HOOKS is non-nil, the new buffer
does not run the hooks ‘kill-buffer-hook',
‘kill-buffer-query-functions', and ‘buffer-list-update-hook'.

(fn BASE-BUFFER NAME &optional CLONE INHIBIT-BUFFER-HOOKS)"
  (error 'unimplemented-error))
(defun* make-overlay ()
  #M"Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
BEG and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
(which means the text *is* included in the overlay).

(fn BEG END &optional BUFFER FRONT-ADVANCE REAR-ADVANCE)"
  (error 'unimplemented-error))
(defun* move-overlay ()
  #M"Set the endpoints of OVERLAY to BEG and END in BUFFER.
If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
buffer.

(fn OVERLAY BEG END &optional BUFFER)"
  (error 'unimplemented-error))
(defun* next-overlay-change ()
  #M"Return the next position after POS where an overlay starts or ends.
If there are no overlay boundaries from POS to (point-max),
the value is (point-max).

(fn POS)"
  (error 'unimplemented-error))
(defun* other-buffer ()
  #M"Return most recently selected buffer other than BUFFER.
Buffers not visible in windows are preferred to visible buffers, unless
optional second argument VISIBLE-OK is non-nil.  Ignore the argument
BUFFER unless it denotes a live buffer.  If the optional third argument
FRAME specifies a live frame, then use that frame's buffer list instead
of the selected frame's buffer list.

The buffer is found by scanning the selected or specified frame's buffer
list first, followed by the list of all buffers.  If no other buffer
exists, return the buffer ‘*scratch*' (creating it if necessary).

(fn &optional BUFFER VISIBLE-OK FRAME)"
  (error 'unimplemented-error))
(defun* overlay-buffer ()
  #M"Return the buffer OVERLAY belongs to.
Return nil if OVERLAY has been deleted.

(fn OVERLAY)"
  (error 'unimplemented-error))
(defun* overlay-end ()
  #M"Return the position at which OVERLAY ends.

(fn OVERLAY)"
  (error 'unimplemented-error))
(defun* overlay-get ()
  #M"Get the property of overlay OVERLAY with property name PROP.

(fn OVERLAY PROP)"
  (error 'unimplemented-error))
(defun* overlay-lists ()
  #M"Return a list giving all the overlays of the current buffer.

For backward compatibility, the value is actually a list that
holds another list; the overlays are in the inner list.
The list you get is a copy, so that changing it has no effect.
However, the overlays you get are the real objects that the buffer uses.

(fn)"
  (error 'unimplemented-error))
(defun* overlay-properties ()
  #M"Return a list of the properties on OVERLAY.
This is a copy of OVERLAY's plist; modifying its conses has no effect on
OVERLAY.

(fn OVERLAY)"
  (error 'unimplemented-error))
(defun* overlay-put ()
  #M"Set one property of overlay OVERLAY: give property PROP value VALUE.
VALUE will be returned.

(fn OVERLAY PROP VALUE)"
  (error 'unimplemented-error))
(defun* overlay-recenter ()
  #M"Recenter the overlays of the current buffer around position POS.
That makes overlay lookup faster for positions near POS (but perhaps slower
for positions far away from POS).

Since Emacs 29.1, this function is a no-op, because the implementation
of overlays changed and their lookup is now fast regardless of their
position in the buffer.  In particular, this function no longer affects
the value returned by ‘overlay-lists'.

(fn POS)"
  (error 'unimplemented-error))
(defun* overlay-start ()
  #M"Return the position at which OVERLAY starts.

(fn OVERLAY)"
  (error 'unimplemented-error))
(defun* overlayp ()
  #M"Return t if OBJECT is an overlay.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* overlays-at ()
  #M"Return a list of the overlays that contain the character at POS.
If SORTED is non-nil, then sort them by decreasing priority.

Zero-length overlays that start and stop at POS are not included in
the return value.  Instead use ‘overlays-in' if those overlays are of
interest.

(fn POS &optional SORTED)"
  (error 'unimplemented-error))
(defun* overlays-in ()
  #M"Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.

Empty overlays are included in the result if they are located at BEG,
between BEG and END, or at END provided END denotes the position at the
end of the accessible part of the buffer.

The resulting list of overlays is in an arbitrary unpredictable order.

(fn BEG END)"
  (error 'unimplemented-error))
(defun* previous-overlay-change ()
  #M"Return the previous position before POS where an overlay starts or ends.
If there are no overlay boundaries from (point-min) to POS,
the value is (point-min).

(fn POS)"
  (error 'unimplemented-error))
(defun* rename-buffer ()
  #M"Change current buffer's name to NEWNAME (a string).
If second arg UNIQUE is nil or omitted, it is an error if a
buffer named NEWNAME already exists.
If UNIQUE is non-nil, come up with a new name using
‘generate-new-buffer-name'.
Interactively, you can set UNIQUE with a prefix argument.
We return the name we actually gave the buffer.
This does not change the name of the visited file (if any).

(fn NEWNAME &optional UNIQUE)"
  (error 'unimplemented-error))
(defun* restore-buffer-modified-p ()
  #M"Like ‘set-buffer-modified-p', but doesn't redisplay buffer's mode line.
A nil FLAG means to mark the buffer as unmodified.  A non-nil FLAG
means mark the buffer as modified.  A special value of ‘autosaved'
will mark the buffer as modified and also as autosaved since it was
last modified.

This function also locks or unlocks the file visited by the buffer,
if both ‘buffer-file-truename' and ‘buffer-file-name' are non-nil.

It is not ensured that mode lines will be updated to show the modified
state of the current buffer.  Use with care.

(fn FLAG)"
  (error 'unimplemented-error))
(defun* set-buffer ()
  #M"Make buffer BUFFER-OR-NAME current for editing operations.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
See also ‘with-current-buffer' when you want to make a buffer current
temporarily.  This function does not display the buffer, so its effect
ends when the current command terminates.  Use ‘switch-to-buffer' or
‘pop-to-buffer' to switch buffers permanently.
The return value is the buffer made current.

(fn BUFFER-OR-NAME)"
  (error 'unimplemented-error))
(defun* set-buffer-major-mode ()
  #M"Set an appropriate major mode for BUFFER.
For the *scratch* buffer, use ‘initial-major-mode', otherwise choose a mode
according to the default value of ‘major-mode'.
Use this function before selecting the buffer, since it may need to inspect
the current buffer's major mode.

(fn BUFFER)"
  (error 'unimplemented-error))
(defun* set-buffer-modified-p ()
  #M"Mark current buffer as modified or unmodified according to FLAG.
A non-nil FLAG means mark the buffer modified.
In addition, this function unconditionally forces redisplay of the
mode lines of the windows that display the current buffer, and also
locks or unlocks the file visited by the buffer, depending on whether
the function's argument is non-nil, but only if both ‘buffer-file-name'
and ‘buffer-file-truename' are non-nil.

(fn FLAG)"
  (error 'unimplemented-error))
(defun* set-buffer-multibyte ()
  #M"Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
In these cases, the buffer contents remain unchanged as a sequence of
bytes but the contents viewed as characters do change.
If FLAG is ‘to', this makes the buffer a multibyte buffer by changing
all eight-bit bytes to eight-bit characters.
If the multibyte flag was really changed, undo information of the
current buffer is cleared.

(fn FLAG)"
  (error 'unimplemented-error))
