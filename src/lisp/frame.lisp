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

(uiop:define-package :cl-emacs/frame
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/frame)
(log-enable :cl-emacs/frame :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* delete-frame () "Delete FRAME, eliminating it from use.
FRAME must be a live frame and defaults to the selected one.

When ‘undelete-frame-mode’ is enabled, the 16 most recently deleted
frames can be undeleted with ‘undelete-frame’, which see.

A frame may not be deleted if its minibuffer serves as surrogate
minibuffer for another frame.  Normally, you may not delete a frame if
all other frames are invisible, but if the second optional argument
FORCE is non-nil, you may do so.

This function runs ‘delete-frame-functions’ before actually
deleting the frame, unless the frame is a tooltip.
The functions are run with one argument, the frame to be deleted.

(fn &optional FRAME FORCE)"
  (error ’unimplemented-error))
(defun* frame--set-was-invisible () "Set FRAME’s was-invisible flag if WAS-INVISIBLE is non-nil.
This function is for internal use only.

(fn FRAME WAS-INVISIBLE)"
  (error ’unimplemented-error))
(defun* frame-after-make-frame () "Mark FRAME as made.
FRAME nil means use the selected frame.  Second argument MADE non-nil
means functions on ‘window-configuration-change-hook’ are called
whenever the window configuration of FRAME changes.  MADE nil means
these functions are not called.

This function is currently called by ‘make-frame’ only and should be
otherwise used with utter care to avoid that running functions on
‘window-configuration-change-hook’ is impeded forever.

(fn FRAME MADE)"
  (error ’unimplemented-error))
(defun* frame-ancestor-p () "Return non-nil if ANCESTOR is an ancestor of DESCENDANT.
ANCESTOR is an ancestor of DESCENDANT when it is either DESCENDANT’s
parent frame or it is an ancestor of DESCENDANT’s parent frame.  Both,
ANCESTOR and DESCENDANT must be live frames and default to the selected
frame.

(fn ANCESTOR DESCENDANT)"
  (error ’unimplemented-error))
(defun* frame-bottom-divider-width () "Return width (in pixels) of horizontal window dividers on FRAME.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-char-height () "Height in pixels of a line in the font in frame FRAME.
If FRAME is omitted or nil, the selected frame is used.
For a terminal frame, the value is always 1.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-char-width () "Width in pixels of characters in the font in frame FRAME.
If FRAME is omitted or nil, the selected frame is used.
On a graphical screen, the width is the standard width of the default font.
For a terminal screen, the value is always 1.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-child-frame-border-width () "Return width of FRAME’s child-frame border in pixels.
 If FRAME’s ‘child-frame-border-width’ parameter is nil, return FRAME’s
 internal border width instead.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-focus () "Return the frame to which FRAME’s keystrokes are currently being sent.
If FRAME is omitted or nil, the selected frame is used.
Return nil if FRAME’s focus is not redirected.
See ‘redirect-frame-focus’.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-fringe-width () "Return fringe width of FRAME in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-internal-border-width () "Return width of FRAME’s internal border in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-list () "Return a list of all live frames.
The return value does not include any tooltip frame.

(fn)"
  (error ’unimplemented-error))
(defun* frame-live-p () "Return non-nil if OBJECT is a frame which has not been deleted.
Value is nil if OBJECT is not a live frame.  If object is a live
frame, the return value indicates what sort of terminal device it is
displayed on.  See the documentation of ‘framep’ for possible
return values.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* frame-native-height () "Return FRAME’s native height in pixels.
If FRAME is omitted or nil, the selected frame is used.  The exact value
of the result depends on the window-system and toolkit in use:

In the Gtk+ and NS versions, it includes only any window (including the
minibuffer or echo area), mode line, and header line.  It does not
include the tool bar or menu bar.  With other graphical versions, it may
also include the tool bar and the menu bar.

If you’re interested only in the height of the text portion of the
frame, see ‘frame-text-height’ instead.

For a text terminal, it includes the menu bar.  In this case, the
result is really in characters rather than pixels (i.e., is identical
to ‘frame-height’).

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-native-width () "Return FRAME’s native width in pixels.
For a terminal frame, the result really gives the width in characters.
If FRAME is omitted or nil, the selected frame is used.

If you’re interested only in the width of the text portion of the
frame, see ‘frame-text-width’ instead.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-parameter () "Return FRAME’s value for parameter PARAMETER.
If FRAME is nil, describe the currently selected frame.

(fn FRAME PARAMETER)"
  (error ’unimplemented-error))
(defun* frame-parameters () "Return the parameters-alist of frame FRAME.
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
If FRAME is omitted or nil, return information on the currently selected frame.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-parent () "Return the parent frame of FRAME.
The parent frame of FRAME is the Emacs frame whose window-system window
is the parent window of FRAME’s window-system window.  When such a frame
exists, FRAME is considered a child frame of that frame.

Return nil if FRAME has no parent frame.  This means that FRAME’s
window-system window is either a \"top-level\" window (a window whose
parent window is the window-system’s root window) or an embedded window
(a window whose parent window is owned by some other application).

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-pointer-visible-p () "Return t if the mouse pointer displayed on FRAME is visible.
Otherwise it returns nil.  FRAME omitted or nil means the
selected frame.  This is useful when ‘make-pointer-invisible’ is set.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-position () "Return top left corner of FRAME in pixels.
FRAME must be a live frame and defaults to the selected one.  The return
value is a cons (x, y) of the coordinates of the top left corner of
FRAME’s outer frame, in pixels relative to an origin (0, 0) of FRAME’s
display.

Note that the values returned are not guaranteed to be accurate: The
values depend on the underlying window system, and some systems add a
constant offset to the values.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-right-divider-width () "Return width (in pixels) of vertical window dividers on FRAME.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-scale-factor () "Return FRAMEs scale factor.
If FRAME is omitted or nil, the selected frame is used.
The scale factor is the amount by which a logical pixel size must be
multiplied to find the real number of pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-scroll-bar-height () "Return scroll bar height of FRAME in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-scroll-bar-width () "Return scroll bar width of FRAME in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-text-cols () "Return width in columns of FRAME’s text area.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-text-height () "Return text area height of FRAME in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-text-lines () "Return height in lines of FRAME’s text area.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-text-width () "Return text area width of FRAME in pixels.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-total-cols () "Return number of total columns of FRAME.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-total-lines () "Return number of total lines of FRAME.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* frame-visible-p () "Return t if FRAME is \"visible\" (actually in use for display).
Return the symbol ‘icon’ if FRAME is iconified or \"minimized\".
Return nil if FRAME was made invisible, via ‘make-frame-invisible’.
On graphical displays, invisible frames are not updated and are
usually not displayed at all, even in a window system’s \"taskbar\".

If FRAME is a text terminal frame, this always returns t.
Such frames are always considered visible, whether or not they are
currently being displayed on the terminal.

(fn FRAME)"
  (error ’unimplemented-error))
(defun* frame-window-state-change () "Return t if FRAME’s window state change flag is set, nil otherwise.
FRAME must be a live frame and defaults to the selected one.

If FRAME’s window state change flag is set, the default values of
‘window-state-change-functions’ and ‘window-state-change-hook’ will be
run during next redisplay, regardless of whether a window state change
actually occurred on FRAME or not.  After that, the value of this flag
is reset.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* framep () "Return non-nil if OBJECT is a frame.
Value is:
  t for a termcap frame (a character-only terminal),
 ‘x’ for an Emacs frame that is really an X window,
 ‘w32’ for an Emacs frame that is a window on MS-Windows display,
 ‘ns’ for an Emacs frame on a GNUstep or Macintosh Cocoa display,
 ‘pc’ for a direct-write MS-DOS frame,
 ‘pgtk’ for an Emacs frame running on pure GTK.
 ‘haiku’ for an Emacs frame running in Haiku.
See also ‘frame-live-p’.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* handle-switch-frame () "Handle a switch-frame event EVENT.
Switch-frame events are usually bound to this function.
A switch-frame event is an event Emacs sends itself to
indicate that input is arriving in a new frame. It does not
necessarily represent user-visible input focus.

(fn EVENT)"
  (error ’unimplemented-error))
(defun* iconify-frame () "Make the frame FRAME into an icon.
If omitted, FRAME defaults to the currently selected frame.

If FRAME is a child frame, consult the variable ‘iconify-child-frame’
for how to proceed.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* last-nonminibuffer-frame () "Return last non-minibuffer frame selected.

(fn)"
  (error ’unimplemented-error))
(defun* lower-frame () "Send FRAME to the back, so it is occluded by any frames that overlap it.
If you don’t specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn’t support multiple overlapping frames, this function does nothing.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* make-frame-invisible () "Make the frame FRAME invisible.
If omitted, FRAME defaults to the currently selected frame.
On graphical displays, invisible frames are not updated and are
usually not displayed at all, even in a window system’s \"taskbar\".

Normally you may not make FRAME invisible if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.

This function has no effect on text terminal frames.  Such frames are
always considered visible, whether or not they are currently being
displayed in the terminal.

(fn &optional FRAME FORCE)"
  (error ’unimplemented-error))
(defun* make-frame-visible () "Make the frame FRAME visible (assuming it is an X window).
If omitted, FRAME defaults to the currently selected frame.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* make-terminal-frame () "Create an additional terminal frame, possibly on another terminal.
This function takes one argument, an alist specifying frame parameters.

You can create multiple frames on a single text terminal, but only one
of them (the selected terminal frame) is actually displayed.

In practice, generally you don’t need to specify any parameters,
except when you want to create a new frame on another terminal.
In that case, the ‘tty’ parameter specifies the device file to open,
and the ‘tty-type’ parameter specifies the terminal type.  Example:

   (make-terminal-frame \\=’((tty . \"/dev/pts/5\") (tty-type . \"xterm\")))

Note that changing the size of one terminal frame automatically
affects all frames on the same terminal device.

(fn PARMS)"
  (error ’unimplemented-error))
(defun* modify-frame-parameters () "Modify FRAME according to new values of its parameters in ALIST.
If FRAME is nil, it defaults to the selected frame.
ALIST is an alist of parameters to change and their new values.
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
Which PARMs are meaningful depends on the kind of frame.
The meaningful parameters are acted upon, i.e. the frame is changed
according to their new values, and are also stored in the frame’s
parameter list so that ‘frame-parameters’ will return them.
PARMs that are not meaningful are still stored in the frame’s parameter
list, but are otherwise ignored.

(fn FRAME ALIST)"
  (error ’unimplemented-error))
(defun* mouse-pixel-position () "Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in pixel units, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.
FRAME might be nil if ‘track-mouse’ is set to ‘drag-source’.  This
means there is no frame under the mouse.  If Emacs is running on a
mouseless terminal or hasn’t been programmed to read the mouse
position, it returns the selected frame for FRAME and nil for X and
Y.

(fn)"
  (error ’unimplemented-error))
(defun* mouse-position () "Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in canonical character cells, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is the
vertical offset, measured in units of the frame’s default character size.
If Emacs is running on a mouseless terminal or hasn’t been programmed
to read the mouse position, it returns the selected frame for FRAME
and nil for X and Y.

FRAME might be nil if ‘track-mouse’ is set to ‘drag-source’.  This
means there is no frame under the mouse.  If ‘mouse-position-function’
is non-nil, ‘mouse-position’ calls it, passing the normal return value
to that function as an argument, and returns whatever that function
returns.

(fn)"
  (error ’unimplemented-error))
(defun* next-frame () "Return the next frame in the frame list after FRAME.
Only frames on the same terminal as FRAME are included in the list
of candidate frames.  If omitted, FRAME defaults to the selected frame.

If MINIFRAME is nil (the default), include all frames except
minibuffer-only frames.

If MINIFRAME is a window, include only its own frame and any frame now
using that window as the minibuffer.

If MINIFRAME is ‘visible’, include only visible frames.

If MINIFRAME is 0, include only visible and iconified frames.

If MINIFRAME is any other value, include all frames.

(fn &optional FRAME MINIFRAME)"
  (error ’unimplemented-error))
(defun* old-selected-frame () "Return the old selected FRAME.
FRAME must be a live frame and defaults to the selected one.

The return value is the frame selected the last time window change
functions were run.

(fn)"
  (error ’unimplemented-error))
(defun* previous-frame () "Return the previous frame in the frame list before FRAME.
It considers only frames on the same terminal as FRAME.
By default, skip minibuffer-only frames.
If omitted, FRAME defaults to the selected frame.
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
If MINIFRAME is a window, include only its own frame
and any frame now using that window as the minibuffer.
If MINIFRAME is ‘visible’, include all visible frames.
If MINIFRAME is 0, include all visible and iconified frames.
Otherwise, include all frames.

(fn &optional FRAME MINIFRAME)"
  (error ’unimplemented-error))
(defun* raise-frame () "Bring FRAME to the front, so it occludes any frames it overlaps.
If FRAME is invisible or iconified, make it visible.
If you don’t specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn’t support multiple overlapping frames, this function selects FRAME.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* reconsider-frame-fonts () "Recreate FRAME’s default font using updated font parameters.
Signal an error if FRAME is not a window system frame.  This should be
called after a ‘config-changed’ event is received, signaling that the
parameters (such as pixel density) used by the system to open fonts
have changed.

(fn FRAME)"
  (error ’unimplemented-error))
(defun* redirect-frame-focus () "Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.
In other words, switch-frame events caused by events in FRAME will
request a switch to FOCUS-FRAME, and ‘last-event-frame’ will be
FOCUS-FRAME after reading an event typed at FRAME.

If FOCUS-FRAME is nil, any existing redirection is canceled, and the
frame again receives its own keystrokes.

Focus redirection is useful for temporarily redirecting keystrokes to
a surrogate minibuffer frame when a frame doesn’t have its own
minibuffer window.

A frame’s focus redirection can be changed by ‘select-frame’.  If frame
FOO is selected, and then a different frame BAR is selected, any
frames redirecting their focus to FOO are shifted to redirect their
focus to BAR.  This allows focus redirection to work properly when the
user switches from one frame to another using ‘select-window’.

This means that a frame whose focus is redirected to itself is treated
differently from a frame whose focus is redirected to nil; the former
is affected by ‘select-frame’, while the latter is not.

The redirection lasts until ‘redirect-frame-focus’ is called to change it.

(fn FRAME &optional FOCUS-FRAME)"
  (error ’unimplemented-error))
(defun* select-frame () "Select FRAME.
Subsequent editing commands apply to its selected window.
Optional argument NORECORD means to neither change the order of
recently selected windows nor the buffer list.

The selection of FRAME lasts until the next time the user does
something to select a different frame, or until the next time
this function is called.  If you are using a window system, the
previously selected frame may be restored as the selected frame
when returning to the command loop, because it still may have
the window system’s input focus.  On a text terminal, the next
redisplay will display FRAME.

This function returns FRAME, or nil if FRAME has been deleted.

(fn FRAME &optional NORECORD)"
  (error ’unimplemented-error))
(defun* selected-frame () "Return the frame that is now selected.

(fn)"
  (error ’unimplemented-error))
(defun* set-frame-height () "Set text height of frame FRAME to HEIGHT lines.
Optional third arg PRETEND non-nil means that redisplay should use
HEIGHT lines but that the idea of the actual height of the frame should
not be changed.

Optional fourth argument PIXELWISE non-nil means that FRAME should be
HEIGHT pixels high.  Note: When ‘frame-resize-pixelwise’ is nil, some
window managers may refuse to honor a HEIGHT that is not an integer
multiple of the default frame font height.

When called interactively, HEIGHT is the numeric prefix and the
currently selected frame will be set to this height.

If FRAME is nil, it defaults to the selected frame.

(fn FRAME HEIGHT &optional PRETEND PIXELWISE)"
  (error ’unimplemented-error))
(defun* set-frame-position () "Set position of FRAME to (X, Y).
FRAME must be a live frame and defaults to the selected one.  X and Y,
if positive, specify the coordinate of the left and top edge of FRAME’s
outer frame in pixels relative to an origin (0, 0) of FRAME’s display.
If any of X or Y is negative, it specifies the coordinates of the right
or bottom edge of the outer frame of FRAME relative to the right or
bottom edge of FRAME’s display.

(fn FRAME X Y)"
  (error ’unimplemented-error))
(defun* set-frame-size () "Set text size of FRAME to WIDTH by HEIGHT, measured in characters.
Optional argument PIXELWISE non-nil means to measure in pixels.  Note:
When ‘frame-resize-pixelwise’ is nil, some window managers may refuse to
honor a WIDTH that is not an integer multiple of the default frame font
width or a HEIGHT that is not an integer multiple of the default frame
font height.

If FRAME is nil, it defaults to the selected frame.

(fn FRAME WIDTH HEIGHT &optional PIXELWISE)"
  (error ’unimplemented-error))
(defun* set-frame-width () "Set text width of frame FRAME to WIDTH columns.
Optional third arg PRETEND non-nil means that redisplay should use WIDTH
columns but that the idea of the actual width of the frame should not
be changed.

Optional fourth argument PIXELWISE non-nil means that FRAME should be
WIDTH pixels wide.  Note: When ‘frame-resize-pixelwise’ is nil, some
window managers may refuse to honor a WIDTH that is not an integer
multiple of the default frame font width.

When called interactively, WIDTH is the numeric prefix and the
currently selected frame will be set to this width.

If FRAME is nil, it defaults to the selected frame.

(fn FRAME WIDTH &optional PRETEND PIXELWISE)"
  (error ’unimplemented-error))
(defun* set-frame-window-state-change () "Set FRAME’s window state change flag according to ARG.
Set FRAME’s window state change flag if ARG is non-nil, reset it
otherwise.

If FRAME’s window state change flag is set, the default values of
‘window-state-change-functions’ and ‘window-state-change-hook’ will be
run during next redisplay, regardless of whether a window state change
actually occurred on FRAME or not.  After that, the value of FRAME’s
window state change flag is reset.

(fn &optional FRAME ARG)"
  (error ’unimplemented-error))
(defun* set-mouse-pixel-position () "Move the mouse pointer to pixel position (X,Y) in FRAME.
The position is given in pixels, where (0, 0) is the upper-left corner
of the frame, X is the horizontal offset, and Y is the vertical offset.

Note, this is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))

(fn FRAME X Y)"
  (error ’unimplemented-error))
(defun* set-mouse-position () "Move the mouse pointer to the center of character cell (X,Y) in FRAME.
Coordinates are relative to the frame, not a window,
so the coordinates of the top left character in the frame
may be nonzero due to left-hand scroll bars or the menu bar.

The position is given in canonical character cells, where (0, 0) is
the upper-left corner of the frame, X is the horizontal offset, and
Y is the vertical offset, measured in units of the frame’s default
character size.

This function is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))

(fn FRAME X Y)"
  (error ’unimplemented-error))
(defun* tool-bar-pixel-width () "Return width in pixels of FRAME’s tool bar.
The result is greater than zero only when the tool bar is on the left
or right side of FRAME.  If FRAME is omitted or nil, the selected frame
is used.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* visible-frame-list () "Return a list of all frames now \"visible\" (being updated).

(fn)"
  (error ’unimplemented-error))
(defun* window-system () "The name of the window system that FRAME is displaying through.
The value is a symbol:
 nil for a termcap frame (a character-only terminal),
 ‘x’ for an Emacs frame that is really an X window,
 ‘w32’ for an Emacs frame that is a window on MS-Windows display,
 ‘ns’ for an Emacs frame on a GNUstep or Macintosh Cocoa display,
 ‘pc’ for a direct-write MS-DOS frame.
 ‘pgtk’ for an Emacs frame using pure GTK facilities.
 ‘haiku’ for an Emacs frame running in Haiku.

FRAME defaults to the currently selected frame.

Use of this function as a predicate is deprecated.  Instead,
use ‘display-graphic-p’ or any of the other ‘display-*-p’
predicates which report frame’s specific UI-related capabilities.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* x-focus-frame () "Set the input focus to FRAME.
FRAME nil means use the selected frame.  Optional argument NOACTIVATE
means do not activate FRAME.

If there is no window system support, this function does nothing.

(fn FRAME &optional NOACTIVATE)"
  (error ’unimplemented-error))
(defun* x-get-resource () "Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.
This uses ‘INSTANCE.ATTRIBUTE’ as the key and ‘Emacs.CLASS’ as the
class, where INSTANCE is the name under which Emacs was invoked, or
the name specified by the ‘-name’ or ‘-rn’ command-line arguments.

The optional arguments COMPONENT and SUBCLASS add to the key and the
class, respectively.  You must specify both of them or neither.
If you specify them, the key is ‘INSTANCE.COMPONENT.ATTRIBUTE’
and the class is ‘Emacs.CLASS.SUBCLASS’.

(fn ATTRIBUTE CLASS &optional COMPONENT SUBCLASS)"
  (error ’unimplemented-error))
(defun* x-parse-geometry () "Parse a display geometry string STRING.
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
The properties returned may include ‘top’, ‘left’, ‘height’, and ‘width’.
For X, the value of ‘left’ or ‘top’ may be an integer,
or a list (+ N) meaning N pixels relative to top/left corner,
or a list (- N) meaning -N pixels relative to bottom/right corner.
On Nextstep, this just calls ‘ns-parse-geometry’.

(fn STRING)"
  (error ’unimplemented-error))
