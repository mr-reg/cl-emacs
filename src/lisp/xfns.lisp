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

(uiop:define-package :cl-emacs/xfns
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/xfns)
(log-enable :cl-emacs/xfns :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* x-backspace-delete-keys-p () "Check if both Backspace and Delete keys are on the keyboard of FRAME.
FRAME nil means use the selected frame.
Value is t if we know that both keys are present, and are mapped to the
usual X keysyms.  Value is ‘lambda’ if we cannot determine if both keys are
present and mapped to the usual X keysyms.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* x-begin-drag () "Begin dragging contents on FRAME, with targets TARGETS.
TARGETS is a list of strings, which defines the X selection targets
that will be available to the drop target.  Block until the mouse
buttons are released, then return the action chosen by the target, or
‘nil’ if the drop was not accepted by the drop target.  Dragging
starts when the mouse is pressed on FRAME, and the contents of the
selection ‘XdndSelection’ will be sent to the X window underneath the
mouse pointer (the drop target) when the mouse button is released.

ACTION is a symbol which tells the target what it should do, and can
be one of the following:

 - ‘XdndActionCopy’, which means to copy the contents from the drag
   source (FRAME) to the drop target.

 - ‘XdndActionMove’, which means to first take the contents of
   ‘XdndSelection’, and to delete whatever was saved into that
   selection afterwards.

‘XdndActionPrivate’ is also a valid return value, and means that the
drop target chose to perform an unspecified or unknown action.

The source is also expected to cooperate with the target to perform
the action chosen by the target.  For example, callers should delete
the buffer text that was dragged if ‘XdndActionMove’ is returned.

There are also some other valid values of ACTION that depend on
details of both the drop target’s implementation details and that of
Emacs.  For that reason, they are not mentioned here.  Consult
\"Drag-and-Drop Protocol for the X Window System\" for more details:
https://freedesktop.org/wiki/Specifications/XDND/.

If RETURN-FRAME is non-nil, this function will return the frame if the
mouse pointer moves onto an Emacs frame, after first moving out of
FRAME.  (This is not guaranteed to work on some systems.)  If
RETURN-FRAME is the symbol ‘now’, any frame underneath the mouse
pointer will be returned immediately.

If ACTION is a list and not nil, its elements are assumed to be a cons
of (ITEM . STRING), where ITEM is the name of an action, and STRING is
a string describing ITEM to the user.  The drop target is expected to
prompt the user to choose between any of the actions in the list.

If ACTION is not specified or nil, ‘XdndActionCopy’ is used
instead.

If ALLOW-CURRENT-FRAME is not specified or nil, then the drop target
is allowed to be FRAME.  Otherwise, no action will be taken if the
mouse buttons are released on top of FRAME.

If FOLLOW-TOOLTIP is non-nil, any tooltip currently being displayed
will be moved to follow the mouse pointer while the drag is in
progress.  Note that this does not work with system tooltips (tooltips
created when ‘use-system-tooltips’ is non-nil).

This function will sometimes return immediately if no mouse buttons
are currently held down.  It should only be called when it is known
that mouse buttons are being held down, such as immediately after a
‘down-mouse-1’ (or similar) event.

(fn TARGETS &optional ACTION FRAME RETURN-FRAME ALLOW-CURRENT-FRAME FOLLOW-TOOLTIP)"
  (error ’unimplemented-error))
(defun* x-change-window-property () "Change window property PROP to VALUE on the X window of FRAME.
PROP must be a string.  VALUE may be a string or a list of conses,
numbers and/or strings.  If an element in the list is a string, it is
converted to an atom and the value of the atom is used.  If an element
is a cons, it is converted to a 32 bit number where the car is the 16
top bits and the cdr is the lower 16 bits.

FRAME nil or omitted means use the selected frame.  If TYPE is given
and non-nil, it is the name of the type of VALUE.  If TYPE is not
given or nil, the type is STRING.

FORMAT gives the size in bits of each element if VALUE is a list.  It
must be one of 8, 16 or 32.

If VALUE is a string or FORMAT is nil or not given, FORMAT defaults to
8.  If OUTER-P is non-nil, the property is changed for the outer X
window of FRAME.  Default is to change on the edit X window.

If WINDOW-ID is non-nil, change the property of that window instead of
FRAME’s X window; the number 0 denotes the root window.  This argument
is separate from FRAME because window IDs are not unique across X
displays or screens on the same display, so FRAME provides context for
the window ID.

If VALUE is a string and FORMAT is 32, then the format of VALUE is
system-specific.  VALUE must contain unsigned integer data in native
endian-ness in multiples of the size of the C type ’long’: the low 32
bits of each such number are used as the value of each element of the
property.

Wait for the request to complete and signal any error, unless
‘x-fast-protocol-requests’ is non-nil, in which case errors will be
silently ignored.

(fn PROP VALUE &optional FRAME TYPE FORMAT OUTER-P WINDOW-ID)"
  (error ’unimplemented-error))
(defun* x-close-connection () "Close the connection to TERMINAL’s X server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame’s terminal.
(On MS Windows, this function does not accept terminal objects.)

(fn TERMINAL)"
  (error ’unimplemented-error))
(defun* x-create-frame () "Make a new X window, which is called a \"frame\" in Emacs terms.
Return an Emacs frame object.  PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use, then
‘default-minibuffer-frame’ must be a frame whose minibuffer can be
shared by the new frame.

This function is an internal primitive--use ‘make-frame’ instead.

(fn PARMS)"
  (error ’unimplemented-error))
(defun* x-delete-window-property () "Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.
If WINDOW-ID is non-nil, remove property from that window instead
 of FRAME’s X window; the number 0 denotes the root window.  This
 argument is separate from FRAME because window IDs are not unique
 across X displays or screens on the same display, so FRAME provides
 context for the window ID.

Value is PROP.

Wait for the request to complete and signal any error, unless
‘x-fast-protocol-requests’ is non-nil, in which case errors will be
silently ignored.

(fn PROP &optional FRAME WINDOW-ID)"
  (error ’unimplemented-error))
(defun* x-display-backing-store () "Return an indication of whether X display TERMINAL does backing store.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

The value may be ‘always’, ‘when-mapped’, or ‘not-useful’.
On Nextstep, the value may be ‘buffered’, ‘retained’, or ‘non-retained’.
On MS Windows, this returns nothing useful.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-color-cells () "Return the number of color cells of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-grayscale-p () "Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-list () "Return the list of display names that Emacs has connections to.

(fn)"
  (error ’unimplemented-error))
(defun* x-display-mm-height () "Return the height in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the height in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use ‘display-monitor-attributes-list’.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-mm-width () "Return the width in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the width in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use ‘display-monitor-attributes-list’.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-monitor-attributes-list () "Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

In addition to the standard attribute keys listed in
‘display-monitor-attributes-list’, the following keys are contained in
the attributes:

 source -- String describing the source from which multi-monitor
	   information is obtained, one of \"Gdk\", \"XRandR 1.5\",
	   \"XRandr\", \"Xinerama\", or \"fallback\"

Internal use only, use ‘display-monitor-attributes-list’ instead.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-pixel-height () "Return the height in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the pixel height for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use ‘display-monitor-attributes-list’.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-pixel-width () "Return the width in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the pixel width for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use ‘display-monitor-attributes-list’.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-planes () "Return the number of bitplanes of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-save-under () "Return t if the X display TERMINAL supports the save-under feature.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

On MS Windows, this just returns nil.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-screens () "Return the number of screens on the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

On MS Windows, this function just returns 1.
On Nextstep, \"screen\" is in X terminology, not that of Nextstep.
For the number of physical monitors, use ‘(length
(display-monitor-attributes-list TERMINAL))’ instead.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-set-last-user-time () "Set the last user time of TERMINAL to TIME-OBJECT.
TIME-OBJECT is the X server time, in milliseconds, of the last user
interaction.  This is the timestamp that ‘x-get-selection-internal’
will use by default to fetch selection data.
The optional second argument TERMINAL specifies which display to act
on.  TERMINAL should be a terminal object, a frame or a display name
(a string).  If TERMINAL is omitted or nil, that stands for the
selected frame’s display.

(fn TIME-OBJECT &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-display-visual-class () "Return the visual class of the X display TERMINAL.
The value is one of the symbols ‘static-gray’, ‘gray-scale’,
‘static-color’, ‘pseudo-color’, ‘true-color’, or ‘direct-color’.
(On MS Windows, the second and last result above are not possible.)

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.
(On MS Windows, this function does not accept terminal objects.)

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-double-buffered-p () "Return t if FRAME is being double buffered.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* x-export-frames () "Return image data of FRAMES in TYPE format.
FRAMES should be nil (the selected frame), a frame, or a list of
frames (each of which corresponds to one page).  Each frame should be
visible.  Optional arg TYPE should be either ‘pdf’ (default), ‘png’,
‘postscript’, or ‘svg’.  Supported types are determined by the
compile-time configuration of cairo.

Note: Text drawn with the ‘x’ font backend is shown with hollow boxes
unless TYPE is ‘png’.

(fn &optional FRAMES TYPE)"
  (error ’unimplemented-error))
(defun* x-frame-edges () "Return edge coordinates of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is a list of the form (LEFT, TOP, RIGHT, BOTTOM).  All values are
in pixels relative to the origin - the position (0, 0) - of FRAME’s
display.

If optional argument TYPE is the symbol ‘outer-edges’, return the outer
edges of FRAME.  The outer edges comprise the decorations of the window
manager (like the title bar or external borders) as well as any external
menu or tool bar of FRAME.  If optional argument TYPE is the symbol
‘native-edges’ or nil, return the native edges of FRAME.  The native
edges exclude the decorations of the window manager and any external
menu or tool bar of FRAME.  If TYPE is the symbol ‘inner-edges’, return
the inner edges of FRAME.  These edges exclude title bar, any borders,
menu bar or tool bar of FRAME.

(fn &optional FRAME TYPE)"
  (error ’unimplemented-error))
(defun* x-frame-geometry () "Return geometric attributes of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is an association list of the attributes listed below.  All height
and width values are in pixels.

‘outer-position’ is a cons of the outer left and top edges of FRAME
  relative to the origin - the position (0, 0) - of FRAME’s display.

‘outer-size’ is a cons of the outer width and height of FRAME.  The
  outer size includes the title bar and the external borders as well as
  any menu and/or tool bar of frame.  For a child frame the value
  includes FRAME’s X borders, if any.

‘external-border-size’ is a cons of the horizontal and vertical width of
  FRAME’s external borders as supplied by the window manager.

‘title-bar-size’ is a cons of the width and height of the title bar of
  FRAME as supplied by the window manager.  If both of them are zero,
  FRAME has no title bar.  If only the width is zero, Emacs was not
  able to retrieve the width information.

‘menu-bar-external’, if non-nil, means the menu bar is external (never
  included in the inner edges of FRAME).

‘menu-bar-size’ is a cons of the width and height of the menu bar of
  FRAME.

‘tool-bar-external’, if non-nil, means the tool bar is external (never
  included in the inner edges of FRAME).

‘tool-bar-position’ tells on which side the tool bar on FRAME is and can
  be one of ‘left’, ‘top’, ‘right’ or ‘bottom’.  If this is nil, FRAME
  has no tool bar.

‘tool-bar-size’ is a cons of the width and height of the tool bar of
  FRAME.

‘internal-border-width’ is the width of the internal border of
  FRAME.

‘outer-border-width’ is the width of the X border of FRAME.  The X
  border is usually shown only for frames without window manager
  decorations, such as child and tooltip frames.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* x-frame-list-z-order () "Return list of Emacs’ frames, in Z (stacking) order.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be either a frame or a display name (a string).  If
omitted or nil, that stands for the selected frame’s display.  Return
nil if TERMINAL contains no Emacs frame.

As a special case, if TERMINAL is non-nil and specifies a live frame,
return the child frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-frame-restack () "Restack FRAME1 below FRAME2.
This means that if both frames are visible and the display areas of
these frames overlap, FRAME2 (partially) obscures FRAME1.  If optional
third argument ABOVE is non-nil, restack FRAME1 above FRAME2.  This
means that if both frames are visible and the display areas of these
frames overlap, FRAME1 (partially) obscures FRAME2.

This may be thought of as an atomic action performed in two steps: The
first step removes FRAME1’s window-step window from the display.  The
second step reinserts FRAME1’s window below (above if ABOVE is true)
that of FRAME2.  Hence the position of FRAME2 in its display’s Z
(stacking) order relative to all other frames excluding FRAME1 remains
unaltered.

Some window managers may refuse to restack windows.

(fn FRAME1 FRAME2 &optional ABOVE)"
  (error ’unimplemented-error))
(defun* x-get-modifier-masks () "Return the X modifier masks corresponding to keyboard modifiers.
The optional second argument TERMINAL specifies which display to fetch
modifier masks from.  TERMINAL should be a terminal object, a frame or
a display name (a string).  If TERMINAL is omitted or nil, that stands
for the selected frame’s display.

Return a list of (HYPER SUPER ALT SHIFT-LOCK META), each element being
a number describing the modifier mask for the corresponding Emacs
modifier.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-hide-tip () "Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.

(fn)"
  (error ’unimplemented-error))
(defun* x-internal-focus-input-context () "Focus and set the client window of all focused frames’ GTK input context.
If FOCUS is nil, focus out and remove the client window instead.
This should be called from a variable watcher for ‘x-gtk-use-native-input’.

(fn FOCUS)"
  (error ’unimplemented-error))
(defun* x-mouse-absolute-pixel-position () "Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the coordinates of
the mouse cursor position in pixels relative to a position (0, 0) of the
selected frame’s display.

(fn)"
  (error ’unimplemented-error))
(defun* x-open-connection () "Open a connection to a display server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can’t open the connection.
(In the Nextstep version, the last two arguments are currently ignored.)

(fn DISPLAY &optional XRM-STRING MUST-SUCCEED)"
  (error ’unimplemented-error))
(defun* x-server-input-extension-version () "Return the version of the X Input Extension supported by TERMINAL.
The value is nil if TERMINAL’s X server doesn’t support the X Input
Extension extension, or if Emacs doesn’t support the version present
on that server.  Otherwise, the return value is a list of the major
and minor versions of the X Input Extension extension running on that
server.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-server-max-request-size () "Return the maximum request size of the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

On MS Windows, this function just returns 1.
On Nextstep, this function just returns nil.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-server-vendor () "Return the \"vendor ID\" string of the GUI software on TERMINAL.

(Labeling every distributor as a \"vendor\" embodies the false assumption
that operating systems cannot be developed and distributed noncommercially.)
The optional argument TERMINAL specifies which display to ask about.

For GNU and Unix systems, this queries the X server software.
For MS Windows and Nextstep the result is hard-coded.

TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-server-version () "Return the version numbers of the GUI software on TERMINAL.
The value is a list of three integers specifying the version of the GUI
software in use.

For GNU and Unix system, the first 2 numbers are the version of the X
Protocol used on TERMINAL and the 3rd number is the distributor-specific
release number.  For MS Windows, the 3 numbers report the OS major and
minor version and build number.  For Nextstep, the first 2 numbers are
hard-coded and the 3rd represents the OS version.  For Haiku, all 3
numbers are hard-coded.

See also the function ‘x-server-vendor’.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame’s display.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-set-mouse-absolute-pixel-position () "Move mouse pointer to absolute pixel position (X, Y).
The coordinates X and Y are interpreted in pixels relative to a position
(0, 0) of the selected frame’s display.

(fn X Y)"
  (error ’unimplemented-error))
(defun* x-show-tip () "Show STRING in a \"tooltip\" window on frame FRAME.
A tooltip window is a small X window displaying a string.

This is an internal function; Lisp code should call ‘tooltip-show’.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip’s appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout from the ‘x-show-tooltip-timeout’
variable.

If the list of frame parameters PARMS contains a ‘left’ parameter,
display the tooltip at that x-position.  If the list of frame parameters
PARMS contains no ‘left’ but a ‘right’ parameter, display the tooltip
right-adjusted at that x-position. Otherwise display it at the
x-position of the mouse, with offset DX added (default is 5 if DX isn’t
specified).

Likewise for the y-position: If a ‘top’ frame parameter is specified, it
determines the position of the upper edge of the tooltip window.  If a
‘bottom’ parameter but no ‘top’ frame parameter is specified, it
determines the position of the lower edge of the tooltip window.
Otherwise display the tooltip window at the y-position of the mouse,
with offset DY added (default is -10).

A tooltip’s maximum size is specified by ‘x-max-tooltip-size’.
Text larger than the specified size is clipped.

(fn STRING &optional FRAME PARMS TIMEOUT DX DY)"
  (error ’unimplemented-error))
(defun* x-synchronize () "If ON is non-nil, report X errors as soon as the erring request is made.
This function has an effect only on X Windows.  With MS Windows, it is
defined but does nothing.

If ON is nil, allow buffering of requests.
Turning on synchronization prohibits the Xlib routines from buffering
requests and seriously degrades performance, but makes debugging much
easier.
The optional second argument TERMINAL specifies which display to act on.
TERMINAL should be a terminal object, a frame or a display name (a string).
If TERMINAL is omitted or nil, that stands for the selected frame’s display.

(fn ON &optional TERMINAL)"
  (error ’unimplemented-error))
(defun* x-translate-coordinates () "Translate coordinates from FRAME.
Translate the given coordinates SOURCE-X and SOURCE-Y from
SOURCE-WINDOW’s coordinate space to that of DEST-WINDOW, on FRAME.

If SOURCE-X and SOURCE-Y are nil, use 0 instead.

FRAME can either be a terminal or a frame.  If nil, it defaults to the
selected frame.  SOURCE-WINDOW must be an X window ID, 0 (which means
to use the root window), or nil, which means to use FRAME’s inner
window.  DEST-WINDOW must be another X window ID, or nil (which means
to use the root window).

Return a list of (X Y CHILD) if the given coordinates are on the same
screen, or nil otherwise, where X and Y are the coordinates in
DEST-WINDOW’s coordinate space, and CHILD is the window ID of any
mapped child in DEST-WINDOW at those coordinates, or nil if there is
no such window.  If REQUIRE-CHILD is nil, avoid fetching CHILD if it
would result in an avoidable request to the X server, thereby
improving performance when the X connection is over a slow network.
Otherwise, always obtain the mapped child window from the X
server.

(fn FRAME &optional SOURCE-WINDOW DEST-WINDOW SOURCE-X SOURCE-Y REQUIRE-CHILD)"
  (error ’unimplemented-error))
(defun* x-uses-old-gtk-dialog () "Return t if the old Gtk+ file selection dialog is used.

(fn)"
  (error ’unimplemented-error))
(defun* x-window-property () "Value is the value of window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.

On X Windows, the following optional arguments are also accepted: If
TYPE is nil or omitted, get the property as a string.  Otherwise TYPE
is the name of the atom that denotes the expected type.

If TYPE is the string \"AnyPropertyType\", decode and return the data
regardless of what the type really is.

The format of the data returned is the same as a selection conversion
to the given type.  For example, if ‘x-get-selection-internal’ returns
an integer when the selection data is a given type,
‘x-window-property’ will do the same for that type.

If WINDOW-ID is non-nil, get the property of that window instead of
FRAME’s X window; the number 0 denotes the root window.  This argument
is separate from FRAME because window IDs are not unique across X
displays, so FRAME provides context for the window ID.

If DELETE-P is non-nil, delete the property after retrieving it.
If VECTOR-RET-P is non-nil, return a vector of values instead of a string.

X allows an arbitrary number of properties to be set on any window.
However, properties are most often set by the window manager or other
programs on the root window or FRAME’s X window in order to
communicate information to Emacs and other programs.  Most of these
properties are specified as part of the Extended Window Manager Hints
and the Inter-Client Communication Conventions Manual, which are
located here:

  https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html

and

  https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html

Return value is nil if FRAME doesn’t have a property with name PROP or
if PROP has no value of TYPE (always a string in the MS Windows case).

(fn PROP &optional FRAME TYPE WINDOW-ID DELETE-P VECTOR-RET-P)"
  (error ’unimplemented-error))
(defun* x-window-property-attributes () "Retrieve metadata about window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.
If WINDOW-ID is non-nil, get the property of that window instead of
 FRAME’s X window; the number 0 denotes the root window.  This
 argument is separate from FRAME because window IDs are not unique
 across X displays or screens on the same display, so FRAME provides
 context for the window ID.

Return value is nil if FRAME doesn’t have a property named PROP.
Otherwise, the return value is a vector with the following fields:

0. The property type, as an integer.  The symbolic name of
 the type can be obtained with ‘x-get-atom-name’.
1. The format of each element; one of 8, 16, or 32.
2. The length of the property, in number of elements.

(fn PROP &optional FRAME WINDOW-ID)"
  (error ’unimplemented-error))
(defun* x-wm-set-size-hint () "Send the size hints for frame FRAME to the window manager.
If FRAME is omitted or nil, use the selected frame.
Signal error if FRAME is not an X frame.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* xw-color-defined-p () "Internal function called by ‘color-defined-p’.
(Note that the Nextstep version of this function ignores FRAME.)

(fn COLOR &optional FRAME)"
  (error ’unimplemented-error))
(defun* xw-color-values () "Internal function called by ‘color-values’.
(Note that the Nextstep version of this function ignores FRAME.)

(fn COLOR &optional FRAME)"
  (error ’unimplemented-error))
(defun* xw-display-color-p () "Internal function called by ‘display-color-p’.

(fn &optional TERMINAL)"
  (error ’unimplemented-error))
