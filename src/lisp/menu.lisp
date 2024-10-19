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

(uiop:define-package :cl-emacs/menu
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/menu)
(log-enable :cl-emacs/menu :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* menu-bar-menu-at-x-y ()
  #M"Return the menu-bar menu on FRAME at pixel coordinates X, Y.
X and Y are frame-relative pixel coordinates, assumed to define
a location within the menu bar.
If FRAME is nil or omitted, it defaults to the selected frame.

Value is the symbol of the menu at X/Y, or nil if the specified
coordinates are not within the FRAME's menu bar.  The symbol can
be used to look up the menu like this:

     (lookup-key MAP [menu-bar SYMBOL])

where MAP is either the current global map or the current local map,
since menu-bar items come from both.

This function can return non-nil only on a text-terminal frame
or on an X frame that doesn't use any GUI toolkit.  Otherwise,
Emacs does not manage the menu bar and cannot convert coordinates
into menu items.

(fn X Y &optional FRAME)"
  (error 'unimplemented-error))
(defun* x-popup-dialog ()
  #M"Pop up a dialog box and return user's selection.
POSITION specifies which frame to use.
This is normally a mouse button event or a window or frame.
If POSITION is t, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

CONTENTS specifies the alternatives to display in the dialog box.
It is a list of the form (DIALOG ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right.
(By default, approximately half appear on each side.)

If HEADER is non-nil, the frame title for the box is \"Information\",
otherwise it is \"Question\".

If the user gets rid of the dialog box without making a valid choice,
for instance using the window manager, then this produces a quit and
‘x-popup-dialog' does not return.

(fn POSITION CONTENTS &optional HEADER)"
  (error 'unimplemented-error))
(defun* x-popup-menu ()
  #M"Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button event
or a list ((XOFFSET YOFFSET) WINDOW)
where XOFFSET and YOFFSET are positions in pixels from the top left
corner of WINDOW.  (WINDOW may be a window or a frame object.)
This controls the position of the top left of the menu as a whole.
If POSITION is t, it means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the \"definition\" in such a key binding looks like
(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.
  Then each keymap makes a separate pane.

When MENU is a keymap or a list of keymaps, the return value is the
list of events corresponding to the user's choice. Note that
‘x-popup-menu' does not actually execute the command bound to that
sequence of events.

Alternatively, you can specify a menu of multiple panes
  with a list of the form (TITLE PANE1 PANE2...),
where each pane is a list of form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE);
but a string can appear as an item--that makes a nonselectable line
in the menu.
With this form of menu, the return value is VALUE from the chosen item.

If POSITION is nil, don't display the menu at all, just precalculate the
cached information about equivalent key sequences.

If the user gets rid of the menu without making a valid choice, for
instance by clicking the mouse away from a valid choice or by typing
keyboard input, then this normally results in a quit and
‘x-popup-menu' does not return.  But if POSITION is a mouse button
event (indicating that the user invoked the menu with the mouse) then
no quit occurs and ‘x-popup-menu' returns nil.

(fn POSITION MENU)"
  (error 'unimplemented-error))
