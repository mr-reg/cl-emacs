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

(uiop:define-package :cl-emacs/keymap
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/keymap)
(log-enable :cl-emacs/keymap :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* accessible-keymaps () "Find all keymaps accessible via prefix characters from KEYMAP.
Returns a list of elements of the form (KEYS . MAP), where the sequence
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
so that the KEYS increase in length.  The first element is ([] . KEYMAP).
An optional argument PREFIX, if non-nil, should be a key sequence;
then the value includes only maps for prefixes that start with PREFIX.

(fn KEYMAP &optional PREFIX)"
  (error ’unimplemented-error))
(defun* command-remapping () "Return the remapping for command COMMAND.
Returns nil if COMMAND is not remapped (or not a symbol).

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by ‘event-start’ and ‘event-end’, and the
remapping occurs in the keymaps associated with it.  It can also be a
number or marker, in which case the keymap properties at the specified
buffer position instead of point are used.  The KEYMAPS argument is
ignored if POSITION is non-nil.

If the optional argument KEYMAPS is non-nil, it should be a keymap or list of
keymaps to search for command remapping.  Otherwise, search for the
remapping in all currently active keymaps.

(fn COMMAND &optional POSITION KEYMAPS)"
  (error ’unimplemented-error))
(defun* copy-keymap () "Return a copy of the keymap KEYMAP.

Note that this is almost never needed.  If you want a keymap that’s like
another yet with a few changes, you should use keymap inheritance rather
than copying.  That is, something like:

    (defvar-keymap foo-map
      :parent <theirmap>
      ...)

Or, if you need to support Emacs versions older than 29:

    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map <theirmap>)
      (define-key map ...)
      ...)

After performing ‘copy-keymap’, the copy starts out with the same definitions
of KEYMAP, but changing either the copy or KEYMAP does not affect the other.
Any key definitions that are subkeymaps are recursively copied.
However, a key definition which is a symbol whose definition is a keymap
is not copied.

(fn KEYMAP)"
  (error ’unimplemented-error))
(defun* current-active-maps () "Return a list of the currently active keymaps.
OLP if non-nil indicates that we should obey ‘overriding-local-map’ and
‘overriding-terminal-local-map’.  POSITION can specify a click position
like in the respective argument of ‘key-binding’.

(fn &optional OLP POSITION)"
  (error ’unimplemented-error))
(defun* current-global-map () "Return the current global keymap.

(fn)"
  (error ’unimplemented-error))
(defun* current-local-map () "Return current buffer’s local keymap, or nil if it has none.
Normally the local keymap is set by the major mode with ‘use-local-map’.

(fn)"
  (error ’unimplemented-error))
(defun* current-minor-mode-maps () "Return a list of keymaps for the minor modes of the current buffer.

(fn)"
  (error ’unimplemented-error))
(defun* define-key () "In KEYMAP, define key sequence KEY as DEF.
This is a legacy function; see ‘keymap-set’ for the recommended
function to use instead.

KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key’s definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see ‘make-keymap’),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node ‘(elisp)Extended Menu Items’.)

If REMOVE is non-nil, the definition will be removed.  This is almost
the same as setting the definition to nil, but makes a difference if
the KEYMAP has a parent, and KEY is shadowing the same binding in the
parent.  With REMOVE, subsequent lookups will return the binding in
the parent, and with a nil DEF, the lookups will return nil.

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP.

(fn KEYMAP KEY DEF &optional REMOVE)"
  (error ’unimplemented-error))
(defun* describe-buffer-bindings () "Insert the list of all defined keys and their definitions.
The list is inserted in the current buffer, while the bindings are
looked up in BUFFER.
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument MENUS, if non-nil, says to mention menu bindings.
(Ordinarily these are omitted from the output.)

(fn BUFFER &optional PREFIX MENUS)"
  (error ’unimplemented-error))
(defun* describe-vector () "Insert a description of contents of VECTOR.
This is text showing the elements of vector matched against indices.
DESCRIBER is the output function used; nil means use ‘princ’.

(fn VECTOR &optional DESCRIBER)"
  (error ’unimplemented-error))
(defun* help--describe-vector () "Insert in the current buffer a description of the contents of VECTOR.
Call DESCRIBER to insert the description of one value found in VECTOR.

PREFIX is a string describing the key which leads to the keymap that
this vector is in.

If PARTIAL, it means do not mention suppressed commands.

SHADOW is a list of keymaps that shadow this map.
If it is non-nil, look up the key in those maps and don’t mention it
if it is defined by any of them.

ENTIRE-MAP is the keymap in which this vector appears.
If the definition in effect in the whole map does not match
the one in this keymap, we ignore this one.

(fn VECTOR PREFIX DESCRIBER PARTIAL SHADOW ENTIRE-MAP MENTION-SHADOW)"
  (error ’unimplemented-error))
(defun* key-binding () "Return the binding for command KEY in current keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

Normally, ‘key-binding’ ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
optional second argument ACCEPT-DEFAULT is non-nil, ‘key-binding’ does
recognize the default bindings, just as ‘read-key-sequence’ does.

Like the normal command loop, ‘key-binding’ will remap the command
resulting from looking up KEY by looking up the command in the
current keymaps.  However, if the optional third argument NO-REMAP
is non-nil, ‘key-binding’ returns the unmapped command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by ‘event-start’ and ‘event-end’, and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used.

(fn KEY &optional ACCEPT-DEFAULT NO-REMAP POSITION)"
  (error ’unimplemented-error))
(defun* key-description () "Return a pretty description of key-sequence KEYS.
Optional arg PREFIX is the sequence of keys leading up to KEYS.
For example, [?\\C-x ?l] is converted into the string \"C-x l\".

For an approximate inverse of this, see ‘kbd’.

(fn KEYS &optional PREFIX)"
  (error ’unimplemented-error))
(defun* keymap--get-keyelt () "Given OBJECT which was found in a slot in a keymap,
trace indirect definitions to get the actual definition of that slot.
An indirect definition is a list of the form
(KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
and INDEX is the object to look up in KEYMAP to yield the definition.

Also if OBJECT has a menu string as the first element,
remove that.  Also remove a menu help string as second element.

If AUTOLOAD, load autoloadable keymaps
that are referred to with indirection.

(fn OBJECT AUTOLOAD)"
  (error ’unimplemented-error))
(defun* keymap-parent () "Return the parent keymap of KEYMAP.
If KEYMAP has no parent, return nil.

(fn KEYMAP)"
  (error ’unimplemented-error))
(defun* keymap-prompt () "Return the prompt-string of a keymap MAP.
If non-nil, the prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.

(fn MAP)"
  (error ’unimplemented-error))
(defun* keymapp () "Return t if OBJECT is a keymap.

A keymap is a list (keymap . ALIST),
or a symbol whose function definition is itself a keymap.
ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
a vector of densely packed bindings for small character codes
is also allowed as an element.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* lookup-key () "Look up key sequence KEY in KEYMAP.  Return the definition.
This is a legacy function; see ‘keymap-lookup’ for the recommended
function to use instead.

A value of nil means undefined.  See doc of ‘define-key’
for kinds of definitions.

A number as value means KEY is \"too long\";
that is, characters or symbols in it except for the last one
fail to be a valid sequence of prefix characters in KEYMAP.
The number is how many characters at the front of KEY
it takes to reach a non-prefix key.
KEYMAP can also be a list of keymaps.

Normally, ‘lookup-key’ ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, ‘lookup-key’ will
recognize the default bindings, just as ‘read-key-sequence’ does.

(fn KEYMAP KEY &optional ACCEPT-DEFAULT)"
  (error ’unimplemented-error))
(defun* make-keymap () "Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
CHARTABLE is a char-table that holds the bindings for all characters
without modifiers.  All entries in it are initially nil, meaning
\"command undefined\".  ALIST is an assoc-list which holds bindings for
function keys, mouse events, and any other things that appear in the
input stream.  Initially, ALIST is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with ‘x-popup-menu’.

(fn &optional STRING)"
  (error ’unimplemented-error))
(defun* make-sparse-keymap () "Construct and return a new sparse keymap.
Its car is ‘keymap’ and its cdr is an alist of (CHAR . DEFINITION),
which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
which binds the function key or mouse event SYMBOL to DEFINITION.
Initially the alist is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with ‘x-popup-menu’.

(fn &optional STRING)"
  (error ’unimplemented-error))
(defun* map-keymap () "Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.

If KEYMAP has a parent, the parent’s bindings are included as well.
This works recursively: if the parent has itself a parent, then the
grandparent’s bindings are also included and so on.

For more information, see Info node ‘(elisp) Keymaps’.

(fn FUNCTION KEYMAP)"
  (error ’unimplemented-error))
(defun* map-keymap-internal () "Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.
If KEYMAP has a parent, this function returns it without processing it.

(fn FUNCTION KEYMAP)"
  (error ’unimplemented-error))
(defun* minor-mode-key-binding () "Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is
the symbol which names the minor mode binding KEY, and BINDING is
KEY’s definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn’t include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of ‘lookup-key’ for more details about this.

(fn KEY &optional ACCEPT-DEFAULT)"
  (error ’unimplemented-error))
(defun* set-keymap-parent () "Modify KEYMAP to set its parent map to PARENT.
Return PARENT.  PARENT should be nil or another keymap.

(fn KEYMAP PARENT)"
  (error ’unimplemented-error))
(defun* single-key-description () "Return a pretty description of a character event KEY.
Control characters turn into C-whatever, etc.
Optional argument NO-ANGLES non-nil means don’t put angle brackets
around function keys and event symbols.

See ‘text-char-description’ for describing character codes.

(fn KEY &optional NO-ANGLES)"
  (error ’unimplemented-error))
(defun* text-char-description () "Return the description of CHARACTER in standard Emacs notation.
CHARACTER must be a valid character code that passes the ‘characterp’ test.
Control characters turn into \"^char\", and characters with Meta and other
modifiers signal an error, as they are not valid character codes.
This differs from ‘single-key-description’ which accepts character events,
and thus doesn’t enforce the ‘characterp’ condition, turns control
characters into \"C-char\", and uses the 2**27 bit for Meta.
See Info node ‘(elisp)Describing Characters’ for examples.

(fn CHARACTER)"
  (error ’unimplemented-error))
(defun* use-global-map () "Select KEYMAP as the global keymap.

(fn KEYMAP)"
  (error ’unimplemented-error))
(defun* use-local-map () "Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.

(fn KEYMAP)"
  (error ’unimplemented-error))
(defun* where-is-internal () "Return list of keys that invoke DEFINITION.
If KEYMAP is a keymap, search only KEYMAP and the global keymap.
If KEYMAP is nil, search all the currently active keymaps, except
 for ‘overriding-local-map’ (which is ignored).
If KEYMAP is a list of keymaps, search only those keymaps.

If optional 3rd arg FIRSTONLY is non-nil, return the first key sequence found,
rather than a list of all possible key sequences.
If FIRSTONLY is the symbol ‘non-ascii’, return the first binding found,
no matter what it is.
If FIRSTONLY has another non-nil value, prefer bindings
that use the modifier key specified in ‘where-is-preferred-modifier’
(or their meta variants) and entirely reject menu bindings.

If optional 4th arg NOINDIRECT is non-nil, don’t extract the commands inside
menu-items.  This makes it possible to search for a menu-item itself.

The optional 5th arg NO-REMAP alters how command remapping is handled:

- If another command OTHER-COMMAND is remapped to DEFINITION, normally
  search for the bindings of OTHER-COMMAND and include them in the
  returned list.  But if NO-REMAP is non-nil, include the vector
  [remap OTHER-COMMAND] in the returned list instead, without
  searching for those other bindings.

- If DEFINITION is remapped to OTHER-COMMAND, normally return the
  bindings for OTHER-COMMAND.  But if NO-REMAP is non-nil, return the
  bindings for DEFINITION instead, ignoring its remapping.

Keys that are represented as events that have a ‘non-key-event’ non-nil
symbol property are ignored.

(fn DEFINITION &optional KEYMAP FIRSTONLY NOINDIRECT NO-REMAP)"
  (error ’unimplemented-error))
