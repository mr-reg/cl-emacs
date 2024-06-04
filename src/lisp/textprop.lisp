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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/textprop
    (:use
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons)
  (:local-nicknames (#:pstrings #:cl-emacs/types/pstrings)                    )
  (:export #:set-text-properties
           #:add-text-properties)
  )
(in-package :cl-emacs/textprop)
(log-enable :cl-emacs/textprop :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/textprop)
(in-suite cl-emacs/textprop)

(defun* add-face-text-property (start end face &optional appendp object)
  #M"Add the face property to the text from START to END.
     FACE specifies the face to add.  It should be a valid value of the
     ‘face' property (typically a face name or a plist of face attributes
     and values).

     If any text in the region already has a non-nil ‘face' property, those
     face(s) are retained.  This is done by setting the ‘face' property to
     a list of faces, with FACE as the first element (by default) and the
     pre-existing faces as the remaining elements.

     If optional fourth argument APPEND is non-nil, append FACE to the end
     of the face list instead.

     If optional fifth argument OBJECT is a buffer (or nil, which means the
     current buffer), START and END are buffer positions (integers or
     markers).  If OBJECT is a string, START and END are 0-based indices
     into it.

     (fn START END FACE &optional APPEND OBJECT)"
  (error 'unimplemented-error))
(defun* add-text-properties (start end props &optional object)
  #M"Add properties to the text from START to END.
     The third argument PROPERTIES is a property list
     specifying the property values to add.  If the optional fourth argument
     OBJECT is a buffer (or nil, which means the current buffer),
     START and END are buffer positions (integers or markers).
     If OBJECT is a string, START and END are 0-based indices into it.
     Return t if any property value actually changed, nil otherwise.

     (fn START END PROPERTIES &optional OBJECT)"
  (assert (pstrings:pstring-p object))
  (log-debug2 "add-text-properties start:~s end:~s props:~s object:~s"
              start end props object)
  (pstrings:set-properties object start end (alexandria:plist-alist props) nil)
  (log-debug2 "add-text-properties result:~s" object)
  )
(test test-add-text-properties
  (let ((pstr (pstrings:build-pstring "ab" '((p3 . v3)))))
    (add-text-properties 1 2 '(p2 v2 p1 v1) pstr)
    (is (string= "#(\"ab\" 0 1 (P3 V3) 1 2 (P1 V1 P2 V2 P3 V3))"
                 (cl:format nil "~s" pstr)))
    ))
(defun* get-char-property ()
  #M"Return the value of POSITION's property PROP, in OBJECT.
     Both overlay properties and text properties are checked.
     OBJECT is optional and defaults to the current buffer.
     If POSITION is at the end of OBJECT, the value is nil.
     If OBJECT is a buffer, then overlay properties are considered as well as
     text properties.
     If OBJECT is a window, then that window's buffer is used, but window-specific
     overlays are considered only if they are associated with OBJECT.

     (fn POSITION PROP &optional OBJECT)"
  (error 'unimplemented-error))
(defun* get-char-property-and-overlay ()
  #M"Like ‘get-char-property', but with extra overlay information.
     The value is a cons cell.  Its car is the return value of ‘get-char-property'
     with the same arguments--that is, the value of POSITION's property
     PROP in OBJECT.  Its cdr is the overlay in which the property was
     found, or nil, if it was found as a text property or not found at all.

     OBJECT is optional and defaults to the current buffer.  OBJECT may be
     a string, a buffer or a window.  For strings, the cdr of the return
     value is always nil, since strings do not have overlays.  If OBJECT is
     a window, then that window's buffer is used, but window-specific
     overlays are considered only if they are associated with OBJECT.  If
     POSITION is at the end of OBJECT, both car and cdr are nil.

     (fn POSITION PROP &optional OBJECT)"
  (error 'unimplemented-error))
(defun* get-text-property ()
  #M"Return the value of POSITION's property PROP, in OBJECT.
     OBJECT should be a buffer or a string; if omitted or nil, it defaults
     to the current buffer.

     If POSITION is at the end of OBJECT, the value is nil, but note that
     buffer narrowing does not affect the value.  That is, if the buffer is
     narrowed and POSITION is at the end of the narrowed buffer, the result
     may be non-nil.

     (fn POSITION PROP &optional OBJECT)"
  (error 'unimplemented-error))
(defun* next-char-property-change ()
  #M"Return the position of next text property or overlay change.
This scans characters forward in the current buffer from POSITION till
it finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found, and LIMIT is nil or omitted, the function
returns (point-max).

If the optional second argument LIMIT is non-nil, the function doesn't
search past position LIMIT, and returns LIMIT if nothing is found
before LIMIT.  LIMIT is a no-op if it is greater than (point-max).

(fn POSITION &optional LIMIT)"
  (error 'unimplemented-error))
(defun* next-property-change ()
  #M"Return the position of next property change.
Scans characters forward from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if LIMIT is nil or omitted, and the property is constant all
the way to the end of OBJECT; if the value is non-nil, it is a position
greater than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.

(fn POSITION &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* next-single-char-property-change ()
  #M"Return the position of next text property or overlay change for a specific property.
Scans characters forward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.

In a string, scan runs to the end of the string, unless LIMIT is non-nil.
In a buffer, scan runs to end of buffer, unless LIMIT is non-nil.
If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.
However, if OBJECT is a buffer and LIMIT is beyond the end of the
buffer, this function returns ‘point-max', not LIMIT.

The property values are compared with ‘eq'.

(fn POSITION PROP &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* next-single-property-change ()
  #M"Return the position of next property change for a specific property.
Scans characters forward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
The property values are compared with ‘eq'.
Return nil if LIMIT is nil or omitted, and the property is constant all
the way to the end of OBJECT; if the value is non-nil, it is a position
greater than POSITION, never equal.

If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.

(fn POSITION PROP &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* previous-char-property-change ()
  #M"Return the position of previous text property or overlay change.
Scans characters backward in the current buffer from POSITION till it
finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found, and LIMIT is nil or omitted, the function
returns (point-min).

If the optional second argument LIMIT is non-nil, the function doesn't
search before position LIMIT, and returns LIMIT if nothing is found
before LIMIT.  LIMIT is a no-op if it is less than (point-min).

(fn POSITION &optional LIMIT)"
  (error 'unimplemented-error))
(defun* previous-property-change ()
  #M"Return the position of previous property change.
Scans characters backwards from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if LIMIT is nil or omitted, and the property is constant all
the way to the start of OBJECT; if the value is non-nil, it is a position
less than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
back past position LIMIT; return LIMIT if nothing is found until LIMIT.

(fn POSITION &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* previous-single-char-property-change ()
  #M"Return the position of previous text property or overlay change for a specific property.
Scans characters backward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.

In a string, scan runs to the start of the string, unless LIMIT is non-nil.
In a buffer, if LIMIT is nil or omitted, it runs to (point-min), and the
value cannot be less than that.
If the optional fourth argument LIMIT is non-nil, don't search back past
position LIMIT; return LIMIT if nothing is found before reaching LIMIT.

The property values are compared with ‘eq'.
If the property is constant all the way to the start of OBJECT, return the
first valid position in OBJECT.

(fn POSITION PROP &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* previous-single-property-change ()
  #M"Return the position of previous property change for a specific property.
Scans characters backward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
The property values are compared with ‘eq'.
Return nil if LIMIT is nil or omitted, and the property is constant all
the way to the start of OBJECT; if the value is non-nil, it is a position
less than POSITION, never equal.

If the optional fourth argument LIMIT is non-nil, don't search
back past position LIMIT; return LIMIT if nothing is found until LIMIT.

(fn POSITION PROP &optional OBJECT LIMIT)"
  (error 'unimplemented-error))
(defun* put-text-property (start end prop value &optional object)
  #M"Set one property of the text from START to END.
     The third and fourth arguments PROPERTY and VALUE
     specify the property to add.
     If the optional fifth argument OBJECT is a buffer (or nil, which means
     the current buffer), START and END are buffer positions (integers or
     markers).  If OBJECT is a string, START and END are 0-based indices into it.

     (fn START END PROPERTY VALUE &optional OBJECT)"
  (error 'unimplemented-error))
(defun* remove-list-of-text-properties (start end list-of-properties &optional object) "Remove some properties from text from START to END.
The third argument LIST-OF-PROPERTIES is a list of property names to remove.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property was actually removed, nil otherwise.

(fn START END LIST-OF-PROPERTIES &optional OBJECT)"
  (error 'unimplemented-error))
(defun* remove-text-properties (start end props &optional object) "Remove some properties from text from START to END.
The third argument PROPERTIES is a property list
whose property names specify the properties to remove.
(The values stored in PROPERTIES are ignored.)
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property was actually removed, nil otherwise.

Use ‘set-text-properties' if you want to remove all text properties.

(fn START END PROPERTIES &optional OBJECT)"
  (error 'unimplemented-error))
(defun* set-text-properties ((start fixnum) (end fixnum) props &optional object)
  #M"Completely replace properties of text from START to END.
     The third argument PROPERTIES is the new property list.
     If the optional fourth argument OBJECT is a buffer (or nil, which means
     the current buffer), START and END are buffer positions (integers or
     markers).  If OBJECT is a string, START and END are 0-based indices into it.
     If PROPERTIES is nil, the effect is to remove all properties from
     the designated part of OBJECT.

     (fn START END PROPERTIES &optional OBJECT)"
  ;; TODO: support buffers as parameter
  (assert (pstrings:pstring-p object))
  (pstrings:set-properties object start end (alexandria:plist-alist props) t)
  object)
(test test-set-text-properties
  (let ((pstr (pstrings:build-pstring "ab" '((p3 . v3)))))
    (set-text-properties 1 2 '(p2 v2 p1 v1) pstr)
    (is (string= "#(\"ab\" 0 1 (P3 V3) 1 2 (P1 V1 P2 V2))"
                 (cl:format nil "~s" pstr)))
    ))
(defun* text-properties-at ()
  #M"Return the list of properties of the character at POSITION in OBJECT.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).

If OBJECT is a string, POSITION is a 0-based index into it.

If POSITION is at the end of OBJECT, the value is nil, but note that
buffer narrowing does not affect the value.  That is, if OBJECT is a
buffer or nil, and the buffer is narrowed and POSITION is at the end
of the narrowed buffer, the result may be non-nil.

If you want to display the text properties at point in a human-readable
form, use the ‘describe-text-properties' command.

(fn POSITION &optional OBJECT)"
  (error 'unimplemented-error))
(defun* text-property-any ()
  #M"Check text from START to END for property PROPERTY equaling VALUE.
If so, return the position of the first character whose property PROPERTY
is ‘eq' to VALUE.  Otherwise return nil.
If the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.

(fn START END PROPERTY VALUE &optional OBJECT)"
  (error 'unimplemented-error))
(defun* text-property-not-all ()
  #M"Check text from START to END for property PROPERTY not equaling VALUE.
If so, return the position of the first character whose property PROPERTY
is not ‘eq' to VALUE.  Otherwise, return nil.
If the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.

(fn START END PROPERTY VALUE &optional OBJECT)"
  (error 'unimplemented-error))

(defun test-me ()
  (run! 'cl-emacs/textprop))
