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

(uiop:define-package :cl-emacs/dbusbind
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/dbusbind)
(log-enable :cl-emacs/dbusbind :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* dbus--init-bus ()
  #M"Establish the connection to D-Bus BUS.

This function is dbus internal.  You almost certainly want to use
‘dbus-init-bus'.

BUS can be either the symbol ‘:system' or the symbol ‘:session', or it
can be a string denoting the address of the corresponding bus.  For
the system and session buses, this function is called when loading
‘dbus.el', there is no need to call it again.

A special case is BUS being the symbol ‘:system-private' or
‘:session-private'.  These symbols still denote the system or session
bus, but using a private connection.  They should not be used outside
dbus.el.

The function returns a number, which counts the connections this Emacs
session has established to the BUS under the same unique name (see
‘dbus-get-unique-name').  It depends on the libraries Emacs is linked
with, and on the environment Emacs is running.  For example, if Emacs
is linked with the gtk toolkit, and it runs in a GTK-aware environment
like Gnome, another connection might already be established.

When PRIVATE is non-nil, a new connection is established instead of
reusing an existing one.  It results in a new unique name at the bus.
This can be used, if it is necessary to distinguish from another
connection used in the same Emacs process, like the one established by
GTK+.  It should be used with care for at least the ‘:system' and
‘:session' buses, because other Emacs Lisp packages might already use
this connection to those buses.

(fn BUS &optional PRIVATE)"
  (error 'unimplemented-error))
(defun* dbus-get-unique-name ()
  #M"Return the unique name of Emacs registered at D-Bus BUS.

(fn BUS)"
  (error 'unimplemented-error))
(defun* dbus-message-internal ()
  #M"Send a D-Bus message.
This is an internal function, it shall not be used outside dbus.el.

The following usages are expected:

‘dbus-call-method', ‘dbus-call-method-asynchronously':
  (dbus-message-internal
    dbus-message-type-method-call BUS SERVICE PATH INTERFACE METHOD HANDLER
    &optional :timeout TIMEOUT &rest ARGS)

‘dbus-send-signal':
  (dbus-message-internal
    dbus-message-type-signal BUS SERVICE PATH INTERFACE SIGNAL &rest ARGS)

‘dbus-method-return-internal':
  (dbus-message-internal
    dbus-message-type-method-return BUS SERVICE SERIAL &rest ARGS)

‘dbus-method-error-internal':
  (dbus-message-internal
    dbus-message-type-error BUS SERVICE SERIAL ERROR-NAME &rest ARGS)

‘dbus-check-arguments': (does not send a message)
  (dbus-message-internal
    dbus-message-type-invalid BUS SERVICE &rest ARGS)

(fn &rest REST)"
  (error 'unimplemented-error))
