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

(uiop:define-package :cl-emacs/inotify
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/inotify)
(log-enable :cl-emacs/inotify :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* inotify-add-watch () "Add a watch for FILE-NAME to inotify.

Return a watch descriptor.  The watch will look for ASPECT events and
invoke CALLBACK when an event occurs.

ASPECT might be one of the following symbols or a list of those symbols:

access
attrib
close-write
close-nowrite
create
delete
delete-self
modify
move-self
moved-from
moved-to
open

all-events or t
move
close

ASPECT can also contain the following symbols, which control whether
the watch descriptor will be created:

dont-follow
onlydir

Watching a directory is not recursive.  CALLBACK is passed a single argument
EVENT which contains an event structure of the format

(WATCH-DESCRIPTOR ASPECTS NAME COOKIE)

WATCH-DESCRIPTOR is the same object that was returned by this function.  It can
be tested for equality using ‘equal’.  ASPECTS describes the event.  It is a
list of ASPECT symbols described above and can also contain one of the following
symbols

ignored
isdir
q-overflow
unmount

If a directory is watched then NAME is the name of file that caused the event.

COOKIE is an object that can be compared using ‘equal’ to identify two matching
renames (moved-from and moved-to).

See inotify(7) and inotify_add_watch(2) for further information.  The
inotify fd is managed internally and there is no corresponding
inotify_init.  Use ‘inotify-rm-watch’ to remove a watch.

The following inotify bit-masks cannot be used because descriptors are
shared across different callers.

IN_EXCL_UNLINK
IN_MASK_ADD
IN_ONESHOT

(fn FILENAME ASPECT CALLBACK)"
  (error ’unimplemented-error))
(defun* inotify-rm-watch () "Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by ‘inotify-add-watch’.

See inotify_rm_watch(2) for more information.

(fn WATCH-DESCRIPTOR)"
  (error ’unimplemented-error))
(defun* inotify-valid-p () "Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by ‘inotify-add-watch’.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling ‘inotify-rm-watch’ also makes
it invalid.

(fn WATCH-DESCRIPTOR)"
  (error ’unimplemented-error))
