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

(uiop:define-package :cl-emacs/dired
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/dired)
(log-enable :cl-emacs/dired :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* directory-files ()
  #M"Return a list of names of files in DIRECTORY.
There are four optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names whose non-directory part
 matches the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with ‘string-lessp'.
 NOSORT is useful if you plan to sort the result yourself.
If COUNT is non-nil and a natural number, the function will return
 COUNT number of file names (if so many are present).

(fn DIRECTORY &optional FULL MATCH NOSORT COUNT)"
  (error 'unimplemented-error))
(defun* directory-files-and-attributes ()
  #M"Return a list of names of files and their attributes in DIRECTORY.
Value is a list of the form:

  ((FILE1 . FILE1-ATTRS) (FILE2 . FILE2-ATTRS) ...)

where each FILEn-ATTRS is the attributes of FILEn as returned
by ‘file-attributes'.

This function accepts five optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names whose non-directory part
 matches the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
ID-FORMAT specifies the preferred format of attributes uid and gid, see
 ‘file-attributes' for further documentation.
If COUNT is non-nil and a natural number, the function will return
 COUNT number of file names (if so many are present).
On MS-Windows, performance depends on ‘w32-get-true-file-attributes',
which see.

(fn DIRECTORY &optional FULL MATCH NOSORT ID-FORMAT COUNT)"
  (error 'unimplemented-error))
(defun* file-attributes ()
  #M"Return a list of attributes of file FILENAME.
Value is nil if specified file does not exist.

ID-FORMAT specifies the preferred format of attributes uid and gid (see
below) - valid values are ‘string' and ‘integer'.  The latter is the
default, but we plan to change that, so you should specify a non-nil value
for ID-FORMAT if you use the returned uid or gid.

To access the elements returned, the following access functions are
provided: ‘file-attribute-type', ‘file-attribute-link-number',
‘file-attribute-user-id', ‘file-attribute-group-id',
‘file-attribute-access-time', ‘file-attribute-modification-time',
‘file-attribute-status-change-time', ‘file-attribute-size',
‘file-attribute-modes', ‘file-attribute-inode-number', and
‘file-attribute-device-number'.

Elements of the attribute list are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid as a string or (if ID-FORMAT is ‘integer' or a string value
  cannot be looked up) as an integer.
 3. File gid, likewise.
 4. Last access time, in the style of ‘current-time'.
  (See a note below about access time on FAT-based filesystems.)
 5. Last modification time, likewise.  This is the time of the last
  change to the file's contents.
 6. Last status change time, likewise.  This is the time of last change
  to the file's attributes: owner and group, access mode bits, etc.
 7. Size in bytes, as an integer.
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. An unspecified value, present only for backward compatibility.
10. inode number, as a nonnegative integer.
11. Filesystem device identifier, as an integer or a cons cell of integers.

Large integers are bignums, so ‘eq' might not work on them.
On most filesystems, the combination of the inode and the device
identifier uniquely identifies the file.  This unique file identification
is provided by the access function ‘file-attribute-file-identifier'.

On MS-Windows, performance depends on ‘w32-get-true-file-attributes',
which see.

On some FAT-based filesystems, only the date of last access is recorded,
so last access time will always be midnight of that day.

(fn FILENAME &optional ID-FORMAT)"
  (error 'unimplemented-error))
(defun* file-attributes-lessp ()
  #M"Return t if first arg file attributes list is less than second.
Comparison is in lexicographic order and case is significant.

(fn F1 F2)"
  (error 'unimplemented-error))
(defun* file-name-all-completions ()
  #M"Return a list of all completions of file name FILE in directory DIRECTORY.
These are all file names in directory DIRECTORY which begin with FILE.

This function ignores some of the possible completions as determined
by ‘completion-regexp-list', which see.  ‘completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.

(fn FILE DIRECTORY)"
  (error 'unimplemented-error))
(defun* file-name-completion ()
  #M"Complete file name FILE in directory DIRECTORY.
Returns the longest string
common to all file names in DIRECTORY that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIRECTORY contains no name starting with FILE.

If PREDICATE is non-nil, call PREDICATE with each possible
completion (in absolute form) and ignore it if PREDICATE returns nil.

This function ignores some of the possible completions as determined
by the variables ‘completion-regexp-list' and
‘completion-ignored-extensions', which see.  ‘completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.

(fn FILE DIRECTORY &optional PREDICATE)"
  (error 'unimplemented-error))
(defun* system-groups ()
  #M"Return a list of user group names currently registered in the system.
The value may be nil if not supported on this platform.

(fn)"
  (error 'unimplemented-error))
(defun* system-users ()
  #M"Return a list of user names currently registered in the system.
If we don't know how to determine that on this platform, just
return a list with one element, taken from ‘user-real-login-name'.

(fn)"
  (error 'unimplemented-error))
