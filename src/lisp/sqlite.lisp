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

(uiop:define-package :cl-emacs/sqlite
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/sqlite)
(log-enable :cl-emacs/sqlite :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* sqlite-available-p ()
  #M"Return t if sqlite3 support is available in this instance of Emacs.

(fn)"
  (error 'unimplemented-error))
(defun* sqlite-close ()
  #M"Close the sqlite database DB.

(fn DB)"
  (error 'unimplemented-error))
(defun* sqlite-columns ()
  #M"Return the column names of SET.

(fn SET)"
  (error 'unimplemented-error))
(defun* sqlite-commit ()
  #M"Commit a transaction in DB.

(fn DB)"
  (error 'unimplemented-error))
(defun* sqlite-execute ()
  #M"Execute a non-select SQL statement.
If VALUES is non-nil, it should be a vector or a list of values
to bind when executing a statement like

   insert into foo values (?, ?, ...)

Value is the number of affected rows.

(fn DB QUERY &optional VALUES)"
  (error 'unimplemented-error))
(defun* sqlite-finalize ()
  #M"Mark this SET as being finished.
This will free the resources held by SET.

(fn SET)"
  (error 'unimplemented-error))
(defun* sqlite-load-extension ()
  #M"Load an SQlite MODULE into DB.
MODULE should be the name of an SQlite module's file, a
shared library in the system-dependent format and having a
system-dependent file-name extension.

Only modules on Emacs' list of allowed modules can be loaded.

(fn DB MODULE)"
  (error 'unimplemented-error))
(defun* sqlite-more-p ()
  #M"Say whether there are any further results in SET.

(fn SET)"
  (error 'unimplemented-error))
(defun* sqlite-next ()
  #M"Return the next result set from SET.
Return nil when the statement has finished executing successfully.

(fn SET)"
  (error 'unimplemented-error))
(defun* sqlite-open ()
  #M"Open FILE as an sqlite database.
If FILE is nil, an in-memory database will be opened instead.

(fn &optional FILE)"
  (error 'unimplemented-error))
(defun* sqlite-pragma ()
  #M"Execute PRAGMA in DB.

(fn DB PRAGMA)"
  (error 'unimplemented-error))
(defun* sqlite-rollback ()
  #M"Roll back a transaction in DB.

(fn DB)"
  (error 'unimplemented-error))
(defun* sqlite-select ()
  #M"Select data from the database DB that matches QUERY.
If VALUES is non-nil, it should be a list or a vector specifying the
values that will be interpolated into a parameterized statement.

By default, the return value is a list, whose contents depend on
the value of the optional argument RETURN-TYPE.

If RETURN-TYPE is nil or omitted, the function returns a list of rows
matching QUERY.  If RETURN-TYPE is ‘full', the function returns a
list whose first element is the list of column names, and the rest
of the elements are the rows matching QUERY.  If RETURN-TYPE is ‘set',
the function returns a set object that can be queried with functions
like ‘sqlite-next' etc., in order to get the data.

(fn DB QUERY &optional VALUES RETURN-TYPE)"
  (error 'unimplemented-error))
(defun* sqlite-transaction ()
  #M"Start a transaction in DB.

(fn DB)"
  (error 'unimplemented-error))
(defun* sqlite-version ()
  #M"Return the version string of the SQLite library.
Signal an error if SQLite support is not available.

(fn)"
  (error 'unimplemented-error))
(defun* sqlitep ()
  #M"Say whether OBJECT is an SQlite object.

(fn OBJECT)"
  (error 'unimplemented-error))
