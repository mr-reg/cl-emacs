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

(uiop:define-package :cl-emacs/fileio
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/fileio)
(log-enable :cl-emacs/fileio :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* access-file ()
  #M"Access file FILENAME, and get an error if that does not work.
The second argument STRING is prepended to the error message.
If there is no error, returns nil.

(fn FILENAME STRING)"
  (error 'unimplemented-error))
(defun* add-name-to-file ()
  #M"Give FILE additional name NEWNAME.  Both args must be strings.
If NEWNAME is a directory name, give FILE a like-named new name under
NEWNAME.

Signal a ‘file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.

(fn FILE NEWNAME &optional OK-IF-ALREADY-EXISTS)"
  (error 'unimplemented-error))
(defun* car-less-than-car ()
  #M"Return t if (car A) is numerically less than (car B).

(fn A B)"
  (error 'unimplemented-error))
(defun* clear-buffer-auto-save-failure ()
  #M"Clear any record of a recent auto-save failure in the current buffer.

(fn)"
  (error 'unimplemented-error))
(defun* copy-file ()
  #M"Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME is a directory name, copy FILE to a like-named file under
NEWNAME.  For NEWNAME to be recognized as a directory name, it should
end in a slash.

This function always sets the file modes of the output file to match
the input file.

The optional third argument OK-IF-ALREADY-EXISTS specifies what to do
if file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil,
signal a ‘file-already-exists' error without overwriting.  If
OK-IF-ALREADY-EXISTS is an integer, request confirmation from the user
about overwriting; this is what happens in interactive use with M-x.
Any other value for OK-IF-ALREADY-EXISTS means to overwrite the
existing file.

Fourth arg KEEP-TIME non-nil means give the output file the same
last-modified time as the old one.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

If PRESERVE-UID-GID is non-nil, try to transfer the uid and gid of
FILE to NEWNAME.

If PRESERVE-PERMISSIONS is non-nil, copy permissions of FILE to NEWNAME;
this includes the file modes, along with ACL entries and SELinux
context if present.  Otherwise, if NEWNAME is created its file
permission bits are those of FILE, masked by the default file
permissions.

(fn FILE NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID PRESERVE-PERMISSIONS)"
  (error 'unimplemented-error))
(defun* default-file-modes ()
  #M"Return the default file protection for created files.
The value is an integer.

(fn)"
  (error 'unimplemented-error))
(defun* delete-directory-internal ()
  #M"Delete the directory named DIRECTORY.  Does not follow symlinks.

(fn DIRECTORY)"
  (error 'unimplemented-error))
(defun* delete-file ()
  #M"Delete file named FILENAME.  If it is a symlink, remove the symlink.
If file has multiple names, it continues to exist with the other names.
TRASH non-nil means to trash the file instead of deleting, provided
‘delete-by-moving-to-trash' is non-nil.

When called interactively, TRASH is t if no prefix argument is given.
With a prefix argument, TRASH is nil.

(fn FILENAME &optional TRASH)"
  (error 'unimplemented-error))
(defun* directory-file-name ()
  #M"Returns the file name of the directory named DIRECTORY.
This is the name of the file that holds the data for the directory DIRECTORY.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
In Unix-syntax, this function just removes the final slash.

(fn DIRECTORY)"
  (error 'unimplemented-error))
(defun* directory-name-p ()
  #M"Return non-nil if NAME ends with a directory separator character.

(fn NAME)"
  (error 'unimplemented-error))
(defun* do-auto-save ()
  #M"Auto-save all buffers that need it.
This auto-saves all buffers that have auto-saving enabled and
were changed since last auto-saved.

Auto-saving writes the buffer into a file so that your edits are
not lost if the system crashes.

The auto-save file is not the file you visited; that changes only
when you save.

Normally, run the normal hook ‘auto-save-hook' before saving.

A non-nil NO-MESSAGE argument means do not print any message if successful.

A non-nil CURRENT-ONLY argument means save only current buffer.

(fn &optional NO-MESSAGE CURRENT-ONLY)"
  (error 'unimplemented-error))
(defun* expand-file-name ()
  #M"Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
(does not start with slash or tilde); both the directory name and
a directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or
missing, the current buffer's value of ‘default-directory' is used.
NAME should be a string that is a valid file name for the underlying
filesystem.

File name components that are ‘.' are removed, and so are file name
components followed by ‘..', along with the ‘..' itself; note that
these simplifications are done without checking the resulting file
names in the file system.

Multiple consecutive slashes are collapsed into a single slash, except
at the beginning of the file name when they are significant (e.g., UNC
file names on MS-Windows.)

An initial \"~\" in NAME expands to your home directory.

An initial \"~USER\" in NAME expands to USER's home directory.  If
USER doesn't exist, \"~USER\" is not expanded.

To do other file name substitutions, see ‘substitute-in-file-name'.

For technical reasons, this function can return correct but
non-intuitive results for the root directory; for instance,
(expand-file-name \"..\" \"/\") returns \"/..\".  For this reason, use
(directory-file-name (file-name-directory dirname)) to traverse a
filesystem tree, not (expand-file-name \"..\" dirname).  Note: make
sure DIRNAME in this example doesn't end in a slash, unless it's
the root directory.

(fn NAME &optional DEFAULT-DIRECTORY)"
  (error 'unimplemented-error))
(defun* file-accessible-directory-p ()
  #M"Return t if FILENAME names a directory you can open.
This means that FILENAME must specify the name of a directory, and the
directory must allow you to open files in it.  If this isn't the case,
return nil.

FILENAME can either be a directory name (eg. \"/tmp/foo/\") or the
file name of a file which is a directory (eg. \"/tmp/foo\", without
the final slash).

In order to use a directory as a buffer's current directory, this
predicate must return true.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-acl ()
  #M"Return ACL entries of file named FILENAME.
The entries are returned in a format suitable for use in ‘set-file-acl'
but is otherwise undocumented and subject to change.
Return nil if file does not exist.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-directory-p ()
  #M"Return t if FILENAME names an existing directory.
Return nil if FILENAME does not name a directory, or if there
was trouble determining whether FILENAME is a directory.

As a special case, this function will also return t if FILENAME is the
empty string (\"\").  This quirk is due to Emacs interpreting the
empty string (in some cases) as the current directory.

Symbolic links to directories count as directories.
See ‘file-symlink-p' to distinguish symlinks.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-executable-p ()
  #M"Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.
(It is generally better to use ‘file-accessible-directory-p' for that
purpose, though.)

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-exists-p ()
  #M"Return t if file FILENAME exists (whether or not you can read it).
Return nil if FILENAME does not exist, or if there was trouble
determining whether the file exists.
See also ‘file-readable-p' and ‘file-attributes'.
This returns nil for a symlink to a nonexistent file.
Use ‘file-symlink-p' to test for such links.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-modes ()
  #M"Return mode bits of file named FILENAME, as an integer.
Return nil if FILENAME does not exist.  If optional FLAG is ‘nofollow',
do not follow FILENAME if it is a symbolic link.

(fn FILENAME &optional FLAG)"
  (error 'unimplemented-error))
(defun* file-name-absolute-p ()
  #M"Return t if FILENAME is an absolute file name.
On Unix, absolute file names start with ‘/'.  In Emacs, an absolute
file name can also start with an initial ‘~' or ‘~USER' component,
where USER is a valid login name.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-name-as-directory ()
  #M"Return a string representing the file name FILE interpreted as a directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
The result can be used as the value of ‘default-directory'
or passed as second argument to ‘expand-file-name'.
For a Unix-syntax file name, just appends a slash unless a trailing slash
is already present.

(fn FILE)"
  (error 'unimplemented-error))
(defun* file-name-case-insensitive-p ()
  #M"Return t if file FILENAME is on a case-insensitive filesystem.
Return nil if FILENAME does not exist or is not on a case-insensitive
filesystem, or if there was trouble determining whether the filesystem
is case-insensitive.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-name-concat ()
  #M"Append COMPONENTS to DIRECTORY and return the resulting string.
Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don't end with a slash, a slash will be
inserted before contatenating.

(fn DIRECTORY &rest COMPONENTS)"
  (error 'unimplemented-error))
(defun* file-name-directory ()
  #M"Return the directory component in file name FILENAME.
Return nil if FILENAME does not include a directory.
Otherwise return a directory name.
Given a Unix syntax file name, returns a string ending in slash.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-name-nondirectory ()
  #M"Return file name FILENAME sans its directory.
For example, in a Unix-syntax file name,
this is everything after the last slash,
or the entire name if it contains no slash.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-newer-than-file-p ()
  #M"Return t if file FILE1 is newer than file FILE2.
If FILE1 does not exist, the answer is nil;
otherwise, if FILE2 does not exist, the answer is t.

(fn FILE1 FILE2)"
  (error 'unimplemented-error))
(defun* file-readable-p ()
  #M"Return t if file FILENAME exists and you can read it.
See also ‘file-exists-p' and ‘file-attributes'.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-regular-p ()
  #M"Return t if FILENAME names a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
Return nil if FILENAME does not exist or is not a regular file,
or there was trouble determining whether FILENAME is a regular file.
Symbolic links to regular files count as regular files.
See ‘file-symlink-p' to distinguish symlinks.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-selinux-context ()
  #M"Return SELinux context of file named FILENAME.
The return value is a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the user, role, type, and range of the
file's SELinux security context.

Return (nil nil nil nil) if the file is nonexistent,
or if SELinux is disabled, or if Emacs lacks SELinux support.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-symlink-p ()
  #M"Return non-nil if file FILENAME is the name of a symbolic link.
The value is the link target, as a string.
Return nil if FILENAME does not exist or is not a symbolic link,
of there was trouble determining whether the file is a symbolic link.

This function does not check whether the link target exists.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-system-info ()
  #M"Return storage information about the file system FILENAME is on.
Value is a list of numbers (TOTAL FREE AVAIL), where TOTAL is the total
storage of the file system, FREE is the free storage, and AVAIL is the
storage available to a non-superuser.  All 3 numbers are in bytes.
If the underlying system call fails, value is nil.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* file-writable-p ()
  #M"Return t if file FILENAME can be written or created by you.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* find-file-name-handler ()
  #M"Return FILENAME's handler function for OPERATION, if it has one.
Otherwise, return nil.
A file name is handled if one of the regular expressions in
‘file-name-handler-alist' matches it.

If OPERATION equals ‘inhibit-file-name-operation', then ignore
any handlers that are members of ‘inhibit-file-name-handlers',
but still do run any other handlers.  This lets handlers
use the standard functions without calling themselves recursively.

(fn FILENAME OPERATION)"
  (error 'unimplemented-error))
(defun* insert-file-contents ()
  #M"Insert contents of file FILENAME after point.
Returns list of absolute file name and number of characters inserted.
If second argument VISIT is non-nil, the buffer's visited filename and
last save file modtime are set, and it is marked unmodified.  If
visiting and the file does not exist, visiting is completed before the
error is signaled.

The optional third and fourth arguments BEG and END specify what portion
of the file to insert.  These arguments count bytes in the file, not
characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.

When inserting data from a special file (e.g., /dev/urandom), you
can't specify VISIT or BEG, and END should be specified to avoid
inserting unlimited data into the buffer.

If optional fifth argument REPLACE is non-nil, replace the current
buffer contents (in the accessible portion) with the file contents.
This is better than simply deleting and inserting the whole thing
because (1) it preserves some marker positions (in unchanged portions
at the start and end of the buffer) and (2) it puts less data in the
undo list.  When REPLACE is non-nil, the second return value is the
number of characters that replace previous buffer contents.

This function does code conversion according to the value of
‘coding-system-for-read' or ‘file-coding-system-alist', and sets the
variable ‘last-coding-system-used' to the coding system actually used.

In addition, this function decodes the inserted text from known formats
by calling ‘format-decode', which see.

(fn FILENAME &optional VISIT BEG END REPLACE)"
  (error 'unimplemented-error))
(defun* make-directory-internal ()
  #M"Create a new directory named DIRECTORY.

(fn DIRECTORY)"
  (error 'unimplemented-error))
(defun* make-symbolic-link ()
  #M"Make a symbolic link to TARGET, named LINKNAME.
If LINKNAME is a directory name, make a like-named symbolic link under
LINKNAME.

Signal a ‘file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if LINKNAME already
exists, and expand leading \"~\" or strip leading \"/:\" in TARGET.
This happens for interactive use with M-x.

(fn TARGET LINKNAME &optional OK-IF-ALREADY-EXISTS)"
  (error 'unimplemented-error))
(defun* make-temp-file-internal ()
  #M"Generate a new file whose name starts with PREFIX, a string.
Return the name of the generated file.  If DIR-FLAG is zero, do not
create the file, just its name.  Otherwise, if DIR-FLAG is non-nil,
create an empty directory.  The file name should end in SUFFIX.
Do not expand PREFIX; a non-absolute PREFIX is relative to the Emacs
working directory.  If TEXT is a string, insert it into the newly
created file.

Signal an error if the file could not be created.

This function does not grok magic file names.

(fn PREFIX DIR-FLAG SUFFIX TEXT)"
  (error 'unimplemented-error))
(defun* make-temp-name ()
  #M"Generate temporary file name (string) starting with PREFIX (a string).

This function tries to choose a name that has no existing file.
For this to work, PREFIX should be an absolute file name, and PREFIX
and the returned string should both be non-magic.

There is a race condition between calling ‘make-temp-name' and
later creating the file, which opens all kinds of security holes.
For that reason, you should normally use ‘make-temp-file' instead.

(fn PREFIX)"
  (error 'unimplemented-error))
(defun* next-read-file-uses-dialog-p ()
  #M"Return t if a call to ‘read-file-name' will use a dialog.
The return value is only relevant for a call to ‘read-file-name' that happens
before any other event (mouse or keypress) is handled.

(fn)"
  (error 'unimplemented-error))
(defun* recent-auto-save-p ()
  #M"Return t if current buffer has been auto-saved recently.
More precisely, if it has been auto-saved since last read from or saved
in the visited file.  If the buffer has no visited file,
then any auto-save counts as \"recent\".

(fn)"
  (error 'unimplemented-error))
(defun* rename-file ()
  #M"Rename FILE as NEWNAME.  Both args must be strings.
If file has names other than FILE, it continues to have those names.
If NEWNAME is a directory name, rename FILE to a like-named file under
NEWNAME.  For NEWNAME to be recognized as a directory name, it should
end in a slash.

Signal a ‘file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
An integer third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.

(fn FILE NEWNAME &optional OK-IF-ALREADY-EXISTS)"
  (error 'unimplemented-error))
(defun* set-binary-mode ()
  #M"Switch STREAM to binary I/O mode or text I/O mode.
STREAM can be one of the symbols ‘stdin', ‘stdout', or ‘stderr'.
If MODE is non-nil, switch STREAM to binary mode, otherwise switch
it to text mode.

As a side effect, this function flushes any pending STREAM's data.

Value is the previous value of STREAM's I/O mode, nil for text mode,
non-nil for binary mode.

On MS-Windows and MS-DOS, binary mode is needed to read or write
arbitrary binary data, and for disabling translation between CR-LF
pairs and a single newline character.  Examples include generation
of text files with Unix-style end-of-line format using ‘princ' in
batch mode, with standard output redirected to a file.

On Posix systems, this function always returns non-nil, and has no
effect except for flushing STREAM's data.

(fn STREAM MODE)"
  (error 'unimplemented-error))
(defun* set-buffer-auto-saved ()
  #M"Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again.

(fn)"
  (error 'unimplemented-error))
(defun* set-default-file-modes ()
  #M"Set the file permission bits for newly created files.
The argument MODE should be an integer; only the low 9 bits are used.
On Posix hosts, this setting is inherited by subprocesses.

This function works by setting the Emacs's file mode creation mask.
Each bit that is set in the mask means that the corresponding bit
in the permissions of newly created files will be disabled.

Note that when ‘write-region' creates a file, it resets the
execute bit, even if the mask set by this function allows that bit
by having the corresponding bit in the mask reset.

(fn MODE)"
  (error 'unimplemented-error))
(defun* set-file-acl ()
  #M"Set ACL of file named FILENAME to ACL-STRING.
ACL-STRING should contain the textual representation of the ACL
entries in a format suitable for the platform.

Value is t if setting of ACL was successful, nil otherwise.

Setting ACL for local files requires Emacs to be built with ACL
support.

(fn FILENAME ACL-STRING)"
  (error 'unimplemented-error))
(defun* set-file-modes ()
  #M"Set mode bits of file named FILENAME to MODE (an integer).
Only the 12 low bits of MODE are used.  If optional FLAG is ‘nofollow',
do not follow FILENAME if it is a symbolic link.

Interactively, prompt for FILENAME, and read MODE with
‘read-file-modes', which accepts symbolic notation, like the ‘chmod'
command from GNU Coreutils.

(fn FILENAME MODE &optional FLAG)"
  (error 'unimplemented-error))
(defun* set-file-selinux-context ()
  #M"Set SELinux context of file named FILENAME to CONTEXT.
CONTEXT should be a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the components of a SELinux context.

Value is t if setting of SELinux context was successful, nil otherwise.

This function does nothing and returns nil if SELinux is disabled,
or if Emacs was not compiled with SELinux support.

(fn FILENAME CONTEXT)"
  (error 'unimplemented-error))
(defun* set-file-times ()
  #M"Set times of file FILENAME to TIMESTAMP.
If optional FLAG is ‘nofollow', do not follow FILENAME if it is a
symbolic link.  Set both access and modification times.  Return t on
success, else nil.  Use the current time if TIMESTAMP is nil.
TIMESTAMP is in the format of ‘current-time'.

(fn FILENAME &optional TIMESTAMP FLAG)"
  (error 'unimplemented-error))
(defun* set-visited-file-modtime ()
  #M"Update buffer's recorded modification time from the visited file's time.
Useful if the buffer was not read from the file normally
or if the file itself has been changed for some known benign reason.
An argument specifies the modification time value to use
(instead of that of the visited file), in the form of a time value as
in ‘current-time' or an integer flag as returned by ‘visited-file-modtime'.

(fn &optional TIME-FLAG)"
  (error 'unimplemented-error))
(defun* substitute-in-file-name ()
  #M"Substitute environment variables referred to in FILENAME.
‘$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.

If FOO is not defined in the environment, ‘$FOO' is left unchanged in
the value of this function.

If ‘/~' appears, all of FILENAME through that ‘/' is discarded.
If ‘//' appears, everything up to and including the first of
those ‘/' is discarded.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* unhandled-file-name-directory ()
  #M"Return a directly usable directory name somehow associated with FILENAME.
A ‘directly usable' directory name is one that may be used without the
intervention of any file name handler.
If FILENAME is a directly usable file itself, return
(file-name-as-directory FILENAME).
If FILENAME refers to a file which is not accessible from a local process,
then this should return nil.
The ‘call-process' and ‘start-process' functions use this function to
get a current directory to run processes in.

(fn FILENAME)"
  (error 'unimplemented-error))
(defun* unix-sync ()
  #M"Tell Unix to finish all pending disk updates.

(fn)"
  (error 'unimplemented-error))
(defun* verify-visited-file-modtime ()
  #M"Return t if last mod time of BUF's visited file matches what BUF records.
This means that the file has not been changed since it was visited or saved.
If BUF is omitted or nil, it defaults to the current buffer.
See Info node ‘(elisp)Modification Time' for more details.

(fn &optional BUF)"
  (error 'unimplemented-error))
(defun* visited-file-modtime ()
  #M"Return the current buffer's recorded visited file modification time.
Return a Lisp timestamp (as in ‘current-time') if the current buffer
has a recorded file modification time, 0 if it doesn't, and -1 if the
visited file doesn't exist.
See Info node ‘(elisp)Modification Time' for more details.

(fn)"
  (error 'unimplemented-error))
(defun* write-region ()
  #M"Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents; END is
ignored.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is a number,
  seek to that offset in the file before writing.
Optional fifth argument VISIT, if t or a string, means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is t, the buffer is marked as visiting FILENAME.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string, or if Emacs is in batch mode,
  do not display the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
The optional seventh arg MUSTBENEW, if non-nil, insists on a check
  for an existing file with the same name.  If MUSTBENEW is ‘excl',
  that means to get an error if the file already exists; never overwrite.
  If MUSTBENEW is neither nil nor ‘excl', that means ask for
  confirmation before overwriting, but do go ahead and overwrite the file
  if the user confirms.

This does code conversion according to the value of
‘coding-system-for-write', ‘buffer-file-coding-system', or
‘file-coding-system-alist', and sets the variable
‘last-coding-system-used' to the coding system actually used.

This calls ‘write-region-annotate-functions' at the start, and
‘write-region-post-annotation-function' at the end.

(fn START END FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)"
  (error 'unimplemented-error))
