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

(uiop:define-package :cl-emacs/timefns
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/timefns)
(log-enable :cl-emacs/timefns :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* current-cpu-time () "Return the current CPU time along with its resolution.
The return value is a pair (CPU-TICKS . TICKS-PER-SEC).
The CPU-TICKS counter can wrap around, so values cannot be meaningfully
compared if too much time has passed between them.

(fn)"
  (error ’unimplemented-error))
(defun* current-time () "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
If the variable ‘current-time-list’ is nil, the time is returned as a
pair of integers (TICKS . HZ), where TICKS counts clock ticks and HZ
is the clock ticks per second.  Otherwise, the time is returned as a
list of integers (HIGH LOW USEC PSEC) where HIGH has the most
significant bits of the seconds, LOW has the least significant 16
bits, and USEC and PSEC are the microsecond and picosecond counts.

You can use ‘time-convert’ to get a particular timestamp form
regardless of the value of ‘current-time-list’.

(fn)"
  (error ’unimplemented-error))
(defun* current-time-string () "Return the current local time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed
if the year is in the range 1000-9999.
The format is ‘Sun Sep 16 01:03:52 1973’.
However, see also the functions ‘decode-time’ and ‘format-time-string’
which provide a much more powerful and general facility.

If SPECIFIED-TIME is given, it is the time value to format instead of
the current time.  See ‘format-time-string’ for the various forms of a
time value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
‘current-time-zone’) or an integer (as from ‘decode-time’) applied
without consideration for daylight saving time.

(fn &optional SPECIFIED-TIME ZONE)"
  (error ’unimplemented-error))
(defun* current-time-zone () "Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If SPECIFIED-TIME is given, the time zone offset is determined from it
instead of using the current time.  The argument should be a Lisp
time value; see ‘format-time-string’ for the various forms of a time
value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
‘current-time-zone’) or an integer (as from ‘decode-time’) applied
without consideration for daylight saving time.

Some operating systems cannot provide all this information to Emacs;
in this case, ‘current-time-zone’ returns a list containing nil for
the data it can’t find.

(fn &optional SPECIFIED-TIME ZONE)"
  (error ’unimplemented-error))
(defun* decode-time () "Decode a timestamp into (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).
The optional TIME is the time value to convert.  See
‘format-time-string’ for the various forms of a time value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
‘current-time-zone’) or an integer (the UTC offset in seconds) applied
without consideration for daylight saving time.

The optional FORM specifies the form of the SEC member.  If ‘integer’,
SEC is an integer; if t, SEC is an integer or (TICKS . HZ) timestamp
with the same precision as TIME.  An omitted or nil FORM is currently
treated like ‘integer’, but this may change in future Emacs versions.

To access (or alter) the elements in the time value, the
‘decoded-time-second’, ‘decoded-time-minute’, ‘decoded-time-hour’,
‘decoded-time-day’, ‘decoded-time-month’, ‘decoded-time-year’,
‘decoded-time-weekday’, ‘decoded-time-dst’ and ‘decoded-time-zone’
accessors can be used.

The list has the following nine members: SEC is an integer or
Lisp timestamp representing a nonnegative value less than 60
(or less than 61 if the operating system supports leap seconds).
MINUTE is an integer between 0 and 59.  HOUR is an integer
between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
integer between 1 and 12.  YEAR is the year number, an integer; 0
represents 1 BC.  DOW is the day of week, an integer between 0 and 6,
where 0 is Sunday.  DST is t if daylight saving time is in effect,
nil if it is not in effect, and -1 if daylight saving information is
not available.  UTCOFF is an integer indicating the UTC offset in
seconds, i.e., the number of seconds east of Greenwich.  (Note that
Common Lisp has different meanings for DOW and UTCOFF, and its
SEC is always an integer between 0 and 59.)

(fn &optional TIME ZONE FORM)"
  (error ’unimplemented-error))
(defun* encode-time () "Convert TIME to a timestamp.

TIME is a list (SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE)
in the style of ‘decode-time’, so that (encode-time (decode-time ...)) works.
In this list, ZONE can be nil for Emacs local time, t for Universal
Time, ‘wall’ for system wall clock time, or a string as in the TZ
environment variable.  ZONE can also be a list (as from
‘current-time-zone’) or an integer (as from ‘decode-time’) applied
without consideration for daylight saving time.  If ZONE specifies a
time zone with daylight-saving transitions, DST is t for daylight
saving time, nil for standard time, and -1 to cause the daylight
saving flag to be guessed.

TIME can also be a list (SECOND MINUTE HOUR DAY MONTH YEAR), which is
equivalent to (SECOND MINUTE HOUR DAY MONTH YEAR nil -1 nil).

As an obsolescent calling convention, if this function is called with
6 or more arguments, the first 6 arguments are SECOND, MINUTE, HOUR,
DAY, MONTH, and YEAR, and specify the components of a decoded time.
If there are more than 6 arguments the *last* argument is used as ZONE
and any other extra arguments are ignored, so that (apply
#\\=’encode-time (decode-time ...)) works.  In this obsolescent
convention, DST is -1 and ZONE defaults to nil.

The range of supported years is at least 1970 to the near future.
Out-of-range values for SECOND through MONTH are brought into range
via date arithmetic.  This can be tricky especially when combined with
DST; see Info node ‘(elisp)Time Conversion’ for details and caveats.

(fn TIME &rest OBSOLESCENT-ARGUMENTS)"
  (error ’unimplemented-error))
(defun* float-time () "Return the current time, as a float number of seconds since the epoch.
If SPECIFIED-TIME is given, it is a time value to convert to float
instead of the current time.  See ‘format-time-string’ for the various
forms of a time value.

WARNING: Since the result is floating point, it may not be exact.
If precise time stamps are required, use either ‘time-convert’,
or (if you need time as a string) ‘format-time-string’.

(fn &optional SPECIFIED-TIME)"
  (error ’unimplemented-error))
(defun* set-time-zone-rule () "Set the Emacs local time zone using TZ, a string specifying a time zone rule.
If TZ is nil or ‘wall’, use system wall clock time; this differs from
the usual Emacs convention where nil means current local time.  If TZ
is t, use Universal Time.  If TZ is a list (as from
‘current-time-zone’) or an integer (as from ‘decode-time’), use the
specified time zone without consideration for daylight saving time.

Instead of calling this function, you typically want something else.
To temporarily use a different time zone rule for just one invocation
of ‘decode-time’, ‘encode-time’, or ‘format-time-string’, pass the
function a ZONE argument.  To change local time consistently
throughout Emacs, call (setenv \"TZ\" TZ): this changes both the
environment of the Emacs process and the variable
‘process-environment’, whereas ‘set-time-zone-rule’ affects only the
former.

(fn TZ)"
  (error ’unimplemented-error))
(defun* time-add () "Return the sum of two time values A and B, as a time value.
See ‘format-time-string’ for the various forms of a time value.
For example, nil stands for the current time.

(fn A B)"
  (error ’unimplemented-error))
(defun* time-convert () "Convert TIME value to a Lisp timestamp of the given FORM.
Truncate the returned value toward minus infinity.

If FORM is a positive integer, return a pair of integers (TICKS . FORM),
where TICKS is the number of clock ticks and FORM is the clock frequency
in ticks per second.

If FORM is t, return (TICKS . PHZ), where PHZ is a suitable clock
frequency in ticks per second.

If FORM is ‘integer’, return an integer count of seconds.

If FORM is ‘list’, return an integer list (HIGH LOW USEC PSEC), where
HIGH has the most significant bits of the seconds, LOW has the least
significant 16 bits, and USEC and PSEC are the microsecond and
picosecond counts.

If FORM is nil, the behavior depends on ‘current-time-list’,
but new code should not rely on it.

(fn TIME &optional FORM)"
  (error ’unimplemented-error))
(defun* time-equal-p () "Return non-nil if A and B are equal time values.
See ‘format-time-string’ for the various forms of a time value.

(fn A B)"
  (error ’unimplemented-error))
(defun* time-less-p () "Return non-nil if time value A is less than time value B.
See ‘format-time-string’ for the various forms of a time value.
For example, nil stands for the current time.

(fn A B)"
  (error ’unimplemented-error))
(defun* time-subtract () "Return the difference between two time values A and B, as a time value.
You can use ‘float-time’ to convert the difference into elapsed seconds.
See ‘format-time-string’ for the various forms of a time value.
For example, nil stands for the current time.

(fn A B)"
  (error ’unimplemented-error))
