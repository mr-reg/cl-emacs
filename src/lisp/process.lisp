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

(uiop:define-package :cl-emacs/process
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/process)
(log-enable :cl-emacs/process :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* accept-process-output () "Allow any pending output from subprocesses to be read by Emacs.
It is given to their filter functions.
Optional argument PROCESS means to return only after output is
received from PROCESS or PROCESS closes the connection.

Optional second argument SECONDS and third argument MILLISEC
specify a timeout; return after that much time even if there is
no subprocess output.  If SECONDS is a floating point number,
it specifies a fractional number of seconds to wait.
The MILLISEC argument is obsolete and should be avoided.

If optional fourth argument JUST-THIS-ONE is non-nil, accept output
from PROCESS only, suspending reading output from other processes.
If JUST-THIS-ONE is an integer, don’t run any timers either.
Return non-nil if we received any output from PROCESS (or, if PROCESS
is nil, from any process) before the timeout expired or the
corresponding connection was closed.

(fn &optional PROCESS SECONDS MILLISEC JUST-THIS-ONE)"
  (error ’unimplemented-error))
(defun* continue-process () "Continue process PROCESS.  May be process or name of one.
See function ‘interrupt-process’ for more details on usage.
If PROCESS is a network or serial process, resume handling of incoming
traffic.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* delete-process () "Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer’s process.

Interactively, it will kill the current buffer’s process.

(fn &optional PROCESS)"
  (error ’unimplemented-error))
(defun* format-network-address () "Convert network ADDRESS from internal format to a string.
A 4 or 5 element vector represents an IPv4 address (with port number).
An 8 or 9 element vector represents an IPv6 address (with port number).
If optional second argument OMIT-PORT is non-nil, don’t include a port
number in the string, even when present in ADDRESS.
Return nil if format of ADDRESS is invalid.

(fn ADDRESS &optional OMIT-PORT)"
  (error ’unimplemented-error))
(defun* get-buffer-process () "Return the (or a) live process associated with BUFFER.
BUFFER may be a buffer or the name of one.
Return nil if all processes associated with BUFFER have been
deleted or killed.

(fn BUFFER)"
  (error ’unimplemented-error))
(defun* get-process () "Return the process named NAME, or nil if there is none.

(fn NAME)"
  (error ’unimplemented-error))
(defun* internal-default-interrupt-process () "Default function to interrupt process PROCESS.
It shall be the last element in list ‘interrupt-process-functions’.
See function ‘interrupt-process’ for more details on usage.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* internal-default-process-filter () "Function used as default process filter.
This inserts the process’s output into its buffer, if there is one.
Otherwise it discards the output.

(fn PROC TEXT)"
  (error ’unimplemented-error))
(defun* internal-default-process-sentinel () "Function used as default sentinel for processes.
This inserts a status message into the process’s buffer, if there is one.

(fn PROC MSG)"
  (error ’unimplemented-error))
(defun* internal-default-signal-process () "Default function to send PROCESS the signal with code SIGCODE.
It shall be the last element in list ‘signal-process-functions’.
See function ‘signal-process’ for more details on usage.

(fn PROCESS SIGCODE &optional REMOTE)"
  (error ’unimplemented-error))
(defun* interrupt-process () "Interrupt process PROCESS.
PROCESS may be a process, a buffer, or the name of a process or buffer.
No arg or nil means current buffer’s process.
Second arg CURRENT-GROUP non-nil means send signal to
the current process-group of the process’s controlling terminal
rather than to the process’s own process group.
If the process is a shell, this means interrupt current subjob
rather than the shell.

If CURRENT-GROUP is ‘lambda’, and if the shell owns the terminal,
don’t send the signal.

This function calls the functions of ‘interrupt-process-functions’ in
the order of the list, until one of them returns non-nil.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* kill-process () "Kill process PROCESS.  May be process or name of one.
See function ‘interrupt-process’ for more details on usage.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* list-system-processes () "Return a list of numerical process IDs of all running processes.
If this functionality is unsupported, return nil.
If ‘default-directory’ is remote, return process IDs of the respective remote host.

See ‘process-attributes’ for getting attributes of a process given its ID.

(fn)"
  (error ’unimplemented-error))
(defun* make-network-process () "Create and return a network server or client process.

In Emacs, network connections are represented by process objects, so
input and output work as for subprocesses and ‘delete-process’ closes
a network connection.  However, a network process has no process id,
it cannot be signaled, and the status codes are different from normal
processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify a filter function to handle the output.  BUFFER may be
also nil, meaning that this process is not associated with any buffer.

:host HOST -- HOST is name of the host to connect to, or its IP
address.  The symbol ‘local’ specifies the local host.  If specified
for a server process, it must be a valid name or address for the local
host, and only clients connecting to that address will be accepted.
If all interfaces should be bound, an address of \"0.0.0.0\" (for
IPv4) or \"::\" (for IPv6) can be used.  (On some operating systems,
using \"::\" listens on both IPv4 and IPv6.)  ‘local’ will use IPv4 by
default, use a FAMILY of ‘ipv6’ to override this.

:service SERVICE -- SERVICE is name of the service desired, or an
integer specifying a port number to connect to.  If SERVICE is t,
a random port number is selected for the server.  A port number can
be specified as an integer string, e.g., \"80\", as well as an integer.

:type TYPE -- TYPE is the type of connection.  The default (nil) is a
stream type connection, ‘datagram’ creates a datagram type connection,
‘seqpacket’ creates a reliable datagram connection.

:family FAMILY -- FAMILY is the address (and protocol) family for the
service specified by HOST and SERVICE.  The default (nil) is to use
whatever address family (IPv4 or IPv6) that is defined for the host
and port number specified by HOST and SERVICE.  Other address families
supported are:
  local -- for a local (i.e. UNIX) address specified by SERVICE.
  ipv4  -- use IPv4 address family only.
  ipv6  -- use IPv6 address family only.

:local ADDRESS -- ADDRESS is the local address used for the connection.
This parameter is ignored when opening a client process. When specified
for a server process, the FAMILY, HOST and SERVICE args are ignored.

:remote ADDRESS -- ADDRESS is the remote partner’s address for the
connection.  This parameter is ignored when opening a stream server
process.  For a datagram server process, it specifies the initial
setting of the remote datagram address.  When specified for a client
process, the FAMILY, HOST, and SERVICE args are ignored.

The format of ADDRESS depends on the address family:
- An IPv4 address is represented as a vector of integers [A B C D P]
corresponding to numeric IP address A.B.C.D and port number P.
- An IPv6 address has the same format as an IPv4 address but with 9
elements rather than 5.
- A local address is represented as a string with the address in the
local address space.
- An \"unsupported family\" address is represented by a cons (F . AV)
where F is the family number and AV is a vector containing the socket
address data with one element per address data byte.  Do not rely on
this format in portable code, as it may depend on implementation
defined constants, data sizes, and data structure alignment.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:nowait BOOL -- If NOWAIT is non-nil for a stream type client
process, return without waiting for the connection to complete;
instead, the sentinel function will be called with second arg matching
\"open\" (if successful) or \"failed\" when the connect completes.
Default is to use a blocking connect (i.e. wait) for stream type
connections.

:noquery BOOL -- Query the user unless BOOL is non-nil, and process is
running when Emacs is exited.

:stop BOOL -- Start process in the ‘stopped’ state if BOOL non-nil.
In the stopped state, a server process does not accept new
connections, and a client process does not handle incoming traffic.
The stopped state is cleared by ‘continue-process’ and set by
‘stop-process’.

:filter FILTER -- Install FILTER as the process filter.

:filter-multibyte BOOL -- If BOOL is non-nil, strings given to the
process filter are multibyte, otherwise they are unibyte.
If this keyword is not specified, the strings are multibyte.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:log LOG -- Install LOG as the server process log function.  This
function is called when the server accepts a network connection from a
client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
is the server process, CLIENT is the new process for the connection,
and MESSAGE is a string.

:plist PLIST -- Install PLIST as the new process’s initial plist.

:tls-parameters LIST -- is a list that should be supplied if you’re
opening a TLS connection.  The first element is the TLS type (either
‘gnutls-x509pki’ or ‘gnutls-anon’), and the remaining elements should
be a keyword list accepted by gnutls-boot (as returned by
‘gnutls-boot-parameters’).

:server QLEN -- if QLEN is non-nil, create a server process for the
specified FAMILY, SERVICE, and connection type (stream or datagram).
If QLEN is an integer, it is used as the max. length of the server’s
pending connection queue (also known as the backlog); the default
queue length is 5.  Default is to create a client process.

The following network options can be specified for this connection:

:broadcast BOOL    -- Allow send and receive of datagram broadcasts.
:dontroute BOOL    -- Only send to directly connected hosts.
:keepalive BOOL    -- Send keep-alive messages on network stream.
:linger BOOL or TIMEOUT -- Send queued messages before closing.
:oobinline BOOL    -- Place out-of-band data in receive data stream.
:priority INT      -- Set protocol defined priority for sent packets.
:reuseaddr BOOL    -- Allow reusing a recently used local address
                      (this is allowed by default for a server process).
:bindtodevice NAME -- bind to interface NAME.  Using this may require
                      special privileges on some systems.
:use-external-socket BOOL -- Use any pre-allocated sockets that have
                             been passed to Emacs.  If Emacs wasn’t
                             passed a socket, this option is silently
                             ignored.


Consult the relevant system programmer’s manual pages for more
information on using these options.


A server process will listen for and accept connections from clients.
When a client connection is accepted, a new network process is created
for the connection with the following parameters:

- The client’s process name is constructed by concatenating the server
process’s NAME and a client identification string.
- If the FILTER argument is non-nil, the client process will not get a
separate process buffer; otherwise, the client’s process buffer is a newly
created buffer named after the server process’s BUFFER name or process
NAME concatenated with the client identification string.
- The connection type and the process filter and sentinel parameters are
inherited from the server process’s TYPE, FILTER and SENTINEL.
- The client process’s contact info is set according to the client’s
addressing information (typically an IP address and a port number).
- The client process’s plist is initialized from the server’s plist.

Notice that the FILTER and SENTINEL args are never used directly by
the server process.  Also, the BUFFER argument is not used directly by
the server process, but via the optional :log function, accepted (and
failed) connections may be logged in the server process’s buffer.

The original argument list, modified with the actual connection
information, is available via the ‘process-contact’ function.

(fn &rest ARGS)"
  (error ’unimplemented-error))
(defun* make-pipe-process () "Create and return a bidirectional pipe process.

In Emacs, pipes are represented by process objects, so input and
output work as for subprocesses, and ‘delete-process’ closes a pipe.
However, a pipe process has no process id, it cannot be signaled,
and the status codes are different from normal processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is the name of the process.  It is modified if necessary to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at the end of that buffer,
unless you specify a filter function to handle the output.  If BUFFER
is not given, the value of NAME is used.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the ‘stopped’ state if BOOL non-nil.
In the stopped state, a pipe process does not accept incoming data,
but you can send outgoing data.  The stopped state is cleared by
‘continue-process’ and set by ‘stop-process’.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

(fn &rest ARGS)"
  (error ’unimplemented-error))
(defun* make-process () "Start a program in a subprocess.  Return the process object for it.

This is similar to ‘start-process’, but arguments are specified as
keyword/argument pairs.  The following arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify a filter function to handle the output.  BUFFER may be
also nil, meaning that this process is not associated with any buffer.

:command COMMAND -- COMMAND is a list starting with the program file
name, followed by strings to give to the program as arguments.  If the
program file name is not an absolute file name, ‘make-process’ will
look for the program file name in ‘exec-path’ (which is a list of
directories).

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- BOOL must be nil.  The ‘:stop’ key is ignored otherwise
and is retained for compatibility with other process types such as
pipe processes.  Asynchronous subprocesses never start in the
‘stopped’ state.  Use ‘stop-process’ and ‘continue-process’ to send
signals to stop and continue a process.

:connection-type TYPE -- TYPE is control type of device used to
communicate with subprocesses.  Values are ‘pipe’ to use a pipe, ‘pty’
to use a pty, or nil to use the default specified through
‘process-connection-type’.  If TYPE is a cons (INPUT . OUTPUT), then
INPUT will be used for standard input and OUTPUT for standard output
(and standard error if ‘:stderr’ is nil).

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:stderr STDERR -- STDERR is either a buffer or a pipe process attached
to the standard error of subprocess.  When specifying this, the
subprocess’s standard error will always communicate via a pipe, no
matter the value of ‘:connection-type’.  If STDERR is nil, standard error
is mixed with standard output and sent to BUFFER or FILTER.  (Note
that specifying :stderr will create a new, separate (but associated)
process, with its own filter and sentinel.  See
Info node ‘(elisp) Asynchronous Processes’ for more details.)

:file-handler FILE-HANDLER -- If FILE-HANDLER is non-nil, then look
for a file name handler for the current buffer’s ‘default-directory’
and invoke that file name handler to make the process.  If there is no
such handler, proceed as if FILE-HANDLER were nil.

(fn &rest ARGS)"
  (error ’unimplemented-error))
(defun* make-serial-process () "Create and return a serial port process.

In Emacs, serial port connections are represented by process objects,
so input and output work as for subprocesses, and ‘delete-process’
closes a serial port connection.  However, a serial process has no
process id, it cannot be signaled, and the status codes are different
from normal processes.

‘make-serial-process’ creates a process and a buffer, on which you
probably want to use ‘process-send-string’.  Try \\[serial-term] for
an interactive terminal.  See below for examples.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:port PORT -- (mandatory) PORT is the path or name of the serial port.
For example, this could be \"/dev/ttyS0\" on Unix.  On Windows, this
could be \"COM1\", or \"\\\\.\\COM10\" for ports higher than COM9 (double
the backslashes in strings).

:speed SPEED -- (mandatory) is handled by ‘serial-process-configure’,
which this function calls.

:name NAME -- NAME is the name of the process.  If NAME is not given,
the value of PORT is used.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at the end of that buffer,
unless you specify a filter function to handle the output.  If BUFFER
is not given, the value of NAME is used.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the ‘stopped’ state if BOOL is non-nil.
In the stopped state, a serial process does not accept incoming data,
but you can send outgoing data.  The stopped state is cleared by
‘continue-process’ and set by ‘stop-process’.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:plist PLIST -- Install PLIST as the initial plist of the process.

:bytesize
:parity
:stopbits
:flowcontrol
-- This function calls ‘serial-process-configure’ to handle these
arguments.

The original argument list, possibly modified by later configuration,
is available via the function ‘process-contact’.

Examples:

(make-serial-process :port \"/dev/ttyS0\" :speed 9600)

(make-serial-process :port \"COM1\" :speed 115200 :stopbits 2)

(make-serial-process :port \"\\\\.\\COM13\" :speed 1200 :bytesize 7 :parity \\=’odd)

(make-serial-process :port \"/dev/tty.BlueConsole-SPP-1\" :speed nil)

(fn &rest ARGS)"
  (error ’unimplemented-error))
(defun* network-interface-info () "Return information about network interface named IFNAME.
The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
NETMASK is the layer 3 network mask, HWADDR is the layer 2 address, and
FLAGS is the current flags of the interface.

Data that is unavailable is returned as nil.

(fn IFNAME)"
  (error ’unimplemented-error))
(defun* network-interface-list () "Return an alist of all network interfaces and their network address.
Each element is cons of the form (IFNAME . IP) where IFNAME is a
string containing the interface name, and IP is the network address in
internal format; see the description of ADDRESS in
‘make-network-process’.  The interface name is not guaranteed to be
unique.

Optional parameter FULL non-nil means return all IP address info for
each interface.  Each element is then a list of the form
    (IFNAME IP BCAST MASK)
where IFNAME is the interface name, IP the IP address,
BCAST the broadcast address, and MASK the network mask.

Optional parameter FAMILY controls the type of addresses to return.
The default of nil means both IPv4 and IPv6, symbol ‘ipv4’ means IPv4
only, symbol ‘ipv6’ means IPv6 only.

See also ‘network-interface-info’, which is limited to IPv4 only.

If the information is not available, return nil.

(fn &optional FULL FAMILY)"
  (error ’unimplemented-error))
(defun* network-lookup-address-info () "Look up Internet Protocol (IP) address info of NAME.
Optional argument FAMILY controls whether to look up IPv4 or IPv6
addresses.  The default of nil means both, symbol ‘ipv4’ means IPv4
only, symbol ‘ipv6’ means IPv6 only.
Optional argument HINTS allows specifying the hints passed to the
underlying library call.  The only supported value is ‘numeric’, which
means treat NAME as a numeric IP address.  This also suppresses DNS
traffic.
Return a list of addresses, or nil if none were found.  Each address
is a vector of integers, as per the description of ADDRESS in
‘make-network-process’.  In case of error log the error message
returned from the lookup.

(fn NAME &optional FAMILY HINT)"
  (error ’unimplemented-error))
(defun* num-processors () "Return the number of processors, a positive integer.
Each usable thread execution unit counts as a processor.
By default, count the number of available processors,
overridable via the OMP_NUM_THREADS environment variable.
If optional argument QUERY is ‘current’, ignore OMP_NUM_THREADS.
If QUERY is ‘all’, also count processors not available.

(fn &optional QUERY)"
  (error ’unimplemented-error))
(defun* process-attributes () "Return attributes of the process given by its PID, a number.
If ‘default-directory’ is remote, PID is regarded as process
identifier on the respective remote host.

Value is an alist where each element is a cons cell of the form

    (KEY . VALUE)

If this functionality is unsupported, the value is nil.

See ‘list-system-processes’ for getting a list of all process IDs.

The KEYs of the attributes that this function may return are listed
below, together with the type of the associated VALUE (in parentheses).
Not all platforms support all of these attributes; unsupported
attributes will not appear in the returned alist.
Unless explicitly indicated otherwise, numbers can have either
integer or floating point values.

 euid    -- Effective user User ID of the process (number)
 user    -- User name corresponding to euid (string)
 egid    -- Effective user Group ID of the process (number)
 group   -- Group name corresponding to egid (string)
 comm    -- Command name (executable name only) (string)
 state   -- Process state code, such as \"S\", \"R\", or \"T\" (string)
 ppid    -- Parent process ID (number)
 pgrp    -- Process group ID (number)
 sess    -- Session ID, i.e. process ID of session leader (number)
 ttname  -- Controlling tty name (string)
 tpgid   -- ID of foreground process group on the process’s tty (number)
 minflt  -- number of minor page faults (number)
 majflt  -- number of major page faults (number)
 cminflt -- cumulative number of minor page faults (number)
 cmajflt -- cumulative number of major page faults (number)
 utime   -- user time used by the process, in ‘current-time’ format
 stime   -- system time used by the process (current-time)
 time    -- sum of utime and stime (current-time)
 cutime  -- user time used by the process and its children (current-time)
 cstime  -- system time used by the process and its children (current-time)
 ctime   -- sum of cutime and cstime (current-time)
 pri     -- priority of the process (number)
 nice    -- nice value of the process (number)
 thcount -- process thread count (number)
 start   -- time the process started (current-time)
 vsize   -- virtual memory size of the process in KB’s (number)
 rss     -- resident set size of the process in KB’s (number)
 etime   -- elapsed time the process is running (current-time)
 pcpu    -- percents of CPU time used by the process (floating-point number)
 pmem    -- percents of total physical memory used by process’s resident set
              (floating-point number)
 args    -- command line which invoked the process (string).

(fn PID)"
  (error ’unimplemented-error))
(defun* process-buffer () "Return the buffer PROCESS is associated with.
The default process filter inserts output from PROCESS into this buffer.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-coding-system () "Return a cons of coding systems for decoding and encoding of PROCESS.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-command () "Return the command that was executed to start PROCESS.
This is a list of strings, the first string being the program executed
and the rest of the strings being the arguments given to it.
For a network or serial or pipe connection, this is nil (process is running)
or t (process is stopped).

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-contact () "Return the contact info of PROCESS; t for a real child.
For a network or serial or pipe connection, the value depends on the
optional KEY arg.  If KEY is nil, value is a cons cell of the form
(HOST SERVICE) for a network connection or (PORT SPEED) for a serial
connection; it is t for a pipe connection.  If KEY is t, the complete
contact information for the connection is returned, else the specific
value for the keyword KEY is returned.  See ‘make-network-process’,
‘make-serial-process’, or ‘make-pipe-process’ for the list of keywords.

If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.
If the optional NO-BLOCK parameter is specified, return nil instead of
waiting for the process to be fully set up.

(fn PROCESS &optional KEY NO-BLOCK)"
  (error ’unimplemented-error))
(defun* process-datagram-address () "Get the current datagram address associated with PROCESS.
If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-exit-status () "Return the exit status of PROCESS or the signal number that killed it.
If PROCESS has not yet exited or died, return 0.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-filter () "Return the filter function of PROCESS.
See ‘set-process-filter’ for more info on filter functions.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-id () "Return the process id of PROCESS.
This is the pid of the external process which PROCESS uses or talks to,
an integer.
For a network, serial, and pipe connections, this value is nil.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-inherit-coding-system-flag () "Return the value of inherit-coding-system flag for PROCESS.
If this flag is t, ‘buffer-file-coding-system’ of the buffer
associated with PROCESS will inherit the coding system used to decode
the process output.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-list () "Return a list of all processes that are Emacs sub-processes.

(fn)"
  (error ’unimplemented-error))
(defun* process-mark () "Return the marker for the end of the last output from PROCESS.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-name () "Return the name of PROCESS, as a string.
This is the name of the program invoked in PROCESS,
possibly modified to make it unique among process names.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-plist () "Return the plist of PROCESS.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-query-on-exit-flag () "Return the current value of query-on-exit flag for PROCESS.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-running-child-p () "Return non-nil if PROCESS has given control of its terminal to a child.
If the operating system does not make it possible to find out, return t.
If it’s possible to find out, return the numeric ID of the foreground
process group if PROCESS did give control of its terminal to a
child process, and return nil if it didn’t.

PROCESS must be a real subprocess, not a connection.

(fn &optional PROCESS)"
  (error ’unimplemented-error))
(defun* process-send-eof () "Make PROCESS see end-of-file in its input.
EOF comes after any text already sent to it.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer’s process.
If PROCESS is a network connection, or is a process communicating
through a pipe (as opposed to a pty), then you cannot send any more
text to PROCESS after you call this function.
If PROCESS is a serial process, wait until all output written to the
process has been transmitted to the serial port.

(fn &optional PROCESS)"
  (error ’unimplemented-error))
(defun* process-send-region () "Send current contents of region as input to PROCESS.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer’s process.
Called from program, takes three arguments, PROCESS, START and END.
If the region is larger than the input buffer of the process (the
length of which depends on the process connection type and the
operating system), it is sent in several bunches.  This may happen
even for shorter regions.  Output from processes can arrive in between
bunches.

If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.

(fn PROCESS START END)"
  (error ’unimplemented-error))
(defun* process-send-string () "Send PROCESS the contents of STRING as input.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer’s process.
If STRING is larger than the input buffer of the process (the length
of which depends on the process connection type and the operating
system), it is sent in several bunches.  This may happen even for
shorter strings.  Output from processes can arrive in between bunches.

If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.

(fn PROCESS STRING)"
  (error ’unimplemented-error))
(defun* process-sentinel () "Return the sentinel of PROCESS.
See ‘set-process-sentinel’ for more info on sentinels.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-status () "Return the status of PROCESS.
The returned value is one of the following symbols:
run  -- for a process that is running.
stop -- for a process stopped but continuable.
exit -- for a process that has exited.
signal -- for a process that has got a fatal signal.
open -- for a network stream connection that is open.
listen -- for a network stream server that is listening.
closed -- for a network stream connection that is closed.
connect -- when waiting for a non-blocking connection to complete.
failed -- when a non-blocking connection has failed.
nil -- if arg is a process name and no such process exists.
PROCESS may be a process, a buffer, the name of a process, or
nil, indicating the current buffer’s process.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-thread () "Return the locking thread of PROCESS.
If PROCESS is unlocked, this function returns nil.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* process-tty-name () "Return the name of the terminal PROCESS uses, or nil if none.
This is the terminal that the process itself reads and writes on,
not the name of the pty that Emacs uses to talk with that terminal.

If STREAM is nil, return the terminal name if any of PROCESS’s
standard streams use a terminal for communication.  If STREAM is one
of ‘stdin’, ‘stdout’, or ‘stderr’, return the name of the terminal
PROCESS uses for that stream specifically, or nil if that stream
communicates via a pipe.

(fn PROCESS &optional STREAM)"
  (error ’unimplemented-error))
(defun* process-type () "Return the connection type of PROCESS.
The value is either the symbol ‘real’, ‘network’, ‘serial’, or ‘pipe’.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer’s process.

(fn PROCESS)"
  (error ’unimplemented-error))
(defun* processp () "Return t if OBJECT is a process.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* quit-process () "Send QUIT signal to process PROCESS.  May be process or name of one.
See function ‘interrupt-process’ for more details on usage.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* serial-process-configure () "Configure speed, bytesize, etc. of a serial process.

Arguments are specified as keyword/argument pairs.  Attributes that
are not given are re-initialized from the process’s current
configuration (available via the function ‘process-contact’) or set to
reasonable default values.  The following arguments are defined:

:process PROCESS
:name NAME
:buffer BUFFER
:port PORT
-- Any of these arguments can be given to identify the process that is
to be configured.  If none of these arguments is given, the current
buffer’s process is used.

:speed SPEED -- SPEED is the speed of the serial port in bits per
second, also called baud rate.  Any value can be given for SPEED, but
most serial ports work only at a few defined values between 1200 and
115200, with 9600 being the most common value.  If SPEED is nil, the
serial port is not configured any further, i.e., all other arguments
are ignored.  This may be useful for special serial ports such as
Bluetooth-to-serial converters which can only be configured through AT
commands.  A value of nil for SPEED can be used only when passed
through ‘make-serial-process’ or ‘serial-term’.

:bytesize BYTESIZE -- BYTESIZE is the number of bits per byte, which
can be 7 or 8.  If BYTESIZE is not given or nil, a value of 8 is used.

:parity PARITY -- PARITY can be nil (don’t use parity), the symbol
‘odd’ (use odd parity), or the symbol ‘even’ (use even parity).  If
PARITY is not given, no parity is used.

:stopbits STOPBITS -- STOPBITS is the number of stopbits used to
terminate a byte transmission.  STOPBITS can be 1 or 2.  If STOPBITS
is not given or nil, 1 stopbit is used.

:flowcontrol FLOWCONTROL -- FLOWCONTROL determines the type of
flowcontrol to be used, which is either nil (don’t use flowcontrol),
the symbol ‘hw’ (use RTS/CTS hardware flowcontrol), or the symbol ‘sw’
(use XON/XOFF software flowcontrol).  If FLOWCONTROL is not given, no
flowcontrol is used.

‘serial-process-configure’ is called by ‘make-serial-process’ for the
initial configuration of the serial port.

Examples:

(serial-process-configure :process \"/dev/ttyS0\" :speed 1200)

(serial-process-configure
    :buffer \"COM1\" :stopbits 1 :parity \\=’odd :flowcontrol \\=’hw)

(serial-process-configure :port \"\\\\.\\COM13\" :bytesize 7)

(fn &rest ARGS)"
  (error ’unimplemented-error))
(defun* set-network-process-option () "For network process PROCESS set option OPTION to value VALUE.
See ‘make-network-process’ for a list of options and values.
If optional fourth arg NO-ERROR is non-nil, don’t signal an error if
OPTION is not a supported option, return nil instead; otherwise return t.

If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.

(fn PROCESS OPTION VALUE &optional NO-ERROR)"
  (error ’unimplemented-error))
(defun* set-process-buffer () "Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
Return BUFFER.

(fn PROCESS BUFFER)"
  (error ’unimplemented-error))
(defun* set-process-coding-system () "Set coding systems of PROCESS to DECODING and ENCODING.
DECODING will be used to decode subprocess output and ENCODING to
encode subprocess input.

(fn PROCESS &optional DECODING ENCODING)"
  (error ’unimplemented-error))
(defun* set-process-datagram-address () "Set the datagram address for PROCESS to ADDRESS.
Return nil upon error setting address, ADDRESS otherwise.

If PROCESS is a non-blocking network process that hasn’t been fully
set up yet, this function will block until socket setup has completed.

(fn PROCESS ADDRESS)"
  (error ’unimplemented-error))
(defun* set-process-filter () "Give PROCESS the filter function FILTER; nil means default.
A value of t means stop accepting output from the process.

When a process has a non-default filter, its buffer is not used for output.
Instead, each time it does output, the entire string of output is
passed to the filter.

The filter gets two arguments: the process and the string of output.
The string argument is normally a multibyte string, except:
- if the process’s input coding system is no-conversion or raw-text,
  it is a unibyte string (the non-converted input).

(fn PROCESS FILTER)"
  (error ’unimplemented-error))
(defun* set-process-inherit-coding-system-flag () "Determine whether buffer of PROCESS will inherit coding-system.
If the second argument FLAG is non-nil, then the variable
‘buffer-file-coding-system’ of the buffer associated with PROCESS
will be bound to the value of the coding system used to decode
the process output.

This is useful when the coding system specified for the process buffer
leaves either the character code conversion or the end-of-line conversion
unspecified, or if the coding system used to decode the process output
is more appropriate for saving the process buffer.

Binding the variable ‘inherit-process-coding-system’ to non-nil before
starting the process is an alternative way of setting the inherit flag
for the process which will run.

This function returns FLAG.

(fn PROCESS FLAG)"
  (error ’unimplemented-error))
(defun* set-process-plist () "Replace the plist of PROCESS with PLIST.  Return PLIST.

(fn PROCESS PLIST)"
  (error ’unimplemented-error))
(defun* set-process-query-on-exit-flag () "Specify if query is needed for PROCESS when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting or killing a buffer if PROCESS is running.  This function
returns FLAG.

(fn PROCESS FLAG)"
  (error ’unimplemented-error))
(defun* set-process-sentinel () "Give PROCESS the sentinel SENTINEL; nil for default.
The sentinel is called as a function when the process changes state.
It gets two arguments: the process, and a string describing the change.

(fn PROCESS SENTINEL)"
  (error ’unimplemented-error))
(defun* set-process-thread () "Set the locking thread of PROCESS to be THREAD.
If THREAD is nil, the process is unlocked.

(fn PROCESS THREAD)"
  (error ’unimplemented-error))
(defun* set-process-window-size () "Tell PROCESS that it has logical window size WIDTH by HEIGHT.
Value is t if PROCESS was successfully told about the window size,
nil otherwise.

(fn PROCESS HEIGHT WIDTH)"
  (error ’unimplemented-error))
(defun* signal-names () "Return a list of known signal names on this system.

(fn)"
  (error ’unimplemented-error))
(defun* signal-process () "Send PROCESS the signal with code SIGCODE.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
If PROCESS is a process object which contains the property
‘remote-pid’, or PROCESS is a number and REMOTE is a remote file name,
PROCESS is interpreted as process on the respective remote host, which
will be the process to signal.
SIGCODE may be an integer, or a symbol whose name is a signal name.

(fn PROCESS SIGCODE &optional REMOTE)"
  (error ’unimplemented-error))
(defun* stop-process () "Stop process PROCESS.  May be process or name of one.
See function ‘interrupt-process’ for more details on usage.
If PROCESS is a network or serial or pipe connection, inhibit handling
of incoming traffic.

(fn &optional PROCESS CURRENT-GROUP)"
  (error ’unimplemented-error))
(defun* waiting-for-user-input-p () "Return non-nil if Emacs is waiting for input from the user.
This is intended for use by asynchronous process output filters and sentinels.

(fn)"
  (error ’unimplemented-error))
