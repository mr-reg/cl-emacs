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

(uiop:define-package :cl-emacs/gnutls
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/gnutls)
(log-enable :cl-emacs/gnutls :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* gnutls-asynchronous-parameters () "Mark this process as being a pre-init GnuTLS process.
The second parameter is the list of parameters to feed to gnutls-boot
to finish setting up the connection.

(fn PROC PARAMS)"
  (error ’unimplemented-error))
(defun* gnutls-boot () "Initialize GnuTLS client for process PROC with TYPE+PROPLIST.
Currently only client mode is supported.  Return a success/failure
value you can check with ‘gnutls-errorp’.

TYPE is a symbol, either ‘gnutls-anon’ or ‘gnutls-x509pki’.
PROPLIST is a property list with the following keys:

:hostname is a string naming the remote host.

:priority is a GnuTLS priority string, defaults to \"NORMAL\".

:trustfiles is a list of PEM-encoded trust files for ‘gnutls-x509pki’.

:crlfiles is a list of PEM-encoded CRL lists for ‘gnutls-x509pki’.

:keylist is an alist of PEM-encoded key files and PEM-encoded
certificates for ‘gnutls-x509pki’.

:callbacks is an alist of callback functions, see below.

:loglevel is the debug level requested from GnuTLS, try 4.

:verify-flags is a bitset as per GnuTLS’
gnutls_certificate_set_verify_flags.

:verify-hostname-error is ignored.  Pass :hostname in :verify-error
instead.

:verify-error is a list of symbols to express verification checks or
t to do all checks.  Currently it can contain ‘:trustfiles’ and
‘:hostname’ to verify the certificate or the hostname respectively.

:min-prime-bits is the minimum accepted number of bits the client will
accept in Diffie-Hellman key exchange.

:complete-negotiation, if non-nil, will make negotiation complete
before returning even on non-blocking sockets.

The debug level will be set for this process AND globally for GnuTLS.
So if you set it higher or lower at any point, it affects global
debugging.

Note that the priority is set on the client.  The server does not use
the protocols’s priority except for disabling protocols that were not
specified.

Processes must be initialized with this function before other GnuTLS
functions are used.  This function allocates resources which can only
be deallocated by calling ‘gnutls-deinit’ or by calling it again.

The callbacks alist can have a ‘verify’ key, associated with a
verification function (UNUSED).

Each authentication type may need additional information in order to
work.  For X.509 PKI (‘gnutls-x509pki’), you probably need at least
one trustfile (usually a CA bundle).

(fn PROC TYPE PROPLIST)"
  (error ’unimplemented-error))
(defun* gnutls-bye () "Terminate current GnuTLS connection for process PROC.
The connection should have been initiated using ‘gnutls-handshake’.

If CONT is not nil the TLS connection gets terminated and further
receives and sends will be disallowed.  If the return value is zero you
may continue using the connection.  If CONT is nil, GnuTLS actually
sends an alert containing a close request and waits for the peer to
reply with the same message.  In order to reuse the connection you
should wait for an EOF from the peer.

This function may also return ‘gnutls-e-again’, or
‘gnutls-e-interrupted’.

(fn PROC CONT)"
  (error ’unimplemented-error))
(defun* gnutls-ciphers () "Return alist of GnuTLS symmetric cipher descriptions as plists.
The alist key is the cipher name.

(fn)"
  (error ’unimplemented-error))
(defun* gnutls-deinit () "Deallocate GnuTLS resources associated with process PROC.
See also ‘gnutls-boot’.

(fn PROC)"
  (error ’unimplemented-error))
(defun* gnutls-digests () "Return alist of GnuTLS digest-algorithm method descriptions as plists.

Use the value of the alist (extract it with ‘alist-get’ for instance)
with ‘gnutls-hash-digest’.  The alist key is the digest-algorithm
method name.

(fn)"
  (error ’unimplemented-error))
(defun* gnutls-error-fatalp () "Return non-nil if ERROR is fatal.
ERROR is an integer or a symbol with an integer ‘gnutls-code’ property.
Usage: (gnutls-error-fatalp ERROR)

(fn ERR)"
  (error ’unimplemented-error))
(defun* gnutls-error-string () "Return a description of ERROR.
ERROR is an integer or a symbol with an integer ‘gnutls-code’ property.

(fn ERROR)"
  (error ’unimplemented-error))
(defun* gnutls-errorp () "Return t if ERROR indicates a GnuTLS problem.
ERROR is an integer or a symbol with an integer ‘gnutls-code’ property.

(fn ERROR)"
  (error ’unimplemented-error))
(defun* gnutls-format-certificate () "Format a X.509 certificate to a string.

Given a PEM-encoded X.509 certificate CERT, returns a human-readable
string representation.

(fn CERT)"
  (error ’unimplemented-error))
(defun* gnutls-get-initstage () "Return the GnuTLS init stage of process PROC.
See also ‘gnutls-boot’.

(fn PROC)"
  (error ’unimplemented-error))
(defun* gnutls-hash-digest () "Digest INPUT with DIGEST-METHOD into a unibyte string.

Return nil on error.

The INPUT can be specified as a buffer or string or in other
ways (see Info node ‘(elisp)Format of GnuTLS Cryptography Inputs’).

The alist of digest algorithms can be obtained with ‘gnutls-digests’.
The DIGEST-METHOD may be a string or symbol matching a key in that
alist, or a plist with the ‘:digest-algorithm-id’ numeric property, or
the number itself.

(fn DIGEST-METHOD INPUT)"
  (error ’unimplemented-error))
(defun* gnutls-hash-mac () "Hash INPUT with HASH-METHOD and KEY into a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node ‘(elisp)Format of GnuTLS Cryptography Inputs’).  The KEY
will be wiped after use if it’s a string.

The INPUT can also be specified as a buffer or string or in other
ways.

The alist of MAC algorithms can be obtained with ‘gnutls-macs’.  The
HASH-METHOD may be a string or symbol matching a key in that alist, or
a plist with the ‘:mac-algorithm-id’ numeric property, or the number
itself.

(fn HASH-METHOD KEY INPUT)"
  (error ’unimplemented-error))
(defun* gnutls-macs () "Return alist of GnuTLS mac-algorithm method descriptions as plists.

Use the value of the alist (extract it with ‘alist-get’ for instance)
with ‘gnutls-hash-mac’.  The alist key is the mac-algorithm method
name.

(fn)"
  (error ’unimplemented-error))
(defun* gnutls-peer-status () "Describe a GnuTLS PROC peer certificate and any warnings about it.

The return value is a property list with top-level keys :warnings and
:certificates.

The :warnings entry is a list of symbols you can get a description of
with ‘gnutls-peer-status-warning-describe’, and :certificates is the
certificate chain for the connection, with the host certificate
first, and intermediary certificates (if any) following it.

In addition, for backwards compatibility, the host certificate is also
returned as the :certificate entry.

(fn PROC)"
  (error ’unimplemented-error))
(defun* gnutls-peer-status-warning-describe () "Describe the warning of a GnuTLS peer status from ‘gnutls-peer-status’.

(fn STATUS-SYMBOL)"
  (error ’unimplemented-error))
(defun* gnutls-symmetric-decrypt () "Decrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node ‘(elisp)Format of GnuTLS Cryptography Inputs’).  The KEY
will be wiped after use if it’s a string.

The IV and INPUT and the optional AEAD_AUTH can also be specified as a
buffer or string or in other ways.

The alist of symmetric ciphers can be obtained with ‘gnutls-ciphers’.
The CIPHER may be a string or symbol matching a key in that alist, or
a plist with the ‘:cipher-id’ numeric property, or the number itself.

AEAD ciphers: these ciphers will have a ‘gnutls-ciphers’ entry with
:cipher-aead-capable set to t.  AEAD_AUTH can be supplied for
these AEAD ciphers, but it may still be omitted (nil) as well.

(fn CIPHER KEY IV INPUT &optional AEAD-AUTH)"
  (error ’unimplemented-error))
(defun* gnutls-symmetric-encrypt () "Encrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error.

The KEY can be specified as a buffer or string or in other ways (see
Info node ‘(elisp)Format of GnuTLS Cryptography Inputs’).  The KEY
will be wiped after use if it’s a string.

The IV and INPUT and the optional AEAD_AUTH can also be specified as a
buffer or string or in other ways.

The alist of symmetric ciphers can be obtained with ‘gnutls-ciphers’.
The CIPHER may be a string or symbol matching a key in that alist, or
a plist with the :cipher-id numeric property, or the number itself.

AEAD ciphers: these ciphers will have a ‘gnutls-ciphers’ entry with
:cipher-aead-capable set to t.  AEAD_AUTH can be supplied for
these AEAD ciphers, but it may still be omitted (nil) as well.

(fn CIPHER KEY IV INPUT &optional AEAD-AUTH)"
  (error ’unimplemented-error))
