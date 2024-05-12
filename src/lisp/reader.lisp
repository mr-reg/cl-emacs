#|
Copyright (C) 2024 by Gleb Borodulia
Author: Gleb Borodulia <mr.reg@mail.ru>

This file is part of cl-emacs.

cl-emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

cl-emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.
|#
(uiop:define-package :cl-emacs/reader
    (:use
     :common-lisp
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/reader-utils
     :cl-emacs/commons)
  (:shadow #:read #:read-from-string)
  (:import-from :common-lisp-user
                #:memq
                )
  (:local-nicknames (:el :cl-emacs/elisp))
  ;; (:use-reexport
  ;;  ;; :cl-emacs/elisp/alien-vars
  ;;  ;; :cl-emacs/elisp/alloc
  ;;  ;; :cl-emacs/elisp/data
  ;;  ;; :cl-emacs/elisp/editfns
  ;;  ;; :cl-emacs/elisp/fileio
  ;;  ;; :cl-emacs/elisp/fns
  ;;  ;; :cl-emacs/elisp/font
  ;;  ;; :cl-emacs/elisp/xfns
  ;;  )
  (:export #:read-from-string)
  )
(in-package :cl-emacs/reader)
(log-enable :cl-emacs/reader :debug1)
(def-suite cl-emacs/reader)
(in-suite cl-emacs/reader)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defclass reader-signal (error)
  ())

(define-condition reader-stopped-signal (reader-signal)
  ())

(defparameter *states*
  '(state/toplevel
    state/chardata
    state/line-comment
    state/string))
(loop for state in *states*
      for id from 0
      do (eval `(defparameter ,state ,id)))

(defstruct reader
  (state 0 :type fixnum)
  (stack nil :type list)
  (character-counter 0 :type fixnum)
  (result nil)
  )
(defun whitespace-p (char)
  #M"return t if character is whitespace or nil. 
     nil used for EOF"
  (declare (type character char))
  (or (null char) (memq char '(#\space #\tab #\newline))))

(defun start-collector (reader)
  (declare (type reader reader))
  (log-debug "start collector")
  (with-slots (stack) reader
    (push nil stack)))
(defun end-list (reader)
  (declare (type reader reader))
  (log-debug "end list")
  (with-slots (stack result) reader
    (let ((last-list (nreverse (pop stack))))
      (if stack
          (push last-list (car stack))
          (progn
            (setq result last-list)
            (error 'reader-stopped-signal))))))

(defun end-string (reader)
  (declare (type reader reader))
  (log-debug "end string")
  (with-slots (stack result) reader
    (let* ((last-charlist (nreverse (pop stack)))
           (parsed (char-list-to-string last-charlist)))
      (if stack
          (push parsed (car stack))
          (progn
            (setq result parsed)
            (error 'reader-stopped-signal))))))
(defun end-chardata (reader)
  (declare (type reader reader))
  (log-debug "end chardata")
  (with-slots (stack result) reader
    (let* ((last-charlist (nreverse (pop stack)))
           (last-chardata (str:upcase (char-list-to-string last-charlist)))
           (non-symbol (parse-elisp-number last-chardata))
           (parsed (or non-symbol (intern last-chardata :cl-emacs/elisp))))
      (if stack
          (push parsed (car stack))
          (progn
            (setq result parsed)
            (error 'reader-stopped-signal))))))

(defun state/toplevel-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (cond
    ((memq char '(#\())
     (start-collector reader))
    ((memq char '(#\)))
     (end-list reader))
    ((whitespace-p char)
     (progn))
    ((memq char '(#\;))
     (process-char reader char :new-state state/line-comment))
    ((memq char '(#\"))
     (start-collector reader)
     (change-state reader state/string)
     )
    (t
     (start-collector reader)
     (process-char reader char :new-state state/chardata))
    ))
(defun state/string-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((memq char '(#\"))
       (end-string reader)
       (change-state reader state/toplevel))
      (t
       (push char (car stack)))
      )))
(defun state/chardata-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((or (memq char '(#\( #\) #\"))
           (whitespace-p char))
       (end-chardata reader)
       (process-char reader char :new-state state/toplevel))
      (t
       (when (or (upper-case-p char) (eq char #\_))
         (push #\_ (car stack)))
       (push char (car stack))
       )
      )))
(defun state/line-comment-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((eq char #\newline)
       (process-char reader char :new-state state/toplevel))
      (t (progn)))))

(defvar *transitions* nil)
;; (defvar *start-handlers* nil)
;; (defvar *stop-handlers* nil)
(let ((n-states (length *states*)))
  (setf *transitions* (make-array n-states))
  ;; (setf *start-handlers* (make-array n-states))
  ;; (setf *stop-handlers* (make-array n-states))
  )
(dolist (state *states*)
  (log-debug "~s" (symbol-name state))
  (eval `(setf (aref *transitions* ,state)
               ,(let* ((transition-function-name (concatenate 'string (symbol-name state) "-TRANSITION"))
                       (transition-function-symbol (find-symbol transition-function-name)))
                  (if transition-function-symbol
                      (symbol-function transition-function-symbol)
                      (error "can't find transition function ~a" transition-function-name)))))
  ;; (eval `(setf (aref *stop-handlers* ,state)
  ;;              ,(let* ((stop-function-name (concatenate 'string (symbol-name state) "-STOP-HANDLER"))
  ;;                      (stop-function-symbol (find-symbol stop-function-name)))
  ;;                 (if stop-function-symbol
  ;;                     (symbol-function stop-function-symbol)
  ;;                     (error "can't find stop-handler function ~a" stop-function-name)))))
  ;; (eval `(setf (aref *start-handlers* ,state)
  ;;              ,(let* ((start-function-name (concatenate 'string (symbol-name state) "-START-HANDLER"))
  ;;                      (start-function-symbol (find-symbol start-function-name)))
  ;;                 (if start-function-symbol
  ;;                     (symbol-function start-function-symbol)
  ;;                     (error "can't find start-handler function ~a" start-function-name)))))
  )

(defun change-state (reader new-state)
  (with-slots (state stack) reader
    (log-debug1 "transition to state ~s" (nth new-state *states*))
    (log-debug1 "stack: ~s" stack)
    (setq state new-state)))
(defun process-char (reader char &key new-state)
  (declare (type reader reader)
           (type character char))
  (with-slots (state) reader
    (log-debug1 "process char ~s in state ~s" char state)
    (when new-state
      (change-state reader new-state))
    (let ((handler (aref *transitions* state)))
      (funcall handler reader char))))

(defun read-internal (stream)
  (let ((reader (make-reader :state state/toplevel)))
    (with-slots (state stack result character-counter) reader
      (handler-case
          (handler-case
              (loop
                do (let ((char (read-char stream)))
                     (incf character-counter)
                     (process-char reader char)))
            (end-of-file ()
              (log-debug1 "end of file")
              (process-char reader nil)))
        (reader-stopped-signal ()
          (log-debug1 "reader stopped")))
      (log-debug "stack: ~s" stack)
      (log-debug "result: ~s" result)
      (values result reader))))

(defun read (stream)
  #M"Read one Lisp expression as text from STREAM, return as Lisp object.
     If STREAM is nil, use the value of `standard-input' (which see).
     STREAM or the value of `standard-input' may be:
     a buffer (read from point and advance it)
     a marker (read from where it points and advance it)
     a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
     a string (takes text from string, starting at the beginning)
     t (read text line using minibuffer and use it, or read from
     standard input in batch mode)."
  (multiple-value-bind (result) (read-internal stream)
    result))
(defun read-from-string (string &optional (start 0) (end (length string)))
  "Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
FINAL-STRING-INDEX is an integer giving the position of the next
remaining character in STRING.  START and END optionally delimit
a substring of STRING from which to read;  they default to 0 and
\(length STRING) respectively.  Negative values are counted from
the end of STRING."
  (with-input-from-string (stream (str:substring start end string))
    (multiple-value-bind (result reader) (read-internal stream)
      (with-slots (character-counter) reader
        (cons result character-counter)))))


(test read-from-string
  (is (equalp (read-from-string "test") '(el::test . 4)))
  (is (equalp (read-from-string "\"test\" ") '("test" . 6)))
  (is (equalp (read-from-string "test two") '(el::test . 5)))
  (is (equalp (car (read-from-string "(a\"b\") ")) '(el::a "b")))
  (is (equalp (car (read-from-string "(test 2 3 \"A ( B\")")) '(el::test 2 3 "A ( B")))
  (is (equalp (car (read-from-string "(test (2 3) 4 ())")) '(el::test (2 3) 4 ())))
  (is (equalp (car (read-from-string #M";;; comment line
                                        test symbol")) 'el::test))
  (is (equalp (car (read-from-string #M"(defvar test-var nil \"some 
                                        multiline docstring `with symbols'.\")"))
              '(el::defvar el::test-var el::nil
                #M"some 
                                          multiline docstring `with symbols'.")))
  )


(defun real-file-test ()
  (with-open-file (stream "../emacs/lisp/master.el" :direction :input)
    (read stream)
    (read stream)))

(defun test-me ()
  (run! 'cl-emacs/reader))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/reader)
