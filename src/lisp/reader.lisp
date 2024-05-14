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
     :cl-emacs/character-reader
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

(define-condition invalid-reader-input-error (reader-signal)
  ())

(define-condition eof-reader-error (reader-signal)
  ())

(define-condition reader-stopped-signal (reader-signal)
  ())

(defparameter *states*
  '(state/toplevel
    state/chardata
    state/character
    state/line-comment
    state/string))

;; using numeric states to select transition function faster
(loop for state in *states*
      for id from 0
      do (eval `(defparameter ,state ,id)))

(defstruct reader
  (state 0 :type fixnum)
  (stack nil :type list)
  (character-counter 0 :type fixnum)
  (extra-buffer nil :type list)
  (result '-undefined-))
(defun push-extra-char (reader char)
  (with-slots (extra-buffer) reader
    (push char extra-buffer)))


(defun whitespace-p (char)
  #M"return t if character is whitespace or nil. 
     nil used for EOF"
  (declare (type character char))
  (or (null char) (memq char '(#\space #\tab #\newline))))

(defun start-collector (reader)
  (declare (type reader reader))
  (log-debug1 "start collector")
  (with-slots (stack) reader
    (push nil stack)))

(defun end-list (reader)
  (declare (type reader reader))
  (log-debug1 "end list")
  (with-slots (stack result) reader
    (let ((reversed-list (pop stack))
          result-list)
      (loop for element in reversed-list
            for idx from 0
            do (cond
                 ((and (= idx 1) (eq element 'el::.))
                  (setq result-list (car result-list)))
                 (t (push element result-list))))
      (if stack
          (push result-list (car stack))
          (progn
            (setq result result-list)
            (error 'reader-stopped-signal))))))

(defun state/toplevel-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (cond
    ((eq char #\()
     (start-collector reader))
    ((eq char #\))
     (end-list reader))
    ((whitespace-p char)
     ;; do nothing on whitespace
     (progn))
    ((eq char #\;)
     (change-state reader state/line-comment)
     (push-extra-char reader char))
    ((eq char #\")
     (start-collector reader)
     (change-state reader state/string))
    ((eq char #\?)
     (start-collector reader)
     (change-state reader state/character))
    (t
     (start-collector reader)
     (change-state reader state/chardata)
     (push-extra-char reader char)
     )
    ))

;; string
(defun end-string (reader)
  (declare (type reader reader))
  (log-debug1 "end string")
  (with-slots (stack result) reader
    (let* ((last-charlist (nreverse (pop stack)))
           (parsed (char-list-to-string last-charlist)))
      (if stack
          (push parsed (car stack))
          (progn
            (setq result parsed)
            (error 'reader-stopped-signal))))))

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

;; character
(defun end-character (reader)
  (declare (type reader reader))
  (log-debug1 "end character")
  (with-slots (stack result extra-buffer) reader
    (let* ((last-charlist (nreverse (pop stack)))
           (parsed (char-list-to-string last-charlist))
           code)
      (handler-case
          (setq code (read-emacs-character parsed))
        (extra-symbols-in-character-spec-error (e)
          (with-slots (cl-emacs/character-reader::position cl-emacs/character-reader::parsed-code) e
            (let ((remainder (str:substring cl-emacs/character-reader::position t parsed)))
              (log-debug1 "~s parsed:~s position:~s" e parsed cl-emacs/character-reader::position)
              (log-debug1 "character definition remainder ~s" remainder)
              (loop for idx from (1- (length remainder)) downto 0
                    do (push (aref remainder idx) extra-buffer))
              (setq code cl-emacs/character-reader::parsed-code)))))
      (if stack
          (push code (car stack))
          (progn
            (setq result code)
            (error 'reader-stopped-signal))))))

(defun state/character-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((whitespace-p char)
       (push-extra-char reader char) ; put whitespace early, because it is stack 
       (end-character reader)
       (change-state reader state/toplevel))
      (t
       (push char (car stack))))))

;; chardata
(defun end-chardata (reader)
  (declare (type reader reader))
  (log-debug1 "end chardata")
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

(defun state/chardata-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((or (memq char '(#\( #\) #\"))
           (whitespace-p char))
       (end-chardata reader)
       (change-state reader state/toplevel)
       (push-extra-char reader char)
       )
      (t
       (when (or (upper-case-p char) (eq char #\_))
         (push #\_ (car stack)))
       (push char (car stack))
       )
      )))
;; comment
(defun state/line-comment-transition (reader char)
  (declare (type reader reader)
           (type character char))
  (with-slots (stack) reader
    (cond
      ((eq char #\newline)
       (change-state reader state/toplevel)
       (push-extra-char reader char)
       )
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
    (log-debug2 "stack: ~s" stack)
    (setq state new-state)))
(defun process-char (reader char &key new-state)
  (declare (type reader reader)
           (type character char))
  (with-slots (state) reader
    (log-debug2 "process char ~s in state ~s" char state)
    (when new-state
      (change-state reader new-state))
    (let ((handler (aref *transitions* state)))
      (funcall handler reader char))))

(declaim (inline process-extra-buffer))
(defun process-extra-buffer (reader)
  (with-slots (extra-buffer) reader
    (log-debug2 "checking extra-buffer: ~s" extra-buffer)
    (loop while extra-buffer
          do (let ((char (car extra-buffer)))
               (log-debug2 "processing extra character ~s" char)
               (setq extra-buffer (cdr extra-buffer))
               ;; extra buffer is modified during character processing
               (process-char reader char)))))

(defun read-internal (stream)
  (let ((reader (make-reader :state state/toplevel)))
    (with-slots (state stack result character-counter extra-buffer) reader
      (handler-case
          (handler-case
              (loop
                do (let ((char (read-char stream)))
                     (incf character-counter)
                     (process-char reader char)
                     (process-extra-buffer reader)))
            (end-of-file ()
              (log-debug1 "end of file")
              (process-extra-buffer reader)
              ;; final whitespace to finish any structure
              (process-char reader nil)
              ;; and extra buffer finally
              (process-extra-buffer reader)))
        (reader-stopped-signal ()
          (log-debug1 "reader stopped")))
      (log-debug2 "stack: ~s" stack)
      (log-debug1 "result: ~s" result)
      (when (or stack (eq result '-undefined-))
        (error 'eof-reader-error))
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
  #M"Read one Lisp expression which is represented as text by STRING.
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
  (is (equalp '(el::test . 4)
              (read-from-string "test")))
  (is (equalp '("test" . 6)
              (read-from-string "\"test\" ")))
  (is (equalp '(el::test . 5)
              (read-from-string "test two")))
  (is (equalp '(el::a "b")
              (car (read-from-string "(a\"b\") "))))
  (is (equalp '(el::test 2 3 "A ( B")
              (car (read-from-string "(test 2 3 \"A ( B\")"))))
  (is (equalp '(el::test (2 3) 4 ())
              (car (read-from-string "(test (2 3) 4 ())"))))
  (is (equalp 'el::test
              (car (read-from-string #M";;; comment line
                                        test symbol"))))
  (is (equalp '(el::defvar el::test-var el::nil
                #M"some 
                   multiline docstring `with symbols'.")
              (car (read-from-string #M"(defvar test-var nil \"some 
                                        multiline docstring `with symbols'.\")"))))
  (is (equalp '(el::.)
              (car (read-from-string "(.)"))))
  ;; "(?A.?B)" , not supported yet, looks like some hardcoded case
  (is (equalp '(3 . 4)
              (car (read-from-string "(3 .  4)"))))
  (is (equalp '4
              (car (read-from-string "(. 4)"))))
  (is (equalp '(65 66)
              (car (read-from-string "(?A?B))"))))
  (is (equalp '(65 . 66)
              (car (read-from-string "(?A. ?B))"))))
  (is (equalp '(el::a 92 65 66 65 32 . 3)
              (car (read-from-string "(a ?\\\\ ?A?B ?A?\\s. 3)"))))
  (is (equalp nil (car (read-from-string "()"))))
  (signals eof-reader-error (read-from-string ""))
  (signals eof-reader-error (read-from-string "("))
  (is (equalp '(el::\:test)
              (car (read-from-string "(:test))"))))
  ;; (is (equalp '(65 66)
  ;;             (car (read-from-string "'(test))"))))
  ;; ` , ,@
  ;;;###autoload
  ;; #'test
  ;; #'(lambda (x) (use-package-require-after-load x body))
  ;; #'*
  ;; #'.
  ;; #'gnus-article-jump-to-part
  ;; #(" " 0 1 (invisible t))
  ;; #+A invalid syntax
  ;; '#'test - function
  ;; #'test - symbol
  ;; #[1 2] - bytecode?
  ;; #\" eof?
  ;; #o600
  ;; #x000100

  )


(defun real-file-test ()
  (with-open-file (stream "../emacs/lisp/master.el" :direction :input)
    (read stream)
    (read stream)))

(defun test-me ()
  (run! 'cl-emacs/reader))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/reader)
