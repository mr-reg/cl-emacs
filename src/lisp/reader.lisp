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
     :defstar
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
(log-enable :cl-emacs/reader :debug2)
(def-suite cl-emacs/reader)
(in-suite cl-emacs/reader)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defclass reader-signal (error)
  ((details :initarg :details
            :initform ""
            :type string)))

(define-condition invalid-reader-input-error (reader-signal)
  ())

(define-condition eof-reader-error (reader-signal)
  ())
(defmethod* print-object ((e reader-signal) stream)
  (with-slots (details) e
    (format stream "#<~a details:~s>"
            (class-name (class-of e)) details))
  )

(define-condition reader-stopped-signal (reader-signal)
  ())

(deftype rcharacter ()
  '(or null character))

(defparameter *states*
  '(state/toplevel
    state/symbol
    state/symbol-escaped
    state/character
    state/line-comment
    state/string
    state/pointer))

;; using numeric states to select transition function faster
(loop for state in *states*
      for id from 0
      do (eval `(defparameter ,state ,id)))

(defstruct reader
  (state 0 :type fixnum)
  (stack nil :type list)
  (character-counter 0 :type fixnum)
  (extra-buffer nil :type list)
  (mod-stack nil :type list)
  ;; key = pointer number, value = cons (nil . placeholder) or (t . real value) 
  (pointers (make-hash-table :test #'eq) :type hash-table))

(defun* push-extra-char ((reader reader) (char rcharacter))
  (with-slots (extra-buffer) reader
    (push char extra-buffer)))
(defun* (pop-stack-frame -> (values t t)) ((reader reader))
  (with-slots (stack mod-stack) reader
    (let ((stack-frame (pop stack))
          (mod-frame (pop mod-stack)))
      (log-debug2 "pop stack-frame:~s mod-frame:~s. stack:~s mod-stack:~s"
                  stack-frame mod-frame stack mod-stack)
      (values stack-frame mod-frame))))

(defun* whitespace-p ((char rcharacter))
  #M"return t if character is whitespace or nil. 
     nil used for EOF"
  (or (null char) (memq char '(#\space #\tab #\newline))))
;; (defun* digit-p (char)
;;   #M"return t if character is digit"
;;   (declare (type character char))
;;   (char>= #\0 char #\9))

(defun* start-collector ((reader reader))
  (log-debug1 "start collector")
  (with-slots (stack mod-stack) reader
    (push nil stack)
    (push nil mod-stack)))

(defun* init-stack ((reader reader))
  "init stack with single empty list only first time. That means that parsing actually started"
  (with-slots (stack mod-stack) reader
    (unless stack
      (log-debug2 "initialize stacks")
      (push nil stack)
      (push nil mod-stack))))

(defun* push-modifier ((reader reader) modifier &key replace-last)
  (with-slots (mod-stack) reader
    (when replace-last
      (pop (car mod-stack)))
    (push modifier (car mod-stack))
    (log-debug1 "add modifier:~s replace-last:~s, mod-stack:~s" modifier replace-last mod-stack)
    ))

(defun* end-collector ((reader reader) collector-result modifiers)
  (setq collector-result (apply-modifiers reader collector-result modifiers))
  (with-slots (stack mod-stack) reader
    (let ((actual-top-modifiers (pop mod-stack)))
      (log-debug2 "actual-top-modifiers:~s" actual-top-modifiers)
      (push (apply-modifiers reader collector-result actual-top-modifiers) (car stack)))
    (push nil mod-stack)
    (change-state reader state/toplevel)
    ;; when our stack contains only one frame with only one element, looks like read operation can be stopped
    (when (and (null (cdr stack)) (null (cdar stack)))
      (error 'reader-stopped-signal))))

(defun* end-list ((reader reader))
  (log-debug1 "end list")
  (multiple-value-bind (reversed-list mod-frame) (pop-stack-frame reader)
    (let (result-list)
      (loop for element in reversed-list
            for idx from 0
            do (cond
                 ((eq element 'el::.)
                  (cond
                    ((= idx 0)
                     ;; dot in 0 index is considered as normal symbol
                     (push element result-list))
                    ((= idx 1)
                     (let ((last-element (car result-list)))
                       (setq result-list (if (eq last-element 'el::nil)
                                             nil
                                             last-element) )))
                    (t
                     (error 'invalid-reader-input-error :details "dot in wrong position"))))
                 (t (push element result-list))))
      (end-collector reader result-list mod-frame))))

(defun* end-vector ((reader reader))
  (log-debug1 "end vector")
  (multiple-value-bind (reversed-list mod-frame) (pop-stack-frame reader)
    (loop for element in reversed-list
          for pos from 0
          do (when (and (eq element 'el::.) (> pos 0))
               (error 'invalid-reader-input-error :details "dot makes no sense in vector")))
    (let* ((result-list (nreverse reversed-list))
           (parsed (make-array (length result-list) :initial-contents result-list)))
      (end-collector reader parsed mod-frame))))

(defun* state/toplevel-transition ((reader reader) (char rcharacter))
  (with-slots (stack mod-stack) reader
    (let* ((current-modifiers (car mod-stack))
           (active-modifier (car current-modifiers)))
      (log-debug2 "toplevel active-modifier: ~s" active-modifier)
      (unless (whitespace-p char)
        (init-stack reader))
      (cond
        ((eq char #\()
         (start-collector reader))
        ((eq char #\))
         (end-list reader))
        ((eq char #\[)
         (start-collector reader))
        ((eq char #\])
         (end-vector reader))
        ((whitespace-p char)
         (when active-modifier
           (error 'eof-reader-error :details (format nil "~s without body" active-modifier)))
         ;; do nothing on whitespace
         )
        ((eq char #\;)
         (when active-modifier
           (error 'eof-reader-error :details (format nil "unexpected semicolon inside ~s" active-modifier)))
         (change-state reader state/line-comment)
         (push-extra-char reader char))
        ((eq char #\")
         (start-collector reader)
         (change-state reader state/string))
        ((eq char #\?)
         (start-collector reader)
         (change-state reader state/character))
        ((eq char #\')
         (if (eq active-modifier '\#)
             (push-modifier reader '\#\' :replace-last t)
             (push-modifier reader '\')))
        ((eq char #\`)
         (push-modifier reader '\`))
        ((eq char #\,)
         (push-modifier reader '\,))
        ((and (eq char #\@) (eq active-modifier '\,))
         (push-modifier reader '\,@ :replace-last t))
        ((and (eq char #\:) (eq active-modifier '\#))
         (pop (car mod-stack))
         (start-collector reader)
         (change-state reader state/symbol)
         (push-modifier reader 'symbol)
         (push-modifier reader '\#\:))
        ((eq char #\#)
         (if (eq active-modifier '\#)
             (progn
               (pop (car mod-stack))
               (push-extra-char reader char)
               (push-extra-char reader char)
               (start-collector reader)
               (change-state reader state/symbol))
             (push-modifier reader '\#))
         )
        ((and (eq active-modifier '\#) (digit-char-p char))
         (pop (car mod-stack))
         (start-collector reader)
         (change-state reader state/pointer)
         (push-extra-char reader char))
        (t
         (start-collector reader)
         (change-state reader state/symbol)
         (push-extra-char reader char)
         )
        ))))

;; string
(defun* end-string ((reader reader))
  (log-debug1 "end string")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let ((parsed (char-list-to-string (nreverse stack-frame))))
      (end-collector reader parsed mod-frame))))

(defun* state/string-transition ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (cond
      ((memq char '(#\"))
       (end-string reader)
       (change-state reader state/toplevel))
      (t
       (push char (car stack)))
      )))

;; character
(defun* end-character ((reader reader))
  (log-debug1 "end character")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (with-slots (extra-buffer) reader
      (let ((parsed (char-list-to-string (nreverse stack-frame)))
            code)
        (handler-case
            (setq code (read-emacs-character parsed))
          (extra-symbols-in-character-spec-error (e)
            (with-slots (cl-emacs/character-reader::position cl-emacs/character-reader::parsed-code) e
              (let ((remainder (str:substring cl-emacs/character-reader::position t parsed)))
                (log-debug2 "~s parsed:~s position:~s" e parsed cl-emacs/character-reader::position)
                (log-debug1 "character definition remainder ~s" remainder)
                (loop for idx from (1- (length remainder)) downto 0
                      do (push (aref remainder idx) extra-buffer))
                (setq code cl-emacs/character-reader::parsed-code)))))
        (end-collector reader code mod-frame)))))

(defun* state/character-transition ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (cond
      ((whitespace-p char)
       (push-extra-char reader char) ; put whitespace early, because it is stack 
       (end-character reader)
       (change-state reader state/toplevel))
      (t
       (push char (car stack))))))

(defun* make-el-symbol (string &key intern)
  (let ((symbol-name (with-output-to-string (stream)
                       (loop for char across string
                             do (when (or (upper-case-p char) (eq char #\_))
                                  (write-char #\_ stream))
                                (write-char (char-upcase char) stream)))))
    (if intern
        (intern symbol-name :cl-emacs/elisp )
        (make-symbol symbol-name))))

;; symbol
(defun* end-symbol ((reader reader))
  (log-debug1 "end symbol")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let* ((chardata (char-list-to-string (nreverse stack-frame)))
           (elisp-number (unless (memq 'symbol mod-frame)
                           (parse-elisp-number chardata)))
           parsed)
      (cond
        ((string= chardata "##") (setq parsed 'el::||))
        ((eq (car mod-frame) '\#\:)
         (pop mod-frame)
         (log-debug2 "removing #: modifier. mod-frame:" mod-frame)
         (setq parsed (make-el-symbol chardata :intern nil)))
        (elisp-number (setq parsed elisp-number))
        (t (setq parsed (make-el-symbol chardata :intern t))))
      (log-debug2 "chardata parsed: ~s" parsed)
      (end-collector reader parsed mod-frame))))

(defun* push-emacs-sym-char-to-stack ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (push char (car stack))))

(defun* state/symbol-transition ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (cond
      ((or (memq char '(#\( #\) #\[ #\] #\" #\' #\` #\,))
           (whitespace-p char))
       (end-symbol reader)
       (change-state reader state/toplevel)
       (push-extra-char reader char))
      ((and (eq char #\.) (null (car stack)))
       (push-emacs-sym-char-to-stack reader char)
       (end-symbol reader)
       (change-state reader state/toplevel))
      ((eq char #\\)
       (change-state reader state/symbol-escaped))
      (t
       (push-emacs-sym-char-to-stack reader char)
       ;; (change-state reader state/symbol-escaped)
       ;; (push-extra-char reader char)
       )
      )))

(defun* state/symbol-escaped-transition ((reader reader) (char rcharacter))
  (push-emacs-sym-char-to-stack reader char)
  (push-modifier reader 'symbol)
  (change-state reader state/symbol)
  )

;; comment
(defun* state/line-comment-transition ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (cond
      ((eq char #\newline)
       (change-state reader state/toplevel)
       (push-extra-char reader char)
       )
      (t (progn)))))

(defun* get-pointer-cons ((reader reader) (pointer-number fixnum))
  #M"return already defined pointer value if it exists, 
     or creates empty placeholder and return it"
  (with-slots (pointers) reader
    (multiple-value-bind (cons found) (gethash pointer-number pointers)
      (unless found
        (let ((placeholder (cons nil nil)))
          (setq cons (cons nil placeholder))
          (setf (gethash pointer-number pointers) cons)))
      cons)))

;; pointer
(defun* end-pointer ((reader reader) &key new-pointer)
  (log-debug1 "end pointer")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (with-slots (pointers) reader
      (let* ((chardata (char-list-to-string (nreverse stack-frame)))
             (pointer-number (parse-elisp-number chardata))
             )
        (unless pointer-number
          (error 'invalid-reader-input-error :details "can't parse pointer number"))
        (if new-pointer
            (progn (push-modifier reader pointer-number)
                   (push-modifier reader 'new-pointer)
                   (change-state reader state/toplevel))
            (end-collector reader (cdr (get-pointer-cons reader pointer-number)) mod-frame))))))

(defun* state/pointer-transition ((reader reader) (char rcharacter))
  (with-slots (stack) reader
    (cond
      ((digit-char-p char)
       (push-emacs-sym-char-to-stack reader char))
      ((eq char #\=)
       (end-pointer reader :new-pointer t)
       (change-state reader state/toplevel))
      ((eq char #\#)
       (end-pointer reader)
       (change-state reader state/toplevel))
      (t
       (error 'invalid-reader-input-error :details "non-digital character inside pointer definition")))))

;; main part

(defvar *transitions* nil)
;; (defvar *start-handlers* nil)
;; (defvar *stop-handlers* nil)
(let ((n-states (length *states*)))
  (setf *transitions* (make-array n-states))
  ;; (setf *start-handlers* (make-array n-states))
  ;; (setf *stop-handlers* (make-array n-states))
  )
(dolist (state *states*)
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

(defun* replace-placeholder-in-tree (tree old-value new-value &optional stack)
  (when (memq tree stack)
    ;; protection from circles
    (return-from replace-placeholder-in-tree tree))
  (push tree stack)
  (log-debug2 "replace-placeholder-in-tree old:~s new:~s type-of:~s tree:~s" old-value new-value (type-of tree) tree)
  (cond
    ((consp tree)
     (log-debug2 "cons iteration")
     (setf (car tree)
           (if (eq old-value (car tree))
               new-value
               (replace-placeholder-in-tree (car tree) old-value new-value stack)))
     (setf (cdr tree)
           (if (eq old-value (cdr tree))
               new-value
               (replace-placeholder-in-tree (cdr tree) old-value new-value stack)))
     tree)
    ((vectorp tree)
     (log-debug2 "vector iteration")
     (loop for idx from 0
           for x across tree
           do (setf (aref tree idx)
                    (if (eq old-value x)
                        new-value
                        (replace-placeholder-in-tree x old-value new-value stack))))
     tree)
    (t
     tree)))

(defun* apply-modifiers ((reader reader) something (modifiers list))
  (log-debug2 "applying modifiers ~s to:~s" modifiers something)
  (with-slots (pointers stack) reader
    (loop
      while modifiers
      do (let ((modifier (car modifiers)))
           (setq modifiers (cdr modifiers))
           (log-debug1 "process ~s modifier" modifier)
           (case modifier
             ((symbol \#\:) (progn))
             (\'
              (setq something (list 'el::quote something)))
             (\`
              (setq something (list 'el::\` something)))
             (\,
              (setq something (list 'el::\, something)))
             (\,@
              (setq something (list 'el::\,@ something)))
             (\#\'
              (setq something (list 'el::function something)))
             ('new-pointer
              (let* ((pointer-number (pop modifiers))
                     (actual-cons (get-pointer-cons reader pointer-number)))
                (log-debug2 "actual-cons:~s" actual-cons)
                (when (car actual-cons)
                  (error 'invalid-reader-input-error
                         :details (format nil "pointer #~a already defined" pointer-number)))
                (log-debug2 "setting pointer #~a to current object ~s" pointer-number something)
                (setf (gethash pointer-number pointers) (cons t something))
                (setq something (replace-placeholder-in-tree something (cdr actual-cons) something))
                (setq stack (replace-placeholder-in-tree stack (cdr actual-cons) something))
                ))
             (t (error 'invalid-reader-input-error
                       :details (format nil "unexpected modifier ~s" modifier)))
             ))))
  ;; all modifiers processed
  (log-debug2 "modification result ~s" something)
  something)

(defun* change-state ((reader reader) new-state)
  (with-slots (state stack mod-stack) reader
    (log-debug1 "transition to state ~s" (nth new-state *states*))
    (log-debug2 "mod-stack: ~s" mod-stack)
    (log-debug2 "stack: ~s" stack)
    (setq state new-state)))
(defun* process-char ((reader reader) (char rcharacter) &key new-state)
  (with-slots (state) reader
    (log-debug2 "process char ~s in state ~s" char state)
    (when new-state
      (change-state reader new-state))
    (let ((handler (aref *transitions* state)))
      (funcall handler reader char))))

(defun* process-extra-buffer ((reader reader))
  (with-slots (extra-buffer) reader
    (log-debug2 "checking extra-buffer: ~s" extra-buffer)
    (loop while extra-buffer
          do (let ((char (car extra-buffer)))
               (log-debug2 "processing extra character ~s" char)
               (setq extra-buffer (cdr extra-buffer))
               ;; extra buffer is modified during character processing
               (process-char reader char)))))

(defun* read-internal (stream)
  (let ((reader (make-reader :state state/toplevel)))
    (with-slots (state stack character-counter extra-buffer) reader
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
      (when (or (null stack) (cdr stack))
        (error 'eof-reader-error :details "Lisp structure is not complete"))
      (values (caar stack) reader))))

(defun* read (stream)
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
(defun* read-from-string (string &optional (start 0) (end (length string)))
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

(test read-symbols
  (is (equalp (quote el::||) 
              (car (read-from-string "##"))))
  (is (equalp (quote (el::test . 4))
              (read-from-string "test")))
  (is (equalp (quote (el::test . 5))
              (read-from-string "test two")))
  (is (equalp (quote (el::\:test))
              (car (read-from-string "(:test))"))))
  (is (equalp (quote el::|AB123-+=*/C|)
              (car (read-from-string "ab123-+=*/c"))))
  (is (equalp (quote el::|AB__~!@$%^&:<>{}?C|)
              (car (read-from-string "ab_~!@$%^&:<>{}?c"))))
  (is-false (equalp (car (read-from-string "FOO"))
                    (car (read-from-string "foo"))))
  (is (equalp (quote el::_AB_CD)
              (car (read-from-string "AbCd"))))
  (is (equalp (quote el::nil)
              (car (read-from-string "nil"))))
  (is (equalp (quote el::|ABC, [/]'`|)
              (car (read-from-string "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))))
  (is (equalp (quote el::\,)
              (car (read-from-string "\\,"))))
  (is (equalp (quote el::+1)
              (car (read-from-string "\\+1"))))
  (is (equalp "NON-INTERN-SYMBOL"
              (symbol-name (car (read-from-string "#:non-intern-symbol")))))
  (is-false (find-symbol "NON-INTERN-SYMBOL" :el))
  (is (equalp "+1"
              (symbol-name (car (read-from-string "#:+1")))))
  (is (equalp ""
              (symbol-name (car (read-from-string "#:,")))))
  )
(test read-integers
  (is (equalp 1 (car (read-from-string "+1")))))

(test read-quotes
  (is (equalp (quote (el::quote el::test-symbol))
              (car (read-from-string "'test-symbol"))))
  (is (equalp (quote (el::quote (el::quote el::test-sym)))
              (car (read-from-string "''test-sym"))))
  (is (equalp (quote (el::quote (el::test-symbol)))
              (car (read-from-string "'(test-symbol)"))))
  (signals eof-reader-error (read-from-string "(' 3)"))
  (signals eof-reader-error (read-from-string "';; test comment"))
  (is (equalp (quote (el::quote "abc ("))
              (car (read-from-string "'\"abc (\""))))
  (is (equalp (quote (el::quote 66))
              (car (read-from-string "'?B"))))

  (is (equalp (quote (el::quote
                      (el::quote
                       (1 (el::quote
                           ((el::quote
                             (el::quote
                              (el::quote
                               (2 (el::quote (el::quote (el::quote (el::quote 3)))))))) 4)) 5))))
              (car (read-from-string "''(1 '('''(2 ''''3) 4) 5)"))))
  (is (equalp (quote (el::quote (el::a (el::quote el::b))))
              (car (read-from-string "'(a'b)"))))
  (is (equalp (quote (el::quote ("a" (el::quote el::b))))
              (car (read-from-string "'(\"a\"'b)"))))
  (is (equalp (quote (el::quote (65 (el::quote el::b))))
              (car (read-from-string "'(?A'b)"))))
  (is (equalp (quote (el::\` el::test-symbol))
              (car (read-from-string "`test-symbol"))))
  (is (equalp (quote (el::\` (el::test-symbol)))
              (car (read-from-string "`(test-symbol)"))))
  (is (equalp (quote (el::\` (el::\` el::test-symbol)))
              (car (read-from-string "``test-symbol"))))
  (is (equalp (quote ((el::\` el::test-symbol) (el::\, 3)))
              (car (read-from-string "(`test-symbol ,3)"))))
  ;; real emacs test (equal (quote (\` (form ((\, a) (\, ((\` (b (\, c))))))))) (car (read-from-string "`(form (,a ,(`(b ,c))))")))
  (is (equalp (quote (el::\` (el::form ((el::\, el::a) (el::\, ((el::\` (el::b (el::\, el::c)))))))))
              (car (read-from-string "`(form (,a ,(`(b ,c))))"))))
  (signals eof-reader-error (read-from-string "'"))
  (signals eof-reader-error (read-from-string "`,"))

  (is (equalp (quote (el::quote el::\,))
              (car (read-from-string "'\\,"))))
  (is (equalp (quote (el::\` (el::a (el::\,@ el::b))))
              (car (read-from-string "`(a ,@b)"))))
  )

(test read-strings
  (is (equalp (quote ("test" . 6))
              (read-from-string "\"test\" ")))
  (is (equalp (quote (el::defvar el::test-var el::nil
                       #M"some 
                          multiline docstring `with symbols'."))
              (car (read-from-string #M"(defvar test-var nil \"some 
                                        multiline docstring `with symbols'.\")"))))
  )
(test read-lists
  (is (equalp (quote (el::a "b"))
              (car (read-from-string "(a\"b\") "))))
  (is (equalp (quote (el::test 2 3 "A ( B"))
              (car (read-from-string "(test 2 3 \"A ( B\")"))))
  (is (equalp (quote (el::test (2 3) 4 ()))
              (car (read-from-string "(test (2 3) 4 ())"))))
  (is (equalp (quote (el::.))
              (car (read-from-string "(.)"))))
  (is (equalp (quote (el::a el::.))
              (car (read-from-string "(a .)"))))
  ;; "(?A.?B)" , not supported yet, looks like some hardcoded case
  (is (equalp (quote (3 . 4))
              (car (read-from-string "(3 .  4)"))))
  (is (equalp (quote 4)
              (car (read-from-string "(. 4)"))))
  (signals invalid-reader-input-error (read-from-string "(a b . c d)"))
  (is (equalp (quote (65 . 66))
              (car (read-from-string "(?A.?B)"))))
  (is (equalp (quote (65 . 66))
              (car (read-from-string "(?A. ?B))"))))
  (is (equalp nil (car (read-from-string "()"))))
  (signals eof-reader-error (read-from-string "("))
  (is (equalp (quote (1 2 3))
              (car (read-from-string "(1 . (2 . (3 . nil)))"))))
  )
(test read-comments
  (is (equalp 'el::test
              (car (read-from-string #M";;; comment line
                                        test symbol"))))
  )
(test read-characters
  (is (equalp '(65 66)
              (car (read-from-string "(?A?B))"))))
  (is (equalp '(el::a 92 65 66 65 32 . 3)
              (car (read-from-string "(a ?\\\\ ?A?B ?A?\\s. 3)")))))
;;https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Lisp-Data-Types

(test reader-special-cases
  (signals eof-reader-error (read-from-string ""))
  )

(test read-vectors
  (is (equalp (quote #(1 el::a))
              (car (read-from-string "[1 a]"))))
  (is (equalp (quote #(el::a el::b el::c el::.))
              (car (read-from-string "[a b c .]"))))
  (signals invalid-reader-input-error (read-from-string "[1 . a]"))
  )
(test read-from-string
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

(test reader-functions
  (is (equalp (quote (el::function el::a))
              (car (read-from-string "#'a"))))
  )

(test read-circles
  (let ((sample (car (read-from-string "#1=(a #1#)"))))
    (is (eq sample (second sample))))
  (let ((sample (car (read-from-string "#1=[a #1#]"))))
    (is (eq sample (aref sample 1))))
  (let ((sample (car (read-from-string "#1=[#1# a #1#]"))))
    (is (eq sample (aref sample 0)))
    (is (eq sample (aref sample 2))))
  (let ((sample (car (read-from-string "#1=(a . #1#)"))))
    (is (eq sample (cdr sample))))
  (let ((sample (car (read-from-string "#1=(#2=[#1# #2#] . #1#)"))))
    (is (eq sample (cdr sample)))
    (is (eq sample (aref (car sample) 0)))
    (is (eq (car sample) (aref (car sample) 1))))
  (let ((sample (car (read-from-string "#1=(#2=[#1# #2#] . #2#)"))))
    (is (eq sample (aref (car sample) 0)))
    (is (eq (car sample) (cdr sample)))
    (is (eq (car sample) (aref (car sample) 1))))
  (let ((sample (car (read-from-string "#1=[#2=(#1# . #2#)]"))))
    (is (eq sample (car (aref sample 0))))
    (is (eq (aref sample 0) (cdr (aref sample 0)))))
  (let ((sample (car (read-from-string "#1=(#2=[#3=(#1# . #2#) #4=(#3# . #4#)])"))))
    (is (eq sample (car (aref (car sample) 0))))
    (is (eq (car sample) (cdr (aref (car sample) 0))))
    (is (eq (aref (car sample) 0) (car (aref (car sample) 1))))
    (is (eq (aref (car sample) 1) (cdr (aref (car sample) 1)))))
  ;; check in other collection types, like hashmap and string properties
  )

(defun* real-file-test ()
  (with-open-file (stream "../emacs/lisp/master.el" :direction :input)
    (read stream)
    (read stream)))

(defun test-me ()
  (run! 'cl-emacs/reader))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/reader)
