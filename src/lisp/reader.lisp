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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/reader
    (:use
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/reader-utils
     :cl-emacs/character-reader
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/alloc
     :cl-emacs/fns
     :cl-emacs/commons)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:textprop #:cl-emacs/textprop)
                    )
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
  (:export #:read-cl-string)
  )

(in-package :cl-emacs/reader)
(log-enable :cl-emacs/reader :debug2)
;; (log-enable :cl-emacs/reader :info)
(def-suite cl-emacs/reader)
(in-suite cl-emacs/reader)
(named-readtables:in-readtable pstrings:pstring-syntax)

;; main reader documentation is here
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Lisp-Data-Types


(define-condition reader-signal (error-with-description)
  ())

(define-condition invalid-reader-input-error (reader-signal)
  ())

(define-condition eof-reader-error (reader-signal)
  ())


(define-condition reader-stopped-signal (reader-signal)
  ())

(defparameter *states*
  '(state/toplevel
    state/symbol
    state/symbol-escaped
    state/character
    state/line-comment
    state/pstring
    state/pointer
    state/radix-bin
    state/radix-oct
    state/radix-hex
    ))

;; using numeric states to select transition function faster
(loop for state in *states*
      for id from 0
      do (cl:eval `(defparameter ,state ,id)))

(defstruct reader
  (state 0 :type fixnum)
  (stack nil :type list)
  (character-counter 0 :type fixnum)
  (extra-buffer nil :type list)
  (mod-stack nil :type list)
  ;; key = pointer number, value = cons (nil . placeholder) or (t . real value)
  (pointers (make-hash-table :test 'el::eq) :type hash-table))

(defun* push-extra-char ((reader reader) (char character))
  (with-slots (extra-buffer) reader
    (push char extra-buffer)))
(defun* (pop-stack-frame -> (values t t)) ((reader reader))
  (with-slots (stack mod-stack) reader
    (let ((stack-frame (pop stack))
          (mod-frame (pop mod-stack)))
      (log-debug2 "pop stack-frame:~s mod-frame:~s. stack:~s mod-stack:~s"
                  stack-frame mod-frame stack mod-stack)
      (values stack-frame mod-frame))))

(defun* char-whitespace-p ((char character))
  #M"return t if character is whitespace or #\null.
     #\nullnil used for EOF"
  (memq char '(#\null #\space #\tab #\newline)))

(defun* char-end-of-statement-p ((char character))
  #M"return t if character is whitespace or nil or any
     special symbol that signals about new statement. "
  (or (char-whitespace-p char) (memq char '(#\( #\) #\[ #\] #\" #\' #\` #\, #\#))))

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
           (parsed (make-array (cl:length result-list) :initial-contents result-list)))
      (end-collector reader parsed mod-frame))))

(defun* state/toplevel-transition ((reader reader) (char character))
  (with-slots (stack mod-stack) reader
    (let* ((current-modifiers (car mod-stack))
           (active-modifier (car current-modifiers)))
      (log-debug2 "toplevel active-modifier: ~s" active-modifier)
      (unless (char-whitespace-p char)
        (init-stack reader))
      (cond
        ((eq active-modifier '\#)
         (cond
           ((eq char #\') (push-modifier reader '\#\' :replace-last t))
           ((eq char #\:)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/symbol)
            (push-modifier reader 'symbol)
            (push-modifier reader '\#\:))
           ((eq char #\#)
            (pop (car mod-stack))
            (push-extra-char reader char)
            (push-extra-char reader char)
            (start-collector reader)
            (change-state reader state/symbol))
           ((digit-char-p char)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/pointer)
            (push-extra-char reader char))
           ((memq char '(#\b #\B))
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/radix-bin))
           ((memq char '(#\o #\O))
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/radix-oct))
           ((memq char '(#\x #\X))
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/radix-hex))
           ((eq char #\!)
            (pop (car mod-stack))
            (change-state reader state/line-comment))
           ((eq char #\_)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/symbol))
           ((eq char #\()
            (pop (car mod-stack))
            (push-modifier reader 'pstring-full)
            (start-collector reader))
           ((eq char #\s)
            (pop (car mod-stack))
            (push-modifier reader 'hash-table)
            )
           (t (error 'invalid-reader-input-error :details "unsupported character after #"))
           ))
        ((eq char #\()
         (start-collector reader))
        ((eq char #\))
         (end-list reader))
        ((eq char #\[)
         (start-collector reader))
        ((eq char #\])
         (end-vector reader))
        ((char-whitespace-p char)
         ;; (when active-modifier
         ;;   (error 'eof-reader-error :details (format nil "~s without body" active-modifier)))
         ;; do nothing on whitespace
         )
        ((eq char #\;)
         (when active-modifier
           (error 'eof-reader-error :details (cl:format nil "unexpected semicolon inside ~s" active-modifier)))
         (change-state reader state/line-comment)
         (push-extra-char reader char))
        ((eq char #\")
         (start-collector reader)
         (change-state reader state/pstring))
        ((eq char #\?)
         (start-collector reader)
         (change-state reader state/character))
        ((eq char #\')
         (push-modifier reader '\'))
        ((eq char #\`)
         (push-modifier reader '\`))
        ((eq char #\,)
         (push-modifier reader '\,))
        ((and (eq char #\@) (eq active-modifier '\,))
         (push-modifier reader '\,@ :replace-last t))
        ((eq char #\#) (push-modifier reader '\#))
        (t
         (start-collector reader)
         (change-state reader state/symbol)
         (push-extra-char reader char)
         )))))

;; string
(defun* end-pstring ((reader reader))
  (log-debug1 "end string")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let ((parsed (char-list-to-pstring (nreverse stack-frame))))
      (end-collector reader parsed mod-frame))))

(defun* state/pstring-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((memq char '(#\"))
       (end-pstring reader)
       (change-state reader state/toplevel))
      (t
       (push char (car stack)))
      )))
;; character
(defun* end-character ((reader reader))
  (log-debug1 "end character")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (with-slots (extra-buffer) reader
      (let ((parsed (char-list-to-cl-string (nreverse stack-frame)))
            code)
        (handler-case
            (setq code (read-emacs-character parsed))
          (extra-symbols-in-character-spec-error (e)
            (with-slots (cl-emacs/character-reader::position cl-emacs/character-reader::parsed-code) e
              (let ((remainder (str:substring cl-emacs/character-reader::position t parsed)))
                (log-debug2 "~s parsed:~s position:~s" e parsed cl-emacs/character-reader::position)
                (log-debug1 "character definition remainder ~s" remainder)
                (loop for idx from (1- (cl:length remainder)) downto 0
                      do (push (aref remainder idx) extra-buffer))
                (setq code cl-emacs/character-reader::parsed-code)))))
        (end-collector reader code mod-frame)))))

(defun* state/character-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((char-whitespace-p char)
       (push-extra-char reader char) ; put whitespace early, because it is stack, not queue
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
        ;; TODO: use emacs functions here
        (cl:intern symbol-name :cl-emacs/elisp )
        (cl:make-symbol symbol-name))))

;; symbol
(defun* end-symbol ((reader reader))
  (log-debug1 "end symbol")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let* ((chardata (char-list-to-cl-string (nreverse stack-frame)))
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

(defun* push-to-reader-stack ((reader reader) char)
  (with-slots (stack) reader
    (push char (car stack))))

(defun* state/symbol-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((char-end-of-statement-p char)
       (end-symbol reader)
       (change-state reader state/toplevel)
       (push-extra-char reader char))

      ;; if we have dot and nothing has been read yet, maybe it is end of symbol
      ;; known special case is .?a
      ((and (eq char #\?) (equal (car stack) '(#\.)))
       (push-extra-char reader char)
       (end-symbol reader)
       (change-state reader state/toplevel))
      ((eq char #\\)
       (change-state reader state/symbol-escaped))
      (t
       (push-to-reader-stack reader char)))))

(defun* state/symbol-escaped-transition ((reader reader) (char character))
  (push-to-reader-stack reader char)
  (push-modifier reader 'symbol)
  (change-state reader state/symbol)
  )

;; comment
(defun* state/line-comment-transition ((reader reader) (char character))
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
      (let* ((chardata (char-list-to-cl-string (nreverse stack-frame)))
             (pointer-number (parse-elisp-number chardata))
             )
        (unless pointer-number
          (error 'invalid-reader-input-error :details "can't parse pointer number"))
        (if new-pointer
            (progn (push-modifier reader pointer-number)
                   (push-modifier reader 'new-pointer)
                   (change-state reader state/toplevel))
            (end-collector reader (cdr (get-pointer-cons reader pointer-number)) mod-frame))))))

(defun* state/pointer-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((digit-char-p char)
       (push-to-reader-stack reader char))
      ((eq char #\=)
       (end-pointer reader :new-pointer t)
       (change-state reader state/toplevel))
      ((eq char #\#)
       (end-pointer reader)
       (change-state reader state/toplevel))
      (t
       (error 'invalid-reader-input-error :details "non-digital character inside pointer definition")))))

                ;;; radix
(defun* end-radix ((reader reader) (radix-bits fixnum))
  (log-debug1 "end radix")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (unless stack-frame
      (error 'invalid-reader-input-error :details "empty number definition"))
    (let ((parsed (reversed-list-to-number stack-frame radix-bits)))
      (end-collector reader parsed mod-frame))))
(defun* generic-radix-transition ((reader reader) (char character) (radix-bits fixnum))
  (let* ((radix (ash 1 radix-bits))
         (parsed (and (characterp char) (digit-char-p char radix))))
    (with-slots (stack) reader
      (cond
        (parsed
         (push-to-reader-stack reader parsed))
        ((char-end-of-statement-p char)
         (end-radix reader radix-bits)
         (change-state reader state/toplevel))
        (t
         (error 'invalid-reader-input-error :details (cl:format nil "bad symbol ~s for radix ~a" char radix)))))))
(defun* state/radix-bin-transition ((reader reader) (char character))
  (generic-radix-transition reader char 1))
(defun* state/radix-oct-transition ((reader reader) (char character))
  (generic-radix-transition reader char 3))
(defun* state/radix-hex-transition ((reader reader) (char character))
  (generic-radix-transition reader char 4))

;; main part

(defvar *transitions* nil)
;; (defvar *start-handlers* nil)
;; (defvar *stop-handlers* nil)
(let ((n-states (cl:length *states*)))
  (setf *transitions* (make-array n-states))
  ;; (setf *start-handlers* (make-array n-states))
  ;; (setf *stop-handlers* (make-array n-states))
  )
(dolist (state *states*)
  (cl:eval `(setf (aref *transitions* ,state)
                  ,(let* ((transition-function-name (concatenate 'string (cl:symbol-name state) "-TRANSITION"))
                          (transition-function-symbol (find-symbol transition-function-name)))
                     (if transition-function-symbol
                         (cl:symbol-function transition-function-symbol)
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
  (when (eq old-value new-value)
    (error 'invalid-reader-input-error
           :details "self-referencing pointer found"))
  (push tree stack)
  (log-debug2 "replace-placeholder-in-tree old:~s new:~s type-of:~s tree:~s" old-value new-value (cl:type-of tree) tree)
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

(defun* transform-to-new-pointer (something
                                  &key
                                  pointer-number
                                  actual-cons
                                  pointers-hash
                                  )
  (log-debug2 "actual-cons:~s" actual-cons)
  (when (car actual-cons)
    (error 'invalid-reader-input-error
           :details (cl:format nil "pointer #~a already defined" pointer-number)))
  (log-debug2 "setting pointer #~a to current object ~s" pointer-number something)
  (setf (gethash pointer-number pointers-hash) (cons t something))
  (replace-placeholder-in-tree something (cdr actual-cons) something))

(defun* transform-to-pstring (something)
  (log-debug2 "starting pstring-full transformation ~s" something)
  (let ((pstr (pop something)))
    (unless (pstrings:pstring-p pstr)
      (error 'invalid-reader-input-error
             :details "first element of property-string definition should be a string"))
    (loop while something
          for start = (pop something)
          for end = (pop something)
          for plist = (pop something)
          do (unless (or (fixnump start) (fixnump end))
               (error 'invalid-reader-input-error
                      :details "index in property-string should be integer"))
             (textprop:set-text-properties start end plist pstr))
    pstr)
  )

(defun* transform-to-hash-table (something)
  (log-debug2 "starting hash-table parsing ~s" something)
  (unless (eq (pop something) 'el::hash-table)
    (error 'invalid-reader-input-error :details
           "hash-table definition should start from the word \"hash-table\""))
  (let (args data ht)
    (ignore-errors ;; like emacs does
     (doplist (key val something)
       (case key
         (el::size (push val args)
          (push :size args))
         (el::test (push val args)
           (push :test args))
         (el::rehash-size (push val args)
          (push :rehash-size args))
         (el::rehash-threshold (push val args)
          (push :rehash-threshold args))
         (el::data (setq data val))
         (t ;; just silently ignore bad parameter, like emacs does
          ))))
    (log-debug2 "args: ~s" args)
    (setq ht (cl:apply 'make-hash-table args))
    (doplist (key val data)
      (puthash key val ht))
    ht)
  )

(defun* apply-modifiers ((reader reader) something (modifiers list) &optional (nil-not-allowed t))
  (log-debug2 "applying modifiers ~s to:~s" modifiers something)
  (when (and modifiers nil-not-allowed (null something))
    (error 'invalid-reader-input-error
           :details (cl:format nil "can't apply modifiers ~s without actual body" modifiers)))
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
             (new-pointer
              (let* ((pointer-number (pop modifiers))
                     (actual-cons (get-pointer-cons reader pointer-number)))
                (setq something
                      (transform-to-new-pointer
                       something
                       :pointer-number pointer-number
                       :actual-cons actual-cons
                       :pointers-hash pointers))
                (setq stack (replace-placeholder-in-tree stack (cdr actual-cons) something))
                ))
             (pstring-full
              (setq something (transform-to-pstring something)))
             (hash-table
              (setq something (transform-to-hash-table something)))
             (t (error 'invalid-reader-input-error
                       :details (cl:format nil "unexpected modifier ~s" modifier)))
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
(defun* process-char ((reader reader) (char character) &key new-state)
  (with-slots (state) reader
    (log-debug2 "process char ~s in state ~s" char state)
    (when new-state
      (change-state reader new-state))
    (let ((handler (aref *transitions* state)))
      (cl:funcall handler reader char))))

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
    (with-slots (state stack mod-stack character-counter extra-buffer pointers) reader
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
              (process-char reader #\null)
              ;; and extra buffer finally
              (process-extra-buffer reader)))
        (reader-stopped-signal ()
          (log-debug1 "reader stopped")))
      (log-debug2 "stack: ~s" stack)
      (when (or (null stack) (cdr stack))
        (error 'eof-reader-error :details "Lisp structure is not complete"))
      (when (car mod-stack)
        (error 'eof-reader-error :details (cl:format nil "modifiers ~s without defined body" (car mod-stack))))
      (loop for pointer-num being each hash-key in pointers using (hash-value cons)
            do (unless (car cons)
                 (error 'invalid-reader-input-error
                        :details (cl:format nil "undefined pointer #~a" pointer-num))))
      (values (caar stack) reader))))

(defun* read-cl-string (cl-string &optional (start 0) (end (cl:length cl-string)))
  #M"Read one Lisp expression which is represented as text by STRING.
                     Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
                     FINAL-STRING-INDEX is an integer giving the position of the next
                     remaining character in STRING.  START and END optionally delimit
                     a substring of STRING from which to read;  they default to 0 and
                     \(length STRING) respectively.  Negative values are counted from
                     the end of STRING."
  (with-input-from-string (stream (str:substring start end cl-string))
    (multiple-value-bind (result reader) (read-internal stream)
      (with-slots (character-counter) reader
        (cons result character-counter)))))

(test read-symbols
  (is (equal (quote el::||)
             (car (read-cl-string "##"))))
  (is (equal (quote (el::test . 4))
             (read-cl-string "test")))
  (is (equal (quote (el::test . 5))
             (read-cl-string "test two")))
  (is (equal (quote (el::\:test))
             (car (read-cl-string "(:test))"))))
  (is (equal (quote el::|AB123-+=*/C|)
             (car (read-cl-string "ab123-+=*/c"))))
  (is (equal (quote el::|AB__~!@$%^&:<>{}?C|)
             (car (read-cl-string "ab_~!@$%^&:<>{}?c"))))
  (is-false (equal (car (read-cl-string "FOO"))
                   (car (read-cl-string "foo"))))
  (is (equal (quote el::_AB_CD)
             (car (read-cl-string "AbCd"))))
  (is (equal (quote el::nil)
             (car (read-cl-string "nil"))))
  (is (equal (quote el::|ABC, [/]'`|)
             (car (read-cl-string "a\\b\\c\\,\\ \\[\\/\\]\\'\\`"))))
  (is (equal (quote el::\,)
             (car (read-cl-string "\\,"))))
  (is (equal (quote el::+1)
             (car (read-cl-string "\\+1"))))
  (is (equal #P"non-intern-symbol"
             (symbol-name (car (read-cl-string "#:non-intern-symbol")))))
  (is-false (find-symbol "non-intern-symbol" :el))
  (is (equal #P"+1"
             (symbol-name (car (read-cl-string "#:+1")))))
  (is (equal #P""
             (symbol-name (car (read-cl-string "#:,")))))
  (is (equal 'el::test
             (car (read-cl-string "#_test"))))
  (is (equal (quote el::||)
             (car (read-cl-string "#_"))))  )

(test read-shorthands
  ;; TODO: add support for shorthands in reader
  ;; (let ((read-symbol-shorthands
  ;;         '(("s-" . "shorthand-longhand-"))))
  ;; (read "#_s-test")
  ;;   (read "#_s-test"))
  )

(test read-numbers
  (is (= 1 (car (read-cl-string "1"))))
  (is (= 1 (car (read-cl-string "1."))))
  (is (= 1 (car (read-cl-string "+1"))))
  (is (= -1 (car (read-cl-string "-1"))))
  (is (= 0 (car (read-cl-string "+0"))))
  (is (= 0 (car (read-cl-string "-0"))))
  (is (= 0 (car (read-cl-string "-0.0"))))

  (is (= 1500 (car (read-cl-string "1500.0"))))
  (is (= 1500 (car (read-cl-string "+15E2"))))
  (is (= 1500 (car (read-cl-string "+1500000e-3"))))
  (is (= 1500 (car (read-cl-string "15.0e+2"))))
  (is (= 12.34 (car (read-cl-string "1.234e+01"))))
  (is (= 1500 (car (read-cl-string ".15e4"))))
  (is (equal cl-emacs/data::*nan* (car (read-cl-string "0.0e+NaN"))))
  (is (equal cl-emacs/data::*nan* (car (read-cl-string "-4.5E+NaN"))))
  (is (equal cl-emacs/data::*infinity* (car (read-cl-string "1.0e+INF"))))
  (is (equal cl-emacs/data::*infinity* (car (read-cl-string "-4.0e+INF"))))
  )
(test read-quotes
  (is (equal (quote (el::quote el::test-symbol))
             (car (read-cl-string "'test-symbol"))))
  (is (equal (quote (el::quote (el::quote el::test-sym)))
             (car (read-cl-string "''test-sym"))))
  (is (equal (quote (el::quote (el::test-symbol)))
             (car (read-cl-string "'(test-symbol)"))))
  (is (equal (quote ((el::quote 3)))
             (car (read-cl-string "(' 3)"))))
  (signals eof-reader-error (read-cl-string "';; test comment"))
  (is (equal `(el::quote ,#P"abc (")
             (car (read-cl-string "'\"abc (\""))))
  (is (equal (quote (el::quote 66))
             (car (read-cl-string "'?B"))))

  (is (equal (quote (el::quote
                     (el::quote
                      (1 (el::quote
                          ((el::quote
                            (el::quote
                             (el::quote
                              (2 (el::quote (el::quote (el::quote (el::quote 3)))))))) 4)) 5))))
             (car (read-cl-string "''(1 '('''(2 ''''3) 4) 5)"))))
  (is (equal (quote (el::quote (el::a (el::quote el::b))))
             (car (read-cl-string "'(a'b)"))))
  (is (equal `(el::quote (,#P"a" (el::quote el::b)))
             (car (read-cl-string "'(\"a\"'b)"))))
  (is (equal (quote (el::quote (65 (el::quote el::b))))
             (car (read-cl-string "'(?A'b)"))))
  (is (equal (quote (el::\` el::test-symbol))
             (car (read-cl-string "`test-symbol"))))
  (is (equal (quote (el::\` (el::test-symbol)))
             (car (read-cl-string "`(test-symbol)"))))
  (is (equal (quote (el::\` (el::\` el::test-symbol)))
             (car (read-cl-string "``test-symbol"))))
  (is (equal (quote ((el::\` el::test-symbol) (el::\, 3)))
             (car (read-cl-string "(`test-symbol ,3)"))))
  ;; real emacs test (equal (quote (\` (form ((\, a) (\, ((\` (b (\, c))))))))) (car (read-cl-string "`(form (,a ,(`(b ,c))))")))
  (is (equal (quote (el::\` (el::form ((el::\, el::a) (el::\, ((el::\` (el::b (el::\, el::c)))))))))
             (car (read-cl-string "`(form (,a ,(`(b ,c))))"))))
  (signals eof-reader-error (read-cl-string "'"))
  (signals eof-reader-error (read-cl-string "`,"))

  (is (equal (quote (el::quote el::\,))
             (car (read-cl-string "'\\,"))))
  (is (equal (quote (el::\` (el::a (el::\,@ el::b))))
             (car (read-cl-string "`(a ,@b)"))))
  )

(test read-strings
  (is (equal `(,#P"test" . 6)
             (read-cl-string "\"test\" ")))
  (is (equal `(el::defvar el::test-var el::nil
                ,(pstrings:build-pstring #M"some
                                            multiline docstring `with symbols'."))
             (car (read-cl-string #M"(defvar test-var nil \"some
                                     multiline docstring `with symbols'.\")"))))
  ;; TODO: add better pstring property validation after more complicated reads
  ;; TODO: research corner cases
  (is (equal
       (quote #P" ")
       (car (read-cl-string "#(\" \" 0 1 (invisible t))"))))
  (is (equal
       (quote #P"foo bar")
       (car (read-cl-string "#(\"foo bar\" 0 3 (face bold) 3 4 nil 4 7 (face italic))"))))

  )
(test read-lists
  (is (equal `(el::a ,#P"b")
             (car (read-cl-string "(a\"b\") "))))
  (is (equal `(el::test 2 3 ,#P"A ( B")
             (car (read-cl-string "(test 2 3 \"A ( B\")"))))
  (is (equal (quote (el::test (2 3) 4 ()))
             (car (read-cl-string "(test (2 3) 4 ())"))))
  (is (equal (quote (el::.))
             (car (read-cl-string "(.)"))))
  (is (equal (quote (el::a el::.))
             (car (read-cl-string "(a .)"))))
  (is (equal (quote (el::a el::.b))
             (car (read-cl-string "(a .b)"))))
  (is (equal (quote (3 . 4))
             (car (read-cl-string "(3 .  4)"))))
  (is (equal (quote 4)
             (car (read-cl-string "(. 4)"))))
  (signals invalid-reader-input-error (read-cl-string "(a b . c d)"))
  (is (equal (quote (65 . 66))
             (car (read-cl-string "(?A.?B)"))))
  (is (equal (quote (65 . 66))
             (car (read-cl-string "(?A. ?B))"))))
  (is (equal nil (car (read-cl-string "()"))))
  (signals eof-reader-error (read-cl-string "("))
  (is (equal (quote (1 2 3))
             (car (read-cl-string "(1 . (2 . (3 . nil)))"))))
  )
(test read-comments
  (is (equal 'el::test
              (car (read-cl-string #M";;; comment line
                                        test symbol"))))
  )
(test read-characters
  (is (equal '(65 66)
             (car (read-cl-string "(?A?B))"))))
  (is (equal '(el::a 92 65 66 65 32 . 3)
             (car (read-cl-string "(a ?\\\\ ?A?B ?A?\\s. 3)")))))

(test reader-special-cases
  (signals eof-reader-error (read-cl-string ""))
  (is (equal '(el::quote el::test)
             (car (read-cl-string #M" ' #!some-stuff
                                        #!some-stuff
                                        test")))))

(test read-vectors
  (is (equal (quote #(1 el::a))
             (car (read-cl-string "[1 a]"))))
  (is (equal (quote #(el::a el::b el::c el::.))
             (car (read-cl-string "[a b c .]"))))
  (signals invalid-reader-input-error (read-cl-string "[1 . a]"))
  )
(test read-cl-string
;;;###autoload
  ;; #'test
  ;; #'(lambda (x) (use-package-require-after-load x body))
  ;; #'*
  ;; #'.
  ;; #'gnus-article-jump-to-part
  ;; #+A invalid syntax
  ;; '#'test - function
  ;; #'test - symbol
  ;; #[1 2] - bytecode?
  ;; #\" eof?
  ;; lisp_file_lexically_bound_p supports some lexical env while loading using lexical-binding variable
  ;; #&N bool vector?
  ;; #@number skip. #@00 - skip to eof/eob
  ;; strings:
  ;;  \s \  \\n ?\C-SPC ?\^SPC \C-SPC' and `\^SPC
  ;; #s( record
  ;; #^[ char table
  ;; #^^[ sub-char table
  ;; #[ byte code
  ;; #$ ???
  ;; #NrDIGITS -- radix-N number, any radix 0-36, r or R
  ;; obarrays?
  )

(test reader-functions
  (is (equal (quote (el::function el::a))
             (car (read-cl-string "#'a"))))
  )

(test read-circles
  (let ((sample (car (read-cl-string "#1=(a #1#)"))))
    (is (eq sample (second sample))))
  (let ((sample (car (read-cl-string "#1=[a #1#]"))))
    (is (eq sample (aref sample 1))))
  (let ((sample (car (read-cl-string "#1=[#1# a #1#]"))))
    (is (eq sample (aref sample 0)))
    (is (eq sample (aref sample 2))))
  (let ((sample (car (read-cl-string "#1=(a . #1#)"))))
    (is (eq sample (cdr sample))))
  (let ((sample (car (read-cl-string "#1=(#2=[#1# #2#] . #1#)"))))
    (is (eq sample (cdr sample)))
    (is (eq sample (aref (car sample) 0)))
    (is (eq (car sample) (aref (car sample) 1))))
  (let ((sample (car (read-cl-string "#1=(#2=[#1# #2#] . #2#)"))))
    (is (eq sample (aref (car sample) 0)))
    (is (eq (car sample) (cdr sample)))
    (is (eq (car sample) (aref (car sample) 1))))
  (let ((sample (car (read-cl-string "#1=[#2=(#1# . #2#)]"))))
    (is (eq sample (car (aref sample 0))))
    (is (eq (aref sample 0) (cdr (aref sample 0)))))
  (let ((sample (car (read-cl-string "#1=(#2=[#3=(#1# . #2#) #4=(#3# . #4#)])"))))
    (is (eq sample (car (aref (car sample) 0))))
    (is (eq (car sample) (cdr (aref (car sample) 0))))
    (is (eq (aref (car sample) 0) (car (aref (car sample) 1))))
    (is (eq (aref (car sample) 1) (cdr (aref (car sample) 1)))))
  (is (equal (quote ((el::quote (el::quote el::a)) (el::quote el::a)))
             (car (read-cl-string "('#1='a #1#)"))))
  (signals invalid-reader-input-error (read-cl-string "#1=(#1# #2#)"))
  (signals invalid-reader-input-error (read-cl-string "#1=#1#"))
  ;; check in other collection types, like hashmap and string properties
  )

(test read-radix
  (signals invalid-reader-input-error (read-cl-string "#b"))
  (is (equal (quote #b10)
             (car (read-cl-string "#b010"))))
  (is (equal (quote #b1010)
             (car (read-cl-string "#B01010"))))
  (signals invalid-reader-input-error (read-cl-string "#b013"))

  (signals invalid-reader-input-error (read-cl-string "#o"))
  (is (equal (quote #o10)
             (car (read-cl-string "#o010"))))
  (is (equal (quote #o1010)
             (car (read-cl-string "#O01010"))))
  (signals invalid-reader-input-error (read-cl-string "#o013a"))

  (signals invalid-reader-input-error (read-cl-string "#x"))
  (is (equal (quote #x010aaf)
             (car (read-cl-string "#x010aAF"))))
  (is (equal (quote #x010aaf)
             (car (read-cl-string "#X010aAF"))))
  (signals invalid-reader-input-error (read-cl-string "#x013aw"))
  )

(test read-hash-tables
  (let ((ht (car (read-cl-string
                  #M"#s(hash-table
                     size 1000 test eq
                     rehash-size 1.3
                     rehash-threshold 0.6
                     data (a 2 b 3))"))))
    (is (= 1000 (hash-table-size ht)))
    (is (eq 'el::eq (hash-table-test ht)))
    (is (= 1.3 (hash-table-rehash-size ht)))
    (is (= 0.6 (hash-table-rehash-threshold ht)))
    (is (= 2 (gethash 'el::a ht)))
    (is (= 3 (gethash 'el::b ht))))
  (let ((ht (car (read-cl-string
                  "#s(hash-table bad-parameter some-value test equal data)"))))
    (is (eq 'el::equal (hash-table-test ht)))
    (is (= 0 (hash-table-count ht)))))

;; (defun* real-file-test ()
;;   (with-open-file (stream "../emacs/lisp/master.el" :direction :input)
;;     (read stream)
;;     (read stream)))


(test read-char-tables ()
  ;; (let ((ct (read "#^[2 nil some-symbol 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 slot0 1 slot2 1 1 1]")))
  ;;   (message "%s" (symbol-plist 'some-symbol))
  ;;   (char-table-extra-slot ct 0)
  ;;   (char-table-extra-slot ct 6))
  )

(defun test-me ()
  (run! 'cl-emacs/reader))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/reader)
