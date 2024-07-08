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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/lib/reader
    (:use
     :defstar
     :cl-emacs/lib/log
     :fiveam
     :cl-emacs/lib/reader-utils
     :cl-emacs/lib/character-reader
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/alloc
     :cl-emacs/fns
     :cl-emacs/lib/commons)
  (:import-from #:alexandria
                #:doplist)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:chartables #:cl-emacs/types/chartables)
                    (#:textprop #:cl-emacs/textprop)
                    (#:creader #:cl-emacs/lib/character-reader)
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
  (:export #:read-cl-string
           #:eof-reader-error)
  )

(in-package :cl-emacs/lib/reader)
;; (log-enable :cl-emacs/lib/reader :debug2)
(log-enable :cl-emacs/lib/reader :info)
(def-suite cl-emacs/lib/reader)
(in-suite cl-emacs/lib/reader)
(named-readtables:in-readtable mstrings:mstring-syntax)

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

(defmacro define-states (&rest states)
  ;; using numeric states to select transition function faster
  (let (body1)
    (push
     (cl:append '(defun define-transitions ())
                (loop for state in states
                      for id from 0
                      for sym-name = (concatenate 'string (cl:symbol-name state) "-TRANSITION")
                      collect
                      `(let ((sym (find-symbol ,sym-name)))
                         (unless sym
                           (error "can't find function ~a" ,sym-name))
                         (setf (aref *transitions* ,id) (cl:symbol-function sym))
                         )))
     body1)
    (setq body1  (cl:append (loop for state in states
                                  for id from 0
                                  collect `(defparameter ,state ,id))
                            body1))
    (push `(defparameter *transitions* (make-array ,(cl:length states))) body1)

    (cons 'progn body1)))
(define-states
    state/toplevel
  state/symbol
  state/symbol-escaped
  state/character
  state/line-comment
  state/pstring
  state/pointer
  state/radix-bin
  state/radix-oct
  state/radix-hex
  state/bool-vector-len)


(defstruct reader
  (state 0 :type fixnum)
  (stack nil :type list)
  (character-counter 0 :type fixnum)
  (extra-buffer nil :type list)
  (mod-stack nil :type list)
  ;; key = pointer number, value = cons (nil . placeholder) or (t . real value)
  (pointers (make-hash-table :test 'el::eq)))

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



(defun* start-collector ((reader reader))
  (log-debug1 "start collector")
  (with-slots (stack mod-stack) reader
    (push nil stack)
    (push nil mod-stack)))

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
      (log-debug2 "result list:~s" result-list)
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
      (cond
        ((eq active-modifier '\#)
         (cond
           ((eq char #\')
            (push-modifier reader '\#\' :replace-last t))
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
            (push-modifier reader 'hash-table))
           ((eq char #\^)
            (pop (car mod-stack))
            (push-modifier reader 'chartable))
           ((eq char #\&)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/bool-vector-len))
           (t (error 'invalid-reader-input-error :details "unsupported character after #"))
           ))
        ((and (eq active-modifier 'chartable)
              (eq char #\^))
         (pop (car mod-stack))
         (push-modifier reader 'sub-chartable))
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
         (push (pstrings:build-pstring "") (car stack))
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
        ((eq char #\#)
         (push-modifier reader '\#))
        (t
         (start-collector reader)
         (change-state reader state/symbol)
         (push-extra-char reader char)
         )))))

;; string
(defun* end-pstring ((reader reader))
  ;; first element in stack-frame is always pstring - collector
  ;; this function should try to process stack frame and return t
  ;; if string was really finished
  ;; if string is really finished, stack should contain only last double quote
  (log-debug1 "end pstring started")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let* ((rev-frame (nreverse stack-frame))
           (collector (car rev-frame))
           (char-list (cdr rev-frame))
           (cl-buffer (char-list-to-cl-string char-list))
           (buffer-len (cl:length cl-buffer))
           (start-position 0)
           (string-is-over nil)
           (current-chunk "")
           (multibyte nil))
      (unless (pstrings:pstring-p collector)
        (error 'invalid-reader-input-error :details
               "pstring reader not initialized properly (bug)"))
      (log-debug2 "collector:~s" collector)
      (loop for char across cl-buffer
            do (when (>= (char-code char) 128)
                 (setf multibyte t)
                 (return)))
      (setq
       current-chunk
       (with-output-to-string (stream)
         (setq
          cl-buffer
          (loop
            while (< start-position buffer-len)
            for char = nil
            do (handler-case
                   (progn
                     (log-debug2 "read character start-position:~s cl-buffer:~s"
                                 start-position cl-buffer)
                     (setq char (read-string-character cl-buffer
                                                       :start-position start-position))
                     ;; complete read means that current string data is over
                     ;; but string reader can continue
                     (log-debug2 "normal complete read")
                     (when char
                       (write-char (code-char char) stream))
                     (return ""))
                 (extra-symbols-in-character-spec-error (e)
                   (with-slots (creader::position
                                creader::parsed-code) e
                     (log-debug2 "position:~s parsed-code:~s"
                                 creader::position
                                 creader::parsed-code)
                     (setq char creader::parsed-code)
                     (cond
                       ;; reader stopped and double quote ahead - proper end of the string
                       ((and (zerop creader::position)
                             (cl:char-equal #\" (aref cl-buffer start-position)))
                        ;; return everything ahead of double quote to main reader
                        (setq string-is-over t)
                        (return (str:substring (1+ start-position) t cl-buffer)))
                       ;; reader stopped but not a double quote ahead
                       ((zerop creader::position)
                        (error 'invalid-reader-input-error :details
                               "string parser is stuck (bug)"))
                       ;; move reader forward
                       (t
                        ;; character can be nil if it should be ignored according to
                        ;; the emacs escape syntax
                        (when char
                          (write-char (code-char char) stream))
                        (incf start-position creader::position)
                        (log-debug2 "moving position +~s=~s" creader::position
                                    start-position)
                        ))
                     )
                   ))
            finally (return "")))))

      (log-debug2 "current-chunk:~s cl-buffer:~s" current-chunk cl-buffer)
      ;; accumulate chunk to collector
      (pstrings:ninsert (pstrings:build-pstring current-chunk) collector)
      (when (and multibyte (not (pstrings:pstring-multibyte collector)))
        (setf (pstrings:pstring-multibyte collector) t))

      (when string-is-over
        ;; here cl-buffer may contain some unprocessed characters for main reader
        (log-debug2 "ending string reader, extra-characters:~s" cl-buffer)
        (loop for char across cl-buffer
              do (push-extra-char reader char))
        (end-collector reader collector mod-frame)
        (return-from end-pstring t))
      ;; here we know that cl-buffer is completely processed and string is still
      ;; not complete. Restore stack and continue read
      (log-debug2 "restart string-reader")
      (with-slots (stack mod-stack) reader
        (push (list collector) stack)
        (push mod-frame mod-stack))
      ;; nil result means that string is not finished
      nil)

    ))

(defun* state/pstring-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (push char (car stack))
    (when (eq char #\")
      (when (end-pstring reader)
        (change-state reader state/toplevel)))))
;; character
(defun* end-character ((reader reader))
  (log-debug1 "end character")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (with-slots (extra-buffer) reader
      (let ((parsed (char-list-to-cl-string (nreverse stack-frame)))
            code)
        (log-debug2 "parsed: ~s" parsed)
        (handler-case
            (setq code (read-emacs-character parsed))
          (extra-symbols-in-character-spec-error (e)
            (with-slots (creader::position creader::parsed-code) e
              (let ((remainder (str:substring creader::position t parsed)))
                (log-debug2 "~s parsed:~s position:~s" e parsed creader::position)
                (log-debug1 "character definition remainder ~s" remainder)
                (loop for idx from (1- (cl:length remainder)) downto 0
                      do (push (aref remainder idx) extra-buffer))
                (setq code creader::parsed-code)))))
        (end-collector reader code mod-frame)))))

(defun* state/character-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((char-whitespace-p char)
       (push char (car stack))
       ;; (push-extra-char reader char) ; put whitespace early, because it is stack, not queue
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
         (push-extra-char reader char)
         (change-state reader state/toplevel))
        (t
         (error 'invalid-reader-input-error :details (cl:format nil "bad symbol ~s for radix ~a" char radix)))))))
(defun* state/radix-bin-transition ((reader reader) (char character))
  (generic-radix-transition reader char 1))
(defun* state/radix-oct-transition ((reader reader) (char character))
  (generic-radix-transition reader char 3))
(defun* state/radix-hex-transition ((reader reader) (char character))
  (generic-radix-transition reader char 4))

(defun* end-bool-vector-len ((reader reader) )
  (log-debug1 "end bool-vector-len")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let* ((chardata (char-list-to-cl-string (nreverse stack-frame)))
           (elisp-number (parse-elisp-number chardata)))
      (unless elisp-number
        (error 'invalid-reader-input-error :details
               (cl:format nil "can't parse bool-vector length: ~s" chardata)))
      (log-debug2 "bool-vector length: ~s" elisp-number)
      ;; push parsed vector length and bool-vector modifier to mod-frame
      (push elisp-number mod-frame)
      (push 'bool-vector mod-frame)
      ;; "cleanup" collector and continue to read char data in normal string state
      (with-slots (stack mod-stack) reader
        (push nil stack)
        (push mod-frame mod-stack))))
  )
(defun* state/bool-vector-len-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((digit-char-p char)
       (push-to-reader-stack reader char))
      ((eq char #\")
       (end-bool-vector-len reader)
       ;; pstring collector initialization
       (push (pstrings:build-pstring "") (car stack))
       (change-state reader state/pstring))
      (t
       (error 'invalid-reader-input-error
              :details "non-digital character inside bool-vector length definition")))))

;; main part


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

(defun* transform-to-chartable ((something cl:vector))
  (let* ((normal-size (aref chartables:+chartab-size+ 0))
         (num-extra-slots (- (cl:length something) 4 normal-size))
         contents extra-slots)
    (unless (<= 0 num-extra-slots 10)
      (error 'invalid-reader-input-error :details
             "chartable vector length is incorrect"))
    (setq contents (make-array normal-size))
    (loop for idx from 0 below normal-size
          do (setf (aref contents idx) (aref something (+ 4 idx))))
    (setq extra-slots (make-array num-extra-slots))
    (loop for idx from 0 below num-extra-slots
          do (setf (aref extra-slots idx) (aref something (+ 4 normal-size idx))))
    (chartables:make-chartable
     :default (aref something 0)
     :parent (aref something 1)
     :purpose (aref something 2)
     :ascii (aref something 3)
     :contents contents
     :extra-slots extra-slots)))

(defun* transform-to-sub-chartable ((something cl:vector))
  (let (depth normal-size)
    (setq depth (when (> (cl:length something) 0) (aref something 0)))
    (unless (<= 1 depth 3)
      (error 'invalid-reader-input-error
             :details "sub-chartable depth can be only 1, 2 or 3"))
    (setq normal-size (aref chartables:+chartab-size+ depth))
    (unless (= (cl:length something) (+ 2 normal-size))
      (error 'invalid-reader-input-error :details
             (cl:format nil "sub-chartable vector length should be exactly 2 + ~s" normal-size)))

    (loop with contents = (make-array normal-size)
          for idx from 0 below normal-size
          do (setf (aref contents idx) (aref something (+ 2 idx)))
          finally (return (chartables:make-sub-chartable :depth (aref something 0)
                                                         :min-char (aref something 1)
                                                         :contents contents)))
    ))

(defun* transform-to-bool-vector ((len fixnum) (pstr pstrings:pstring))
  (let ((result (make-array len :element-type 'cl:bit :initial-element 0))
        (idx 0))
    (pstrings:map-chars #'(lambda (char)
                            (let ((code (char-code char)))
                              (log-debug2 "bool-vector idx:~s code:~s" idx code)
                              (unless (<= 0 code 255)
                                (error 'invalid-reader-input-error :details
                                       "can't use character with code >= 256 in bool-vector definition "))
                              (loop with mask = 1
                                    while (and (< mask 256) (< idx len))
                                    do (setf (aref result idx)
                                             (if (zerop (logand code mask)) 0 1))
                                       (log-debug2 "mask:~s bit:~s" mask (logand code mask))
                                       (incf idx)
                                       (setq mask (ash mask 1)))))
                        pstr)
    result))

(defun* apply-modifiers ((reader reader) something (modifiers list) &optional (nil-allowed t))
  (log-debug2 "applying modifiers ~s to:~s" modifiers something)
  (when (and modifiers (not nil-allowed) (null something))
    (error 'invalid-reader-input-error
           :details (cl:format nil "can't apply modifiers ~s without actual body" modifiers)))
  (with-slots (pointers stack) reader
    (loop
      while modifiers
      do (let ((modifier (pop modifiers)))
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
             (chartable
              (setq something (transform-to-chartable something)))
             (sub-chartable
              (setq something (transform-to-sub-chartable something)))
             (bool-vector
              (setq something (transform-to-bool-vector (pop modifiers) something)))
             (t (error 'invalid-reader-input-error
                       :details (cl:format nil "unexpected modifier ~s" modifier)))
             ))))
  ;; all modifiers processed
  (log-debug2 "modification result ~s" something)
  something)

(defun* change-state ((reader reader) new-state)
  (with-slots (state stack mod-stack) reader
    (log-debug1 "transition to state ~s" new-state)
    (log-debug2 "mod-stack: ~s" mod-stack)
    (log-debug2 "stack: ~s" stack)
    (setq state new-state)
    ))
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

(defun* read-internal (stream &optional (external-position 0))
  #M"external-position is used only for better debug. It will show you exact character
     position is source string, where reader found a problem"
  (let ((reader (make-reader :state state/toplevel)))
    (with-slots (state stack mod-stack character-counter extra-buffer pointers) reader
      (push nil stack)
      (push nil mod-stack)
      (handler-case
          (handler-case
              (loop for read-position from external-position
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
      (when (or (null stack) (equal '(nil) stack))
        (error 'eof-reader-error :details "Reader not started - empty input"))
      (when  (cdr stack)
        (error 'eof-reader-error :details "Lisp structure is not complete"))
      (when (car mod-stack)
        (error 'eof-reader-error :details (cl:format nil "modifiers ~s without defined body" (car mod-stack))))
      (maphash #'(lambda (pointer-num cons)
                   (unless (car cons)
                     (error 'invalid-reader-input-error
                            :details (cl:format nil "undefined pointer #~a" pointer-num)))
                   ) pointers)
      (values (caar stack) reader))))

(defun* read-cl-string (cl-string &optional (start 0) (end (cl:length cl-string)))
  #M"Read one Lisp expression which is represented as text by STRING.
                     Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
                     FINAL-STRING-INDEX is an integer giving the position of the next
                     remaining character in STRING.  START and END optionally delimit
                     a substring of STRING from which to read;  they default to 0 and
                     \(length STRING) respectively.  Negative values are counted from
                     the end of STRING."
  (with-input-from-string (stream cl-string :start start :end end)
    (multiple-value-bind (result reader) (read-internal stream start)
      (with-slots (character-counter) reader
        (cons result character-counter)))))

;; last function, should be called after all transitions definition
(define-transitions)

(test test-read-symbols
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
  (is (equal (pstrings:build-pstring "non-intern-symbol")
             (symbol-name (car (read-cl-string "#:non-intern-symbol")))))
  (is-false (find-symbol "non-intern-symbol" :el))
  (is (equal (pstrings:build-pstring "+1")
             (symbol-name (car (read-cl-string "#:+1")))))
  (is (equal (pstrings:build-pstring "")
             (symbol-name (car (read-cl-string "#:,")))))
  (is (equal 'el::test
             (car (read-cl-string "#_test"))))
  (is (equal (quote el::||)
             (car (read-cl-string "#_"))))
  (is (equal (quote el::|-|)
             (car (read-cl-string "-"))))
  )

(test test-read-shorthands
  ;; TODO: add support for shorthands in reader
  ;; (let ((read-symbol-shorthands
  ;;         '(("s-" . "shorthand-longhand-"))))
  ;; (read "#_s-test")
  ;;   (read "#_s-test"))
  )

(test test-read-numbers
  (is (= 1 (car (read-cl-string "1"))))
  (is (= 1 (car (read-cl-string "1."))))
  (is (= 1 (car (read-cl-string "+1"))))
  (is (= -1 (car (read-cl-string "-1"))))
  (is (= 0 (car (read-cl-string "+0"))))
  (is (= 0 (car (read-cl-string "-0"))))
  (is (= 0 (car (read-cl-string "-0.0"))))

  (is (= 1500 (car (read-cl-string "1500.0"))))
  (is (= 1500 (car (read-cl-string "+15E2"))))
  (is (= 1600 (car (read-cl-string "+160000e-2"))))
  (is (= 1500 (car (read-cl-string "15.0e+2"))))
  (is (= 12.34 (car (read-cl-string "1.234e+01"))))
  (is (= 1500 (car (read-cl-string ".15e4"))))
  (is (equal cl-emacs/data::*nan* (car (read-cl-string "0.0e+NaN"))))
  (is (equal cl-emacs/data::*nan* (car (read-cl-string "-4.5E+NaN"))))
  (is (equal cl-emacs/data::*positive-infinity* (car (read-cl-string "1.0e+INF"))))
  (is (equal cl-emacs/data::*negative-infinity* (car (read-cl-string "-4.0e+INF"))))
  )
(test test-read-quotes
  (is (equal (quote (el::quote el::test-symbol))
             (car (read-cl-string "'test-symbol"))))
  (is (equal (quote (el::quote (el::quote el::test-sym)))
             (car (read-cl-string "''test-sym"))))
  (is (equal (quote (el::quote (el::test-symbol)))
             (car (read-cl-string "'(test-symbol)"))))
  (is (equal (quote ((el::quote 3)))
             (car (read-cl-string "(' 3)"))))
  (signals eof-reader-error (read-cl-string "';; test comment"))
  (is (equal `(el::quote ,(pstrings:build-pstring "abc ("))
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
  (is (equal `(el::quote (,(pstrings:build-pstring "a") (el::quote el::b)))
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
  (is (equal (quote (el::|,| (el::|,| el::a)))
             (car (read-cl-string ",,a"))))
  (is (equal (quote (el::|`| el::|,| el::a))
             (car (read-cl-string "(\\` . ,a)")))))

(test test-read-strings
  (is (equal `(,(pstrings:build-pstring "test") . 6)
             (read-cl-string "\"test\" ")))
  (is (equal `(el::defvar el::test-var el::nil
                ,(pstrings:build-pstring #M"some
                                            multiline docstring `with symbols'."))
             (car (read-cl-string #M"(defvar test-var nil \"some
                                     multiline docstring `with symbols'.\")"))))
  ;; TODO: add better pstring property validation after more complicated reads
  (is (equal
       (pstrings:build-pstring " ")
       (car (read-cl-string "#(\" \" 0 1 (invisible t))"))))
  (is (equal
       (pstrings:build-pstring "foo bar")
       (car (read-cl-string "#(\"foo bar\" 0 3 (face bold) 3 4 nil 4 7 (face italic))"))))
  (is (equal (pstrings:build-pstring "abc\\z\"")
             (car (read-cl-string "\"abc\\\\\\x7a\\\"\"" ))))
  (is (equal (pstrings:build-pstring "sample string without newline.")
             (car (read-cl-string
                   #M"#(\"sampl\\ e \\
                      string without newline.\" 0 4 (face bold))"))))
  (is (equal (pstrings:build-pstring (cl:format nil "~c, \"\"~c" #\tab #\soh))
             (car (read-cl-string "\"\\t, \\\"\\\"\\C-a\""))))
  (is (equal  (pstrings:build-pstring "")
              (car (read-cl-string "#(\"\" 0 0 (invisible t))"))))
  (is (equal
       (pstrings:build-pstring "fooA0bar")
       (car (read-cl-string "\"foo\\u00410bar\")"))))
  ;; (read-cl-string "\"\\x103000\")")
  (is-false (pstrings:pstring-multibyte (car (read-cl-string "\"\\M-k\""))))
  (is (pstrings:pstring-multibyte (car (read-cl-string "\"\\±\""))))
  )
(test test-read-lists
  (is (equal `(el::a ,(pstrings:build-pstring "b"))
             (car (read-cl-string "(a\"b\") "))))
  (is (equal `(el::test 2 3 ,(pstrings:build-pstring "A ( B"))
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
  (is (equal (quote (el::quote nil))
             (car (read-cl-string "'()"))))
  )
(test test-read-comments
  (is (equal 'el::test
             (car (read-cl-string #M";;; comment line
                                        test symbol"))))
  (signals eof-reader-error (read-cl-string ";; just comment"))
  )
(test test-read-characters
  (is (equal '(65 66)
             (car (read-cl-string "(?A?B))"))))
  (is (equal '(el::a 92 65 66 65 32 . 3)
             (car (read-cl-string "(a ?\\\\ ?A?B ?A?\\s. 3)"))))
  (is (equal 32
             (car (read-cl-string "?\\ "))))
  (is (equal #x202a
             (car (read-cl-string "?\\x202a"))))
  (is (equal '(#x202a #x202a)
             (car (read-cl-string "(?\\x202a ?\\x202a)"))))
  (is (equal #o202
             (car (read-cl-string "?\\202"))))
  )

(test test-reader-special-cases
  (signals eof-reader-error (read-cl-string ""))
  (is (equal '(el::quote el::test)
             (car (read-cl-string #M" ' #!some-stuff
                                        #!some-stuff
                                        test")))))

(test test-read-vectors
  (is (equal (quote #(1 el::a))
             (car (read-cl-string "[1 a]"))))
  (is (equal (quote #(el::a el::b el::c el::.))
             (car (read-cl-string "[a b c .]"))))
  (signals invalid-reader-input-error (read-cl-string "[1 . a]"))
  )
(test test-read-cl-string
;;;###autoload
  ;; #&8"\0"
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

(test test-reader-functions
  (is (equal (quote (el::function el::a))
             (car (read-cl-string "#'a"))))
  )

(test test-read-circles
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

(test test-read-radix
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
  (is (equal (quote (el::test #x123))
             (car (read-cl-string "(test #x123)"))))
  )

(test test-read-hash-tables
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


(test test-read-chartables ()
  (let ((sub-ct (car (read-cl-string "#^^[3 12 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]"))))
    (is (= 3 (chartables:sub-chartable-depth sub-ct)))
    (is (= 12 (chartables:sub-chartable-min-char sub-ct)))
    (is (= 8 (aref (chartables:sub-chartable-contents sub-ct) 0)))
    (is (= 3 (aref (chartables:sub-chartable-contents sub-ct) 3)))
    )
  (signals invalid-reader-input-error (read-cl-string "#^^[3 12 8 8 8 3 8]"))
  (let ((ct (car (read-cl-string
                  #M"#^[8 nil test
                     #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     #^^[1 0 #^^[2 0 #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     #^^[1 65536 3 3 #^^[2 73728 3 #^^[3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
                     slot0 slot1 slot2]")
                 )))
    (is (equal
         '((0 79 8) (80 73859 3) (73860 4194303 8))
         (chartables:get-chartable-ranges ct)))
    (is (equal '#(el::slot0 el::slot1 el::slot2)
               (chartables:chartable-extra-slots ct)))
    ))

(test test-read-bool-vector ()
  (is (equal #*1010
             (car (read-cl-string "#&4\"\""))))
  (is (equal #*10
             (car (read-cl-string "#&2\"a\""))))
  (is (equal #*
             (car (read-cl-string "#&0\"\""))))
  (is (equal #*01011110110001
             (car (read-cl-string "#&14\"z#\""))))
  )

(defun test-me ()
  (run! 'cl-emacs/lib/reader))

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/lib/reader)
