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

(uiop:define-package :cl-emacs/lib/reader
    (:use
     :defstar
     :cl-emacs/lib/log
     :common-lisp
     :cl-emacs/lib/elisp-packages
     :cl-emacs/lib/reader-utils
     :cl-emacs/lib/character-reader
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     )
  (:import-from :alexandria
                #:doplist)
  (:import-from :serapeum
                #:fixnump
                #:memq)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:chartables #:cl-emacs/types/chartables)
                    (#:textprop #:cl-emacs/textprop)
                    (#:creader #:cl-emacs/lib/character-reader)
                    )
  (:export #:read-cl-string
           #:read-simple
           )
  )

(in-package :cl-emacs/lib/reader)
;; (log-enable :cl-emacs/lib/reader :debug2)
(log-enable :cl-emacs/lib/reader :info)
;; (def-suite cl-emacs/lib/reader)
;; (in-suite cl-emacs/lib/reader)
(named-readtables:in-readtable elisp-function-syntax)
;; main reader documentation is here
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Lisp-Data-Types



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
                         (setf (aref *state-names* ,id) ,(cl:symbol-name state))
                         (setf (aref *transitions* ,id) (cl:symbol-function sym))
                         )))
     body1)
    (setq body1  (cl:append (loop for state in states
                                  for id from 0
                                  collect `(defparameter ,state ,id))
                            body1))
    (push `(defparameter *transitions* (make-array ,(cl:length states))) body1)
    (push `(defparameter *state-names* (make-array ,(cl:length states))) body1)

    (cons 'progn body1)))
(define-states
    state/toplevel
  state/symbol
  state/symbol-escaped
  state/character
  state/named-character
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
  (pointers (make-hash-table :test 'eq)))

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
  (log-debug1 "<- end list")
  (multiple-value-bind (reversed-list mod-frame) (pop-stack-frame reader)
    (let (result-list)
      (log-debug2 "reversed-list ~s" reversed-list)
      (loop for element in reversed-list
            for idx from 0
            do (cond
                 ((eq element '|.|)
                  (cond
                    ((= idx 0)
                     ;; dot in 0 index is considered as normal symbol
                     (push 'el::|.| result-list))
                    ((= idx 1)
                     (let ((last-element (car result-list)))
                       (setq result-list (if (eq last-element 'el::nil)
                                             nil
                                             last-element) )))
                    (t
                     ;; (push element result-list)
                     (error 'invalid-reader-input-error :details "dot in wrong position")
                     )))
                 (t (push element result-list))))
      (log-debug2 "result list:~s" result-list)
      (end-collector reader result-list mod-frame))))

(defun* end-vector ((reader reader))
  (log-debug1 "<- end vector")
  (multiple-value-bind (reversed-list mod-frame) (pop-stack-frame reader)
    (let* ((len (cl:length reversed-list))
           (parsed (make-array len)))
      (loop for element in reversed-list
            for pos from 0
            for idx from (1- len) downto 0
            do (when (and (eq element '|.|) (> pos 0))
                 (error 'invalid-reader-input-error :details "dot makes no sense in vector"))
               (setf (aref parsed idx) (if (eq element '|.|)
                                           'el::|.|
                                           element))
            )
      (end-collector reader parsed mod-frame)
      )
    ))

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
            ;; (push-extra-char reader char)
            ;; (push-extra-char reader char)
            (start-collector reader)
            (change-state reader state/symbol))
           ((simple-digit-char-p char)
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
            (push-modifier reader 'record-or-hash-table))
           ((eq char #\^)
            (pop (car mod-stack))
            (push-modifier reader 'chartable))
           ((eq char #\&)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/bool-vector-len))
           ((eq char #\$)
            (pop (car mod-stack))
            (start-collector reader)
            (change-state reader state/symbol)
            ;;; always return nil, because this is bytecode-specific stuff
            (push-to-reader-stack reader #\n)
            (push-to-reader-stack reader #\i)
            (push-to-reader-stack reader #\l)
            )
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
           (error 'incomplete-reader-error :details (cl:format nil "unexpected semicolon inside ~s" active-modifier)))
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
         (push-modifier reader '|,@| :replace-last t))
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
  (log-debug1 "<- end pstring started")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (let* ((rev-frame (nreverse stack-frame))
           (collector (car rev-frame))
           (char-list (cdr rev-frame))
           (cl-buffer (char-list-to-cl-string char-list))
           (buffer-len (cl:length cl-buffer))
           (start-position 0)
           (string-is-over nil)
           (current-chunk "")
           ;; we respect previous multibyte flag if it was already set on collector
           (multibyte (pstrings:pstring-multibyte collector)))
      (unless (pstrings:pstring-p collector)
        (error 'invalid-reader-input-error :details
               "pstring reader not initialized properly (bug)"))
      (log-debug2 "collector:~s" collector)

      (when el::string-multibyte-flag-emacs-compatible
        (unless multibyte
          ;; string will be multibyte only if during reading procedure we encounter non-ascii charactor
          ;; nobody cares about actualy multibyte characters during read, maybe because emacs postpone
          ;; their processing on later time
          (loop for char across cl-buffer
                do (when (>= (char-code char) 128)
                     (setf multibyte t)
                     (return)))))
      (setq
       current-chunk
       (with-output-to-string (stream)
         (setq
          cl-buffer
          (loop
            while (< start-position buffer-len)
            for char = nil
            for is-unicode-spec = (when el::string-multibyte-flag-emacs-compatible
                                    (unless multibyte
                                      (and (>= (cl:length cl-buffer) (1+ start-position))
                                           (cl:char-equal (aref cl-buffer start-position) #\\)
                                           (cl:char-equal (aref cl-buffer (1+ start-position)) #\u))
                                      ))
            do (labels ((write-new-char (code)
                          ;; character can be nil if it should be ignored according to
                          ;; the emacs escape syntax, like "\ "
                          (when char
                            (when (and el::string-multibyte-flag-emacs-compatible  is-unicode-spec (>= code #x80))
                              (setq multibyte t))
                            (cl:write-char (safe-code-char char) stream))))
                 (handler-case
                     (progn
                       (log-debug2 "read character start-position:~s cl-buffer:~s"
                                   start-position cl-buffer)
                       (setq char (read-string-character cl-buffer
                                                         :start-position start-position))
                       ;; complete read means that current string data is over
                       ;; but string reader can continue
                       (log-debug2 "normal complete read")
                       (write-new-char char)
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
                          (write-new-char char)
                          (incf start-position creader::position)
                          (log-debug2 "moving position +~s=~s" creader::position
                                      start-position)
                          ))
                       )
                     )))
            finally (return "")))))

      (log-debug2 "current-chunk:~s cl-buffer:~s" current-chunk cl-buffer)
      (when el::string-multibyte-flag-emacs-compatible
        (unless multibyte
          ;; check for another magic "multibyte" constant
          (loop for char across current-chunk
                do (when (>= (char-code char) 2048)
                     (setf multibyte t)
                     (return)))))
      ;; accumulate chunk to collector
      (pstrings:ninsert (pstrings:build-pstring current-chunk) collector)
      (when el::string-multibyte-flag-emacs-compatible
        ;; completely ignore what pstrings internal multibyte analyzer thinks
        ;; during read process only reader approach is right (sarcazm)
        (setf (pstrings:pstring-multibyte collector) multibyte)
        )
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
  (log-debug1 "<- end character")
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
       (end-character reader)
       (change-state reader state/toplevel))
      ((and (eq char #\{) (equal (car stack) '(#\N #\\)))
       (push char (car stack))
       (change-state reader state/named-character))
      (t
       (push char (car stack))))))
(defun* state/named-character-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((eq char #\})
       (push char (car stack))
       (change-state reader state/character))
      (t
       (push char (car stack))))))

(defun* make-el-symbol (string &key intern)
  (let ((symbol-name (with-output-to-string (stream)
                       (loop for char across string
                             do (when (or (upper-case-p char) (eq char #\_))
                                  (cl:write-char #\_ stream))
                                (cl:write-char (char-upcase char) stream)))))
    (if intern
        ;; TODO: use emacs functions here
        (cl:intern symbol-name :cl-emacs/elisp )
        (cl:make-symbol symbol-name))))

;; symbol
(defun* end-symbol ((reader reader))
  (log-debug1 "<- end symbol")
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
      ;; dot special case
      (when (eq parsed 'el::|.|)
        (setq parsed '|.|))
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

(defun* get-pointer-cons ((reader reader) (pointer-number fixnum) &key force-recreate)
  #M"return already defined pointer value if it exists,
     or creates empty placeholder and return it
     car = T if pointer is defined
     cdr = fake placeholder memory cell if pointer is not defined, or real value if car is T"
  (log-debug1 "get-pointer-cons pointer-number:~s force-recreate:~s" pointer-number force-recreate)
  (with-slots (pointers) reader
    (multiple-value-bind (cons found) (gethash pointer-number pointers)
      (when (or force-recreate (not found))
        (let ((placeholder (cons nil nil)))
          (setq cons (cons nil placeholder))
          (setf (gethash pointer-number pointers) cons)))
      cons)))

;; pointer

(defun* end-pointer ((reader reader) &key new-pointer)
  (log-debug1 "<- end pointer (new-pointer:~s)" new-pointer)
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (with-slots (pointers) reader
      (let* ((chardata (char-list-to-cl-string (nreverse stack-frame)))
             (pointer-number (parse-elisp-number chardata))
             )
        (unless pointer-number
          (error 'invalid-reader-input-error :details "can't parse pointer number"))
        (if new-pointer
            ;;; following emacs logic:
            ;;; recreate pointer on reader event, not memory structure logic.
            ;;; So here we should just forget about previous pointer information
            (let ((placeholder (cdr (get-pointer-cons reader pointer-number :force-recreate t))))
              ;;; we can't use pointer number as reference, because when we will finish reading operation
              ;;; pointer with this number may be already very different. But we still have our unique
              ;;; placeholder, so we should care about them
              (push-modifier reader placeholder)
              (push-modifier reader pointer-number)
              (push-modifier reader 'new-pointer)
              ;;; this modifier will be processed later, if on this moment pointer will be not defined yet
              (change-state reader state/toplevel))
            (end-collector reader (cdr (get-pointer-cons reader pointer-number)) mod-frame))))))

(defun* state/pointer-transition ((reader reader) (char character))
  (with-slots (stack) reader
    (cond
      ((simple-digit-char-p char)
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
  (log-debug1 "<- end radix")
  (multiple-value-bind (stack-frame mod-frame) (pop-stack-frame reader)
    (unless stack-frame
      (error 'invalid-reader-input-error :details "empty number definition"))
    (let ((parsed (reversed-list-to-number stack-frame radix-bits)))
      (end-collector reader parsed mod-frame))))
(defun* generic-radix-transition ((reader reader) (char character) (radix-bits fixnum))
  (let* ((radix (ash 1 radix-bits))
         (parsed (and (characterp char) (simple-digit-char-p char radix)))
         (sign (and (characterp char) (memq char '(#\+ #\-)))))
    (with-slots (stack) reader
      (cond
        ((and sign (null (car (reader-stack reader))))
         (when (cl:char-equal char #\-)
           (push-modifier reader '-)))
        (parsed
         (push-to-reader-stack reader parsed))
        ((or (char-end-of-statement-p char) sign)
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
  (log-debug1 "<- end bool-vector-len")
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
      ((simple-digit-char-p char)
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
  #M"tree - memory structure
     old-value - placeholder
     new-value - real-value
     stack - internal structure for recursion"
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
                                  placeholder
                                  pointers-hash
                                  )
  #M"something - memory structure, what pointer will reference to
     pointer-number - pointer id
     placeholder - link to pointer placeholder cons
     pointers-hash - all pointers storage"

  ;;; redefine pointer to new value, if passed placeholder really equal to stored in hashmap
  ;;; otherwise it means that pointer with this number was already redefined in reader memory,
  ;;; so we don't care about new reads with same pointer anymore
  (when (eq placeholder (cdr (gethash pointer-number pointers-hash)))
    (log-debug2 "setting pointer #~a to current object ~s" pointer-number something)
    (setf (gethash pointer-number pointers-hash) (cons t something)))

  (replace-placeholder-in-tree something placeholder something))

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
    (setq ht (cl:apply '@make-hash-table args))
    (doplist (key val data)
             (@puthash key val ht))
    ht)
  )

(defun* transform-to-record (something)
  (log-debug2 "starting record parsing ~s" something)
  (let* ((len (cl:length something))
         (record-type (car something))
         (parsed (make-array len)))
    (unless (> len 0)
      (error 'invalid-reader-input-error
             :details "record definition is not complete"))
    (unless (symbolp record-type)
      (error 'invalid-reader-input-error
             :details (cl:format nil "record type should be a symbol: ~s" record-type)))

    (loop for element in something
          for idx from 0
          do (setf (aref parsed idx) element))
    (setf (cl:get record-type 'el::type) 'el::record)
    parsed))

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
             ((symbol \#\:)
              (when (eq something '|.|)
                (log-debug2 "processing dot as symbol")
                (setq something 'el::|.|)))
             (\'
              (setq something (list 'el::quote something)))
             (\`
              (setq something (list 'el::\` something)))
             (\,
              (setq something (list 'el::\, something)))
             (|,@|
              (setq something (list 'el::|,@| something)))
             (\#\'
              (setq something (list 'el::function something)))
             (\-
              (setq something (- something))
              )
             (new-pointer
              (let* ((pointer-number (pop modifiers))
                     (placeholder (pop modifiers)))
                (log-debug2 "processing new-pointer modifier: pointer-number:~s" pointer-number)
                (setq something
                      (transform-to-new-pointer
                       something
                       :pointer-number pointer-number
                       :placeholder placeholder
                       :pointers-hash pointers))
                (setq stack (replace-placeholder-in-tree stack placeholder something))))
             (pstring-full
              (setq something (transform-to-pstring something)))
             (record-or-hash-table
              (if (eq (car something) 'el::hash-table)
                  (setq something (transform-to-hash-table (cdr something)))
                  (setq something (transform-to-record something))))
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
    (log-debug1 "-> transition to ~s" (aref *state-names* new-state))
    (log-debug2 "   mod-stack: ~s" mod-stack)
    (log-debug2 "   stack: ~s" stack)
    (setq state new-state)
    ))
(defun* process-char ((reader reader) (char character) &key new-state)
  (with-slots (state) reader
    (log-debug2 ".. process char ~s in state ~s" char state)
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
      (log-debug2 "start read from position: ~s" external-position)
      (handler-case
          (handler-case
              (loop for read-position from external-position
                    do (let ((char (cl:read-char stream)))
                         (log-debug2 "read char result: ~s" char)
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
        (error 'empty-reader-error :details "Reader not started - empty input"))
      (when  (cdr stack)
        (error 'incomplete-reader-error :details "Lisp structure is not complete"))
      (when (car mod-stack)
        (error 'incomplete-reader-error :details (cl:format nil "modifiers ~s without defined body" (car mod-stack))))
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
  (log-debug2 "* read cl-string:~s start:~s end:~s" cl-string start end)
  (with-input-from-string (stream cl-string :start start :end end)
    (multiple-value-bind (result reader) (read-internal stream start)
      (with-slots (character-counter) reader
        (cons result character-counter)))))

(defun* read-simple ((cl-string cl:string))
  #M"Simple read function interface for unit testing, returns just resulting FORM"
  (car (read-cl-string cl-string)))

;; last function, should be called after all transitions definition
(define-transitions)

(cl-emacs/lib/log::get-max-loglevel)

(in-package :cl-emacs/elisp)
(reexport-symbols :cl-emacs/lib/reader)
