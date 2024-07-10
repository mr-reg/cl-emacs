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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/lib/character-reader
    (:use
     :cl-emacs/lib/log
     :fiveam
     :defstar
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/fns
     :cl-emacs/lib/reader-utils
     :cl-emacs/lib/commons)

  (:export #:read-emacs-character
           #:read-string-character
           #:extra-symbols-in-character-spec-error
           #:invalid-character-spec-error)
  (:local-nicknames (:el :cl-emacs/elisp)))
(in-package :cl-emacs/lib/character-reader)
(log-enable :cl-emacs/lib/character-reader :info)
;; (log-enable :cl-emacs/lib/character-reader :debug2)
(def-suite cl-emacs/lib/character-reader)
(in-suite cl-emacs/lib/character-reader)
(named-readtables:in-readtable mstrings:mstring-syntax)

(define-condition character-reader-error ()
  ((input :initarg :input
          :initform ""
          :type string)
   (start-position :initarg :start-position
                   :initform 0
                   :type fixnum)
   (position :initarg :position
             :initform -1
             :type fixnum)
   (details :initarg :details
            :initform ""
            :type string)))
(defmethod print-object ((e character-reader-error) stream)
  (with-slots (input details start-position position) e
    (cl:format stream "#<~a details:~s, input:~s>"
               (class-name (class-of e)) details
               (str:substring
                start-position
                (if (= -1 position) t
                    (min (+ 1 start-position position) (cl:length input)))
                input)))
  )
(define-condition invalid-character-spec-error (character-reader-error)
  ())

(define-condition extra-symbols-in-character-spec-error (character-reader-error)
  (
   (parsed-code :initarg :parsed-code
                :initform 0
                :type (or null fixnum))))
(defmethod initialize-instance :after ((e extra-symbols-in-character-spec-error) &key position input parsed-code)
  (with-slots (details) e
    (let ((rem (- (cl:length input) position)))
      (setq details (cl:format nil "~a last character~p not parsed. Current result ~a"
                               rem rem parsed-code)))))

(defun* decode-named-char ((raw-input string) (name string))
  (let* ((clean-name (str:replace-all "\n" "" name))
         decoded
         )
    (if (str:starts-with-p "U+" clean-name)
        (let ((hex-part (str:substring 2 t clean-name)))
          (loop for char across hex-part
                do (unless (digit-char-p char 16)
                     (error 'invalid-character-spec-error
                            :input raw-input
                            :details (cl:format nil "invalid character in hex notation ~a" char))))
          (let* ((parsed (parse-integer hex-part :radix 16)))
            ;; for wrong character (code-char) will be
            ;; nil in CCL
            ;; U~X in SBCL

            (when (and (< parsed cl-unicode:+code-point-limit+) (safe-code-char parsed)
                       ;; this is filter to ignore characters without unicode name, but
                       ;; in SBCL this does not work consistently
                       ;; (not (string= (char-name (code-char parsed)) (cl:format nil "U~X" parsed)))
                       )
              (setq decoded parsed))))
        (let ((parsed (cl-unicode:character-named clean-name)))
          (when parsed
            (setq decoded (char-code parsed)))))
    (or decoded
        (error 'invalid-character-spec-error
               :input raw-input
               :details (cl:format nil "can't recognize unicode name ~a" name)))))



;;
;; Let's decode these ancient alien codes
;;
;;               ========
;;          ====           ===
;;       ==                    ==
;;     *    .---------.    .-----.*
;;   *     !     @@    !   !   @@  .*
;;  *       `---------'     `-------'*
;; 1                                  1
;; 1                   __======.   ,  1
;; 1                __=`       |   ,  1
;;  1              -`         /       1
;;  1                                *
;;    *                             *
;;      ==                       ==
;;         ==                  =
;;             ===============
;;                  // tt    \\
;;                //    tt      \\
;;              //      ####=======33
;;             EE======####
;;                       tt
;;                       tt

(defun* read-emacs-character ((input string) &key string-mode (start-position 0))
  #M"Read emacs character notation with all it's weird exceptions.
     Initial ? sign should be omitted
     Return value: character code, because emacs has no special type for character

     when string-mode = t, reader uses in-string escape syntax, which
     is a little different for some reason.

     for in-string mode result cound be nil, which means that this character
     specification is ignored"
  (block parsing
    (let ((mode 'toplevel)
          (n-chars (cl:length input))
          (position 0)
          (modifiers 0)
          (caret 0)
          (control 0)
          (super 0)
          (shift 0)
          (named-list)
          ;; this will be true only if we found slashed space or newline in string
          (ignore-whitespace nil)
          (octals) (hex))
      (labels ((change-mode (new-mode)
                 (log-debug2 "change-mode ~s" new-mode)
                 (setq mode new-mode))
               (return-result (result)
                 #M"result can be:
                    nil if no character found, problem in non-string mode
                    number
                    "
                 (when (and (null result) (not string-mode))
                   (error 'invalid-character-spec-error
                          :input input :position position
                          :start-position start-position
                          :details "character specification is empty"))


                 ;; modifier + null result means error
                 ;; null can be here only in string mode
                 (loop for _ below (+ caret control)
                       do ;; any modifier makes slashed whitespace important
                          (setq ignore-whitespace nil)
                          (cond
                            ((null result)
                             (error 'invalid-character-spec-error
                                    :input input :position position
                                    :start-position start-position
                                    :details "invalid modifier in string"))
                            ((and string-mode (= result 32))
                             (setq result 0))
                            ((= result 63) (incf result 64))
                            ((< 96 result 123) (decf result 96))
                            ((<= 64 result 95) (decf result 64))
                            (t
                             (if string-mode
                                 (error 'invalid-character-spec-error
                                        :input input :position position
                                        :start-position start-position
                                        :details "invalid modifier for string")
                                 (progn
                                   (setq result (logior result #x4000000))))
                             )))
                 ;; super modifier is impossible in string
                 (loop for _ below (+ super)
                       do (setq result (logior result #x800000)))
                 (loop for _ below (+ shift)
                       do ;; any modifier makes slashed whitespace important
                          (setq ignore-whitespace nil)
                          (if string-mode
                              (cond
                                ((null result)
                                 (error 'invalid-character-spec-error
                                        :input input :position position
                                        :start-position start-position
                                        :details "invalid modifier in string"))
                                ((<= 97 result 122) (decf result 32))
                                ((<= 65 result 90) (progn))
                                (t (error 'invalid-character-spec-error
                                          :input input :position position
                                          :start-position start-position
                                          :details "invalid modifier for string")))
                              (setq result (logior result #x2000000))))
                 (unless (zerop modifiers)
                   ;; any modifier makes slashed whitespace important
                   (setq ignore-whitespace nil)
                   (unless result
                     (error 'invalid-character-spec-error
                            :input input :position position
                            :start-position start-position
                            :details "invalid modifier in string"))
                   (setq result (logior result modifiers))                   )

                 (when (and ignore-whitespace string-mode)
                   (setq result nil))
                 (unless (= position n-chars)
                   (if (not string-mode)
                       (let ((first-char (aref input (+ position start-position))))
                         (if (or (char-end-of-statement-p first-char)
                                 (memq first-char '(#\? #\.)))
                             ;; "valid" symbol ending, it is not an error
                             (error 'extra-symbols-in-character-spec-error
                                    :input input :position position
                                    :start-position start-position
                                    :position position
                                    :parsed-code result)
                             (error 'invalid-character-spec-error
                                    :input input :position position
                                    :start-position start-position
                                    :details (cl:format nil "can't parse extra character ~s in the end" first-char))))

                       (error 'extra-symbols-in-character-spec-error
                              :input input :position position
                              :start-position start-position
                              :position position
                              :parsed-code result)
                       )
                   )

                 (return-from parsing result))

               (process-one-character (char)
                 (log-debug2 "process-one-character ~s" char)
                 (ecase mode
                   (toplevel
                    (cond
                      ((null char)
                       (cond
                         ((and (not string-mode)(or (> caret 0) (> super 0)))
                          (setf modifiers 0)
                          (setf caret 0)
                          (setf super 0)
                          (return-result -1))
                         (t (return-result nil))))
                      ((and (memq char '(#\" #\( #\) #\[ #\])) (not string-mode))
                       (error 'invalid-character-spec-error
                              :input input :position position
                              :start-position start-position
                              :details (cl:format nil "invalid symbol found ~a" char)))
                      ((and (eq char #\") string-mode)
                       (decf position)
                       (return-result nil))
                      ((eq char #\\) (change-mode 'special))
                      (t (return-result (char-code char)))))
                   (special
                    (cond
                      ((digit-char-p char 8)
                       (change-mode 'octal)
                       (push (digit-char-p char 8) octals))
                      ((eq char #\A)
                       (if string-mode
                           (error 'invalid-character-spec-error
                                  :input input :position position
                                  :start-position start-position
                                  :details "invalid modifier for string")
                           (setq modifiers (logior modifiers #x400000)))
                       (change-mode 'modifier))
                      ((eq char #\C)
                       (incf control)
                       (change-mode 'modifier))
                      ((eq char #\H)
                       (if string-mode
                           (error 'invalid-character-spec-error
                                  :input input :position position
                                  :start-position start-position
                                  :details "invalid modifier for string")
                           (setq modifiers (logior modifiers #x1000000)))
                       (change-mode 'modifier))
                      ((eq char #\M)
                       (if string-mode
                           (setq modifiers (logior modifiers #x80))
                           (setq modifiers (logior modifiers #x8000000)))
                       (change-mode 'modifier))
                      ((eq char #\s)
                       (if string-mode
                           (return-result 32)
                           (progn
                             (incf super)
                             (change-mode 'super-modifier))))
                      ((eq char #\S)
                       (incf shift)
                       (change-mode 'modifier))
                      ((eq char #\^) (incf caret)(change-mode 'toplevel))
                      ((eq char #\d) (return-result 127))
                      ((eq char #\e) (return-result 27))
                      ((eq char #\n) (return-result 10))
                      ((eq char #\r) (return-result 13))
                      ((eq char #\t) (return-result 9))
                      ((eq char #\v) (return-result 11))
                      ((eq char #\x) (change-mode 'hexadecimal))
                      ((eq char #\u) (change-mode '4-unicode))
                      ((eq char #\U) (change-mode '8-unicode))
                      ((eq char #\N) (change-mode 'named))
                      ((and (eq char #\NewLine) (not string-mode))
                       (return-result -1))
                      ((memq char '(#\a #\b #\f))
                       (return-result (- (char-code char) 90)))
                      (t
                       (when (memq char '(#\space #\newline))
                         (setq ignore-whitespace t))
                       (return-result (char-code char)))))
                   (octal
                    (cond
                      ((null char)
                       (return-result (reversed-list-to-number octals 3)))
                      ((digit-char-p char 8)
                       (push (digit-char-p char 8) octals))
                      ;; character is not octal
                      (t
                       (decf position)
                       (return-result (reversed-list-to-number octals 3)))
                      ))
                   (hexadecimal
                    (when (and char
                               (not (char-whitespace-p char))
                               (not (char-end-of-statement-p char)))
                      (if (digit-char-p char 16)
                          (push (digit-char-p char 16) hex)
                          (if string-mode
                              (progn
                                (decf position)
                                (setq char nil))
                              (error 'invalid-character-spec-error
                                     :input input :position position
                                     :start-position start-position
                                     :details (cl:format nil "bad symbol in hexadecimal mode ~a" char)))))
                    (when (or (null char) (char-whitespace-p char) (char-end-of-statement-p char))
                      (when (> (cl:length hex) 8)
                        (error 'invalid-character-spec-error
                               :input input :position position
                               :start-position start-position
                               :details "in hexadecimal mode you can use only 8 numbers in character definition"))
                      (when (and char (or (char-whitespace-p char) (char-end-of-statement-p char)))
                        (decf position))
                      (let ((result (reversed-list-to-number hex 4)))
                        (return-result result))))
                   (4-unicode
                    (when char
                      (if (digit-char-p char 16)
                          (push (digit-char-p char 16) hex)
                          (error 'invalid-character-spec-error
                                 :input input :position position
                                 :start-position start-position
                                 :details (cl:format nil "bad symbol in 4-unicode mode ~a" char))))
                    (when (or (null char) (>= (cl:length hex) 4))
                      (unless (= 4 (cl:length hex))
                        (error 'invalid-character-spec-error
                               :input input :position position
                               :start-position start-position
                               :details "spec should should contain exactly 4 hexadecimal symbols"))
                      (let ((result (reversed-list-to-number hex 4)))
                        (return-result result))))
                   (8-unicode
                    (when char
                      (if (digit-char-p char 16)
                          (push (digit-char-p char 16) hex)
                          (error 'invalid-character-spec-error
                                 :input input :position position
                                 :start-position start-position
                                 :details (cl:format nil "bad symbol in 8-unicode mode ~a" char))))
                    (when (or (null char) (>= (cl:length hex) 8))
                      (unless (= 8 (cl:length hex))
                        (error 'invalid-character-spec-error
                               :input input :position position
                               :start-position start-position
                               :details "spec should should contain exactly 8 hexadecimal symbols"))
                      (let ((result (reversed-list-to-number hex 4)))
                        (when (> result cl-unicode:+code-point-limit+)
                          (error 'invalid-character-spec-error
                                 :input input :position position
                                 :start-position start-position
                                 :details "character not in unicode range"))
                        (return-result result))))
                   (modifier
                    (cond
                      ((null char)
                       (error 'invalid-character-spec-error
                              :input input :position position
                              :start-position start-position
                              :details "not found symbol - after the modifier"))
                      ((eq char #\-)
                       (change-mode 'toplevel))
                      (t (error 'invalid-character-spec-error
                                :input input :position position
                                :start-position start-position
                                :details (cl:format nil "bad symbol after the modifier ~a" char)))))
                   (super-modifier
                    (cond
                      ((eq char #\-)
                       (change-mode 'toplevel))
                      (t
                       (decf super)
                       (when char
                         (decf position))
                       (return-result 32) )))
                   (named
                    (cond
                      ((null char)
                       (error 'invalid-character-spec-error
                              :input input :position position
                              :start-position start-position
                              :details "curly brace should open after N"))
                      ((eq char #\{)
                       (change-mode 'named-in-braces))
                      (t (error 'invalid-character-spec-error
                                :input input :position position
                                :start-position start-position
                                :details (cl:format nil "bad symbol after N ~a" char)))))
                   (named-in-braces
                    (cond
                      ((null char)
                       (error 'invalid-character-spec-error
                              :input input :position position
                              :start-position start-position
                              :details "no closing curly brace"))
                      ((eq char #\})
                       (return-result (decode-named-char input (char-list-to-cl-string (nreverse named-list)))))
                      (t (push char named-list))))
                   )))
        (loop for idx from start-position below (cl:length input)
              for char = (aref input idx)
              do (incf position)
                 (process-one-character char)
              finally (progn
                        (process-one-character nil)
                        (error 'invalid-character-spec-error
                               :input input :position position
                               :start-position start-position
                               :details "unexpected end of character specification")))))))



(defun* read-string-character ((input string) &key (start-position 0))
  (read-emacs-character input :string-mode t :start-position start-position))

(defmacro extra-symbols+is ((op result form))
  `(is (,op ,result (handler-case
                        (progn
                          ,form
                          'extra-symbols-exception-not-found)
                      (extra-symbols-in-character-spec-error (e)
                        (with-slots (parsed-code) e
                          parsed-code))))))

(test test-read-single-character
  (is (= 1 (read-emacs-character ""))) (is (= 1 (read-string-character "")))
  (is (= 26 (read-emacs-character ""))) (is (= 26 (read-string-character "")))
  (is (= 27 (read-emacs-character ""))) (is (= 27 (read-string-character "")))
  (is (= 28 (read-emacs-character ""))) (is (= 28 (read-string-character "")))
  (is (= 29 (read-emacs-character ""))) (is (= 29 (read-string-character "")))
  (is (= 30 (read-emacs-character ""))) (is (= 30 (read-string-character "")))
  (is (= 31 (read-emacs-character ""))) (is (= 31 (read-string-character "")))
  (is (= 32 (read-emacs-character " "))) (is (= 32 (read-string-character " ")))
  (is (= 33 (read-emacs-character "!"))) (is (= 33 (read-string-character "!")))
  (is (= 35 (read-emacs-character "#"))) (is (= 35 (read-string-character "#")))
  (is (= 36 (read-emacs-character "$"))) (is (= 36 (read-string-character "$")))
  (is (= 37 (read-emacs-character "%"))) (is (= 37 (read-string-character "%")))
  (is (= 38 (read-emacs-character "&"))) (is (= 38 (read-string-character "&")))
  (is (= 39 (read-emacs-character "'"))) (is (= 39 (read-string-character "'")))

  (signals invalid-character-spec-error (read-emacs-character "("))
  (is (= 40 (read-string-character "(")))
  (signals invalid-character-spec-error (read-emacs-character "()"))
  (extra-symbols+is (= 40 (read-string-character "()")))
  (signals invalid-character-spec-error (read-emacs-character ")"))
  (is (= 41 (read-string-character ")")))
  (signals invalid-character-spec-error (read-emacs-character "\""))
  (extra-symbols+is (equal nil (read-string-character "\"")))
  (is (= 42 (read-emacs-character "*"))) (is (= 42 (read-string-character "*")))
  (is (= 43 (read-emacs-character "+"))) (is (= 43 (read-string-character "+")))
  (is (= 44 (read-emacs-character ","))) (is (= 44 (read-string-character ",")))
  (is (= 45 (read-emacs-character "-"))) (is (= 45 (read-string-character "-")))
  (is (= 46 (read-emacs-character "."))) (is (= 46 (read-string-character ".")))
  (is (= 47 (read-emacs-character "/"))) (is (= 47 (read-string-character "/")))
  (is (= 48 (read-emacs-character "0"))) (is (= 48 (read-string-character "0")))
  (is (= 49 (read-emacs-character "1"))) (is (= 49 (read-string-character "1")))
  (is (= 50 (read-emacs-character "2"))) (is (= 50 (read-string-character "2")))
  (is (= 51 (read-emacs-character "3"))) (is (= 51 (read-string-character "3")))
  (is (= 52 (read-emacs-character "4"))) (is (= 52 (read-string-character "4")))
  (is (= 53 (read-emacs-character "5"))) (is (= 53 (read-string-character "5")))
  (is (= 54 (read-emacs-character "6"))) (is (= 54 (read-string-character "6")))
  (is (= 55 (read-emacs-character "7"))) (is (= 55 (read-string-character "7")))
  (is (= 56 (read-emacs-character "8"))) (is (= 56 (read-string-character "8")))
  (is (= 57 (read-emacs-character "9"))) (is (= 57 (read-string-character "9")))
  (is (= 58 (read-emacs-character ":"))) (is (= 58 (read-string-character ":")))
  (is (= 59 (read-emacs-character ";"))) (is (= 59 (read-string-character ";")))
  (is (= 60 (read-emacs-character "<"))) (is (= 60 (read-string-character "<")))
  (is (= 61 (read-emacs-character "="))) (is (= 61 (read-string-character "=")))
  (is (= 62 (read-emacs-character ">"))) (is (= 62 (read-string-character ">")))
  (is (= 63 (read-emacs-character "?"))) (is (= 63 (read-string-character "?")))
  (is (= 64 (read-emacs-character "@"))) (is (= 64 (read-string-character "@")))
  (is (= 65 (read-emacs-character "A"))) (is (= 65 (read-string-character "A")))
  (is (= 90 (read-emacs-character "Z"))) (is (= 90 (read-string-character "Z")))
  (signals invalid-character-spec-error (read-emacs-character "[")) (is (= 91 (read-string-character "[")))
  (signals invalid-character-spec-error (read-emacs-character "]")) (is (= 93 (read-string-character "]")))
  (is (= 94 (read-emacs-character "^"))) (is (= 94 (read-string-character "^")))
  (is (= 121 (read-emacs-character "y"))) (is (= 121 (read-string-character "y")))
  (is (= 123 (read-emacs-character "{"))) (is (= 123 (read-string-character "{")))
  (is (= 124 (read-emacs-character "|"))) (is (= 124 (read-string-character "|")))
  (is (= 125 (read-emacs-character "}"))) (is (= 125 (read-string-character "}")))
  (is (= 126 (read-emacs-character "~"))) (is (= 126 (read-string-character "~")))
  (is (= 127 (read-emacs-character ""))) (is (= 127 (read-string-character "")))
  (is (= 160 (read-emacs-character " "))) (is (= 160 (read-string-character " ")))
  (is (= 169 (read-emacs-character "©"))) (is (= 169 (read-string-character "©")))
  (is (= 1044 (read-emacs-character "Д"))) (is (= 1044 (read-string-character "Д")))
  (is (= 1069 (read-emacs-character "Э"))) (is (= 1069 (read-string-character "Э")))
  (is (= 9785 (read-emacs-character "☹"))) (is (= 9785 (read-string-character "☹")))
  (is (= 128169 (read-emacs-character "💩"))) (is (= 128169 (read-string-character "💩")))
  (is (= 128512 (read-emacs-character "😀"))) (is (= 128512 (read-string-character "😀")))
  )

(test test-read-one-special-character
  (is (= 1 (read-emacs-character "\\"))) (is (= 1 (read-string-character "")))
  (is (= -1 (read-emacs-character (cl:format nil "\\~c" (safe-code-char 10)))))
  (is (equal nil (read-string-character (cl:format nil "\\~c" (safe-code-char 10)))))
  (is (= 13 (read-emacs-character (cl:format nil "\\~c" (safe-code-char 13)))))
  (is (= 13 (read-string-character (cl:format nil "\\~c" (safe-code-char 13)))))
  (is (= 32 (read-emacs-character "\\ "))) (is (equal nil (read-string-character "\\ ")))
  (is (= 33 (read-emacs-character "\\!"))) (is (= 33 (read-string-character "\\!")))
  (is (= 47 (read-emacs-character "\\/"))) (is (= 47 (read-string-character "\\/")))
  (is (= 0 (read-emacs-character "\\0"))) (is (= 0 (read-string-character "\\0")))
  (is (= 1 (read-emacs-character "\\1"))) (is (= 1 (read-string-character "\\1")))
  (is (= 3 (read-emacs-character "\\3"))) (is (= 3 (read-string-character "\\3")))
  (is (= 4 (read-emacs-character "\\4"))) (is (= 4 (read-string-character "\\4")))
  (is (= 5 (read-emacs-character "\\5"))) (is (= 5 (read-string-character "\\5")))
  (is (= 7 (read-emacs-character "\\7"))) (is (= 7 (read-string-character "\\7")))
  (is (= 56 (read-emacs-character "\\8"))) (is (= 56 (read-string-character "\\8")))
  (is (= 57 (read-emacs-character "\\9"))) (is (= 57 (read-string-character "\\9")))
  (is (= 64 (read-emacs-character "\\@"))) (is (= 64 (read-string-character "\\@")))
  (signals invalid-character-spec-error (read-emacs-character "\\A"))
  (signals invalid-character-spec-error (read-string-character "\\A"))
  (signals invalid-character-spec-error (read-emacs-character "\\\\A"))
  (extra-symbols+is (= 92 (read-string-character "\\\\A")))
  (signals invalid-character-spec-error (read-emacs-character "\\C"))
  (signals invalid-character-spec-error (read-string-character "\\C"))
  (signals invalid-character-spec-error (read-emacs-character "\\H"))
  (signals invalid-character-spec-error (read-string-character "\\H"))
  (signals invalid-character-spec-error (read-emacs-character "\\M"))
  (signals invalid-character-spec-error (read-string-character "\\M"))
  (signals invalid-character-spec-error (read-emacs-character "\\N"))
  (signals invalid-character-spec-error (read-string-character "\\N"))
  (signals invalid-character-spec-error (read-emacs-character "\\S"))
  (signals invalid-character-spec-error (read-string-character "\\S"))
  (signals invalid-character-spec-error (read-emacs-character "\\U"))
  (signals invalid-character-spec-error (read-string-character "\\U"))
  (is (= 90 (read-emacs-character "\\Z"))) (is (= 90 (read-string-character "\\Z")))
  (is (= 93 (read-emacs-character "\\]"))) (is (= 93 (read-string-character "\\]")))
  (is (= -1 (read-emacs-character "\\^")))
  (signals invalid-character-spec-error (read-string-character "\\^"))
  (is (= 95 (read-emacs-character "\\_"))) (is (= 95 (read-string-character "\\_")))
  (is (= 96 (read-emacs-character "\\`"))) (is (= 96 (read-string-character "\\`")))
  (is (= 7 (read-emacs-character "\\a"))) (is (= 7 (read-string-character "\\a")))
  (is (= 8 (read-emacs-character "\\b"))) (is (= 8 (read-string-character "\\b")))
  (is (= 99 (read-emacs-character "\\c"))) (is (= 99 (read-string-character "\\c")))
  (is (= 127 (read-emacs-character "\\d"))) (is (= 127 (read-string-character "\\d")))
  (is (= 27 (read-emacs-character "\\e"))) (is (= 27 (read-string-character "\\e")))
  (is (= 12 (read-emacs-character "\\f"))) (is (= 12 (read-string-character "\\f")))
  (is (= 103 (read-emacs-character "\\g"))) (is (= 103 (read-string-character "\\g")))
  (is (= 104 (read-emacs-character "\\h"))) (is (= 104 (read-string-character "\\h")))
  (is (= 105 (read-emacs-character "\\i"))) (is (= 105 (read-string-character "\\i")))
  (is (= 106 (read-emacs-character "\\j"))) (is (= 106 (read-string-character "\\j")))
  (is (= 107 (read-emacs-character "\\k"))) (is (= 107 (read-string-character "\\k")))
  (is (= 108 (read-emacs-character "\\l"))) (is (= 108 (read-string-character "\\l")))
  (is (= 109 (read-emacs-character "\\m"))) (is (= 109 (read-string-character "\\m")))
  (is (= 10 (read-emacs-character "\\n"))) (is (= 10 (read-string-character "\\n")))
  (is (= 111 (read-emacs-character "\\o"))) (is (= 111 (read-string-character "\\o")))
  (is (= 112 (read-emacs-character "\\p"))) (is (= 112 (read-string-character "\\p")))
  (is (= 113 (read-emacs-character "\\q"))) (is (= 113 (read-string-character "\\q")))
  (is (= 13 (read-emacs-character "\\r"))) (is (= 13 (read-string-character "\\r")))
  (is (= 32 (read-emacs-character "\\s"))) (is (= 32 (read-string-character "\\s")))
  (is (= 9 (read-emacs-character "\\t"))) (is (= 9 (read-string-character "\\t")))
  (signals invalid-character-spec-error (read-emacs-character "\\u"))
  (signals invalid-character-spec-error (read-string-character "\\u"))
  (is (= 11 (read-emacs-character "\\v"))) (is (= 11 (read-string-character "\\v")))
  (is (= 119 (read-emacs-character "\\w"))) (is (= 119 (read-string-character "\\w")))
  (is (= 0 (read-emacs-character "\\x"))) (is (= 0 (read-string-character "\\x")))
  (is (= 121 (read-emacs-character "\\y"))) (is (= 121 (read-string-character "\\y")))
  (is (= 122 (read-emacs-character "\\z"))) (is (= 122 (read-string-character "\\z")))
  (is (= 123 (read-emacs-character "\\{"))) (is (= 123 (read-string-character "\\{")))
  (is (= 124 (read-emacs-character "\\|"))) (is (= 124 (read-string-character "\\|")))
  (is (= 125 (read-emacs-character "\\}"))) (is (= 125 (read-string-character "\\}")))
  (is (= 126 (read-emacs-character "\\~"))) (is (= 126 (read-string-character "\\~")))
  (is (= 127 (read-emacs-character "\\"))) (is (= 127 (read-string-character "\\")))
  (is (= 160 (read-emacs-character "\\ "))) (is (= 160 (read-string-character "\\ ")))
  (is (= 1044 (read-emacs-character "\\Д"))) (is (= 1044 (read-string-character "\\Д")))
  )

(test test-read-modifiers-general
  (signals invalid-character-spec-error (read-emacs-character "\\A"))
  (signals invalid-character-spec-error (read-string-character "\\A"))

  (signals invalid-character-spec-error (read-emacs-character "\\A-"))
  (signals invalid-character-spec-error (read-string-character "\\A-"))

  (signals invalid-character-spec-error (read-emacs-character "\\AA"))
  (signals invalid-character-spec-error (read-string-character "\\AA"))

  (signals invalid-character-spec-error (read-emacs-character "\\C"))
  (signals invalid-character-spec-error (read-string-character "\\C"))

  (signals invalid-character-spec-error (read-emacs-character "\\C-"))
  (signals invalid-character-spec-error (read-string-character "\\C-"))

  (signals invalid-character-spec-error (read-emacs-character "\\CC"))
  (signals invalid-character-spec-error (read-string-character "\\CC"))

  (signals invalid-character-spec-error (read-emacs-character "\\$-a"))
  (extra-symbols+is (= 36 (read-string-character "\\$-a")))

  (is (= 37748843 (read-emacs-character "\\A-\\S-k")))
  (signals invalid-character-spec-error (read-string-character "\\A-\\S-k"))

  (is (= 71303216 (read-emacs-character "\\^\\A-0")))
  (signals invalid-character-spec-error (read-string-character "\\^\\A-0"))
  (is (= 71303168 (read-emacs-character "\\^\\A-\\0")))

  (is (= 67108865 (read-emacs-character "\\C-\\C-a")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\C-a"))
  (is (= 67108877 (read-emacs-character "\\C-\\C-m")))
  (is (= 16777240 (read-emacs-character "\\C-\\H-x")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\H-x"))
  (is (= 201326625 (read-emacs-character "\\C-\\M-!")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\M-!"))
  (is (= 201326649 (read-emacs-character "\\C-\\M-9")))
  (is (= 201326654 (read-emacs-character "\\C-\\M->")))
  (is (= 201326626 (read-emacs-character "\\C-\\M-\\\"")))
  (is (= 201326631 (read-emacs-character "\\C-\\M-\\'")))
  (is (= 201326651 (read-emacs-character "\\C-\\M-\\;")))
  (is (= 167772182 (read-emacs-character "\\C-\\M-\\S-v")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\M-\\S-v"))
  (is (= 134217756 (read-emacs-character "\\C-\\M-\\\\")))
  (is (= 201326624 (read-emacs-character "\\C-\\M-\\s")))
  (is (= 134217759 (read-emacs-character "\\C-\\M-_")))
  (is (= 134217729 (read-emacs-character "\\C-\\M-a")))
  (is (= 134217750 (read-emacs-character "\\C-\\M-v")))
  (is (= 201326825 (read-emacs-character "\\C-\\M-é")))
  (is (= 33554447 (read-emacs-character "\\C-\\S-O")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\S-O"))
  (is (= 33554433 (read-emacs-character "\\C-\\S-a")))
  (is (= 33554454 (read-emacs-character "\\C-\\S-v")))
  (is (= 75497504 (read-emacs-character "\\C-\\s- ")))
  (is (= -1 (read-emacs-character "\\C-\\s-")))

  (is (= 201326624 (read-emacs-character "\\M-\\C-\\ ")))
  ;; can't be supported in human code without hardcoded exception, sorry
  ;; (signals invalid-character-spec-error (read-string-character "\\M-\\C-\\ "))

  (is (= 134217730 (read-emacs-character "\\M-\\C-b")))
  (is (= 130 (read-string-character "\\M-\\C-b")))

  (is (= -1 (read-emacs-character "\\A-\\^")))
  (signals invalid-character-spec-error (read-string-character "\\A-\\^"))
  )

(test test-read-caret-modifier
  (is (= 67108896 (read-emacs-character "\\^ ")))
  (is (= 0 (read-string-character "\\^ ")))

  (is (= 67108897 (read-emacs-character "\\^!")))
  (signals invalid-character-spec-error (read-string-character "\\^!"))

  (signals invalid-character-spec-error (read-emacs-character "\\^\""))
  (signals invalid-character-spec-error (read-string-character "\\^\""))

  (is (= 67108926 (read-emacs-character "\\^>")))
  (signals invalid-character-spec-error (read-string-character "\\^>"))

  (is (= 127 (read-emacs-character "\\^?"))) (is (= 127 (read-string-character "\\^?")))
  (is (= 0 (read-emacs-character "\\^@"))) (is (= 0 (read-string-character "\\^@")))
  (is (= 1 (read-emacs-character "\\^A"))) (is (= 1 (read-string-character "\\^A")))
  (is (= 26 (read-emacs-character "\\^Z"))) (is (= 26 (read-string-character "\\^Z")))
  (is (= 29 (read-emacs-character "\\^\\]"))) (is (= 29 (read-string-character "\\^\\]")))
  (is (= 31 (read-emacs-character "\\^_"))) (is (= 31 (read-string-character "\\^_")))

  (is (= 67108960 (read-emacs-character "\\^`")))
  (signals invalid-character-spec-error (read-string-character "\\^`"))

  (is (= 1 (read-emacs-character "\\^a"))) (is (= 1 (read-string-character "\\^a")))
  (is (= 26 (read-emacs-character "\\^z"))) (is (= 26 (read-string-character "\\^z")))

  (is (= 67108987 (read-emacs-character "\\^{")))
  (signals invalid-character-spec-error (read-string-character "\\^{"))

  (is (= 67108865 (read-emacs-character "\\^\\^a")))
  (signals invalid-character-spec-error (read-string-character "\\^\\^a"))
  )

(test test-read-a-modifier
  (is (= 4194304 (read-emacs-character "\\A-\\0")))
  (signals invalid-character-spec-error (read-string-character "\\A-\\0"))
  (is (= 4194304 (read-emacs-character "\\A-\\^@")))
  (is (= 4194403 (read-emacs-character "\\A-c")))
  (signals invalid-character-spec-error (read-string-character "\\A-c"))
  )

(test test-read-c-modifier
  (is (= 67108896 (read-emacs-character "\\C-\\ "))) (is (= 0 (read-string-character "\\C-\\ ")))

  (is (= 67108898 (read-emacs-character "\\C-\\\"")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\\""))

  (is (= 67108864 (read-emacs-character "\\C-\\0")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\0"))
  (is (= 67108911 (read-emacs-character "\\C-/")))
  (signals invalid-character-spec-error (read-string-character "\\C-/"))
  (is (= 67108925 (read-emacs-character "\\C-=")))
  (signals invalid-character-spec-error (read-string-character "\\C-="))

  (is (= 127 (read-emacs-character "\\C-?"))) (is (= 127 (read-string-character "\\C-?")))
  (is (= 0 (read-emacs-character "\\C-@"))) (is (= 0 (read-string-character "\\C-@")))
  (is (= 19 (read-emacs-character "\\C-S"))) (is (= 19 (read-string-character "\\C-S")))
  (is (= 67108864 (read-emacs-character "\\C-\\0")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\0"))

  (is (= 201326825 (read-emacs-character "\\C-\\M-é")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\M-é"))

  (is (= 27 (read-emacs-character "\\C-\\["))) (is (= 27 (read-string-character "\\C-\\[")))
  (is (= 28 (read-emacs-character "\\C-\\\\"))) (is (= 28 (read-string-character "\\C-\\\\")))
  (is (= 67108991 (read-emacs-character "\\C-\\d")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\d"))
  (is (= 30 (read-emacs-character "\\C-^"))) (is (= 30 (read-string-character "\\C-^")))
  (is (= 1 (read-emacs-character "\\C-a"))) (is (= 1 (read-string-character "\\C-a")))
  (is (= 26 (read-emacs-character "\\C-z"))) (is (= 26 (read-string-character "\\C-z")))

  (is (= 67109097 (read-emacs-character "\\C-é")))
  (signals invalid-character-spec-error (read-string-character "\\C-é"))
  (is (= 67109207 (read-emacs-character "\\C-ŗ")))
  (signals invalid-character-spec-error (read-string-character "\\C-ŗ"))
  (is (= 67108991 (read-emacs-character "\\C-\\d")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\d"))
  (is (= 67108877 (read-emacs-character "\\C-\\r")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\r"))
  (is (= 67108896 (read-emacs-character "\\C-\\s"))) (is (= 0 (read-string-character "\\C-\\s")))
  (is (= 67108873 (read-emacs-character "\\C-\\t")))
  (signals invalid-character-spec-error (read-string-character "\\C-\\t"))
  (is (= 3 (read-emacs-character "\\C-c"))) (is (= 3 (read-string-character "\\C-c")))
  (is (= 7 (read-emacs-character "\\C-g"))) (is (= 7 (read-string-character "\\C-g")))
  )

(test test-read-h-modifier
  (is (= 16777216 (read-emacs-character "\\H-\\0")))
  (signals invalid-character-spec-error (read-string-character "\\H-\\0"))
  (signals invalid-character-spec-error (read-string-character "\\H-\\ "))
  )

(test test-read-m-modifier
  (is (= 134217760 (read-emacs-character "\\M- "))) (is (= 160 (read-string-character "\\M- ")))
  (is (= 134217773 (read-emacs-character "\\M--"))) (is (= 173 (read-string-character "\\M--")))
  (is (= 134217776 (read-emacs-character "\\M-0"))) (is (= 176 (read-string-character "\\M-0")))
  (is (= 134217777 (read-emacs-character "\\M-1"))) (is (= 177 (read-string-character "\\M-1")))
  (is (= 134217778 (read-emacs-character "\\M-2"))) (is (= 178 (read-string-character "\\M-2")))
  (is (= 134217779 (read-emacs-character "\\M-3"))) (is (= 179 (read-string-character "\\M-3")))
  (is (= 134217780 (read-emacs-character "\\M-4"))) (is (= 180 (read-string-character "\\M-4")))
  (is (= 134217781 (read-emacs-character "\\M-5"))) (is (= 181 (read-string-character "\\M-5")))
  (is (= 134217782 (read-emacs-character "\\M-6"))) (is (= 182 (read-string-character "\\M-6")))
  (is (= 134217783 (read-emacs-character "\\M-7"))) (is (= 183 (read-string-character "\\M-7")))
  (is (= 134217784 (read-emacs-character "\\M-8"))) (is (= 184 (read-string-character "\\M-8")))
  (is (= 134217785 (read-emacs-character "\\M-9"))) (is (= 185 (read-string-character "\\M-9")))
  (is (= 134217728 (read-emacs-character "\\M-\\0"))) (is (= 128 (read-string-character "\\M-\\0")))
  (is (= 134217728 (read-emacs-character "\\M-\\C-@"))) (is (= 128 (read-string-character "\\M-\\C-@")))
  (is (= 134217728 (read-emacs-character "\\M-\\^@"))) (is (= 128 (read-string-character "\\M-\\^@")))
  (is (= 134217825 (read-emacs-character "\\M-a"))) (is (= 225 (read-string-character "\\M-a")))
  (is (= 134217827 (read-emacs-character "\\M-c"))) (is (= 227 (read-string-character "\\M-c")))
  (is (= 134217842 (read-emacs-character "\\M-r"))) (is (= 242 (read-string-character "\\M-r")))
  (is (= 134217843 (read-emacs-character "\\M-s"))) (is (= 243 (read-string-character "\\M-s")))
  (is (= 134217846 (read-emacs-character "\\M-v"))) (is (= 246 (read-string-character "\\M-v")))
  (is (= 134217848 (read-emacs-character "\\M-x"))) (is (= 248 (read-string-character "\\M-x")))
  (is (= 134217730 (read-emacs-character "\\M-\\002"))) (is (= 130 (read-string-character "\\M-\\002")))
  )

(test test-read-s-modifier
  (is (= 33554432 (read-emacs-character "\\S-\\0")))
  (signals invalid-character-spec-error (read-string-character "\\S-\\0"))
  (is (= 33554432 (read-emacs-character "\\S-\\^@")))
  (signals invalid-character-spec-error (read-string-character "\\S-\\^@"))
  (is (= 33554464 (read-emacs-character "\\S-\\s")))
  (signals invalid-character-spec-error (read-string-character "\\S-\\s"))
  (is (= 33554529 (read-emacs-character "\\S-a")))
  (is (= 33554531 (read-emacs-character "\\S-c")))
  (is (= 33554531 (read-emacs-character "\\S-\\c")))
  (is (= 65 (read-string-character "\\S-a")))
  (is (= 67 (read-string-character "\\S-\\c")))
  (is (= 75 (read-string-character "\\S-\\k")))
  )

(test test-read-super-modifier
  (is (= 8388608 (read-emacs-character "\\s-\\0")))
  (extra-symbols+is (= 32 (read-string-character "\\s-\\0")))
  (is (= 8388707 (read-emacs-character "\\s-c")))
  (is (= -1 (read-emacs-character "\\s-")))
  (extra-symbols+is (= 32 (read-string-character "\\s-")))
  (is (= 8388609 (read-emacs-character "\\s-")))
  (is (= 8388698 (read-emacs-character "\\s-Z")))
  (is (= 8388702 (read-emacs-character "\\s-^")))
  (is (= 8388734 (read-emacs-character "\\s-~")))
  (is (= 8388609 (read-emacs-character "\\s-\\")))
  (is (= 8388655 (read-emacs-character "\\s-\\/")))
  (is (= 8388608 (read-emacs-character "\\s-\\0")))
  (is (= 8388664 (read-emacs-character "\\s-\\8")))
  (is (= -1 (read-emacs-character "\\s-\\^")))
  (is (= 75497504 (read-emacs-character "\\s-\\^ ")))
  (is (= 8388703 (read-emacs-character "\\s-\\_")))
  (is (= 8388615 (read-emacs-character "\\s-\\a")))
  (is (= 8388735 (read-emacs-character "\\s-\\d")))
  (is (= 8388621 (read-emacs-character "\\s-\\r")))
  (is (= 8388640 (read-emacs-character "\\s-\\s")))
  (is (= 8388733 (read-emacs-character "\\s-\\}")))
  (is (= 8388733 (read-emacs-character "\\s-\\}")))
  (is (= 8388608 (read-emacs-character "\\s-\\s-\\0")))
  (extra-symbols+is (= 32 (read-emacs-character "\\s.")))
  )

(test test-read-n-syntax
  (is (= 224 (read-emacs-character "\\N{LATIN SMALL LETTER A WITH GRAVE}")))
  (is (= 224 (read-string-character "\\N{LATIN SMALL LETTER A WITH GRAVE}")))
  (is (= 10052 (read-emacs-character "\\N{SNOWFLAKE}")))
  (is (= 1114111 (read-emacs-character "\\N{U+10ffff}")))
  (is (= 224 (read-emacs-character "\\N{U+E0}")))
  (is (= 224 (read-string-character "\\N{U+E0}")))
  (is (= 65535 (read-emacs-character "\\N{U+ffff}")))
  (is (= 8324 (read-emacs-character "\\N{SUBSCRIPT FOUR}")))
  (is (= 43031 (read-emacs-character "\\N{U+A817}")))
  (is (= 43031  (read-emacs-character "\\N{SYLOTI  NAGRI LETTER   DHO}")))
  (is (= 43031  (read-string-character "\\N{SYLOTI  NAGRI LETTER   DHO}")))
  (is (= 43031 (read-emacs-character "\\N{SYLOTI  NAGRI LETTER \n  DHO}")))
  (is (= 128719 (read-emacs-character "\\N{BED}")))
  (is (= 3053 (read-emacs-character "\\N{U+BED}")))
  (is (= 65024 (read-emacs-character "\\N{VARIATION SELECTOR-1}")))
  (is (= 65039 (read-emacs-character "\\N{VARIATION SELECTOR-16}")))
  (is (= 917760 (read-emacs-character "\\N{VARIATION SELECTOR-17}")))
  (is (= 917999 (read-emacs-character "\\N{VARIATION SELECTOR-256}")))
  (is (= 63744 (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-F900}")))
  (is (= 64217 (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-FAD9}")))
  (is (= 194560 (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-2F800}")))
  (is (= 195101 (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-2FA1D}")))
  (is (= 43031 (read-emacs-character "\\N{U+A817}")))
  (extra-symbols+is (= 43031 (read-string-character "\\N{U+A817}A")))

  (signals invalid-character-spec-error (read-emacs-character "\\N{U+110000}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{DOES NOT EXIST}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{VARIATION SELECTOR-0}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{VARIATION SELECTOR-257}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{VARIATION SELECTOR--0}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-F8FF}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-FADA}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-2F7FF}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{CJK COMPATIBILITY IDEOGRAPH-2FA1E}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{LATIN CAPITAL LETTER Ø}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{}"))
  ;; these 4 non-existing unicode symbols on some platofrms do not cause
  ;; errors in common lisp. So we do not support this test without special reason.
  ;; (signals invalid-character-spec-error (read-emacs-character "\\N{U+D800}"))
  ;; (signals invalid-character-spec-error (read-emacs-character "\\N{U+D801}"))
  ;; (signals invalid-character-spec-error (read-emacs-character "\\N{U+Dffe}"))
  ;; (signals invalid-character-spec-error (read-emacs-character "\\N{U+DFFF}"))

  (signals invalid-character-spec-error (read-emacs-character "\\N{0.5}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+-0}"))
  )

(test test-read-octal ()
  (is (= 0 (read-emacs-character "\\000"))) (is (= 0 (read-string-character "\\000")))
  (extra-symbols+is (= 2 (read-emacs-character "\\002.")))
  (extra-symbols+is (= 2 (read-string-character "\\002.")))
  (is (= 59 (read-emacs-character "\\073"))) (is (= 59 (read-string-character "\\073")))
  (is (= 2 (read-emacs-character "\\02"))) (is (= 2 (read-string-character "\\02")))
  (is (= 137 (read-emacs-character "\\211"))) (is (= 137 (read-string-character "\\211")))
  (is (= 255 (read-emacs-character "\\377"))) (is (= 255 (read-string-character "\\377")))
  (is (= 292 (read-emacs-character "\\444"))) (is (= 292 (read-string-character "\\444")))
  (is (= 438 (read-emacs-character "\\666"))) (is (= 438 (read-string-character "\\666")))
  (is (= 448 (read-emacs-character "\\700"))) (is (= 448 (read-string-character "\\700")))
  (signals invalid-character-spec-error (read-emacs-character "\\805"))
  (extra-symbols+is (= 56 (read-string-character "\\805")))
  (signals invalid-character-spec-error (read-emacs-character "\\508"))
  (extra-symbols+is (= 40 (read-string-character "\\508")))
  )

(test test-read-u-syntax
  (is (= 224 (read-emacs-character "\\U000000E0")))
  (is (= 224 (read-string-character "\\U000000E0")))
  (signals invalid-character-spec-error (read-emacs-character "\\U00000E0"))
  (signals invalid-character-spec-error (read-emacs-character "\\U00220000"))
  (signals invalid-character-spec-error (read-emacs-character "\\U0000000E0"))
  (extra-symbols+is (= 14 (read-string-character "\\U0000000E0")))

  (signals invalid-character-spec-error (read-emacs-character "\\u003"))
  (is (= 0 (read-emacs-character "\\u0000")))
  (is (= 33 (read-emacs-character "\\u0021")))
  (is (= 33 (read-string-character "\\u0021")))
  (is (= 224 (read-emacs-character "\\u00e0")))
  (is (= 8364 (read-emacs-character "\\u20AC")))
  (is (= 64831 (read-emacs-character "\\uFD3F")))

  (extra-symbols+is (= 33 (read-string-character "\\u00217")))
  )

(test test-read-x-syntax
  (is (= 1114112 (read-emacs-character "\\x00110000")))
  (is (= 1114112 (read-string-character "\\x00110000")))
  (is (= 2228224 (read-emacs-character "\\x00220000")))
  (signals invalid-character-spec-error (read-emacs-character "\\x002200000"))
  (signals invalid-character-spec-error (read-string-character "\\x002200000"))
  (is (= 1 (read-emacs-character "\\x1")))
  (is (= 7 (read-emacs-character "\\x7")))
  (is (= 1792 (read-emacs-character "\\x700")))
  (extra-symbols+is (= 1792 (read-string-character "\\x700x")))
  (is (= 128 (read-emacs-character "\\x80")))
  (is (= 174 (read-emacs-character "\\x0aE")))
  (signals invalid-character-spec-error (read-emacs-character "\\xam"))
  (is (= 230 (read-emacs-character "\\xE6")))
  (is (= 15 (read-emacs-character "\\xF")))
  (is (= 160 (read-emacs-character "\\xa0")))
  (is (= 246 (read-emacs-character "\\xf6")))
  (is (= 255 (read-emacs-character "\\xff")))
  (is (= 57343 (read-emacs-character "\\xdfff")))
  )

(test test-read-other-corner-cases
  (signals invalid-character-spec-error (read-emacs-character ""))
  (signals invalid-character-spec-error (read-emacs-character "\")\""))
  (signals invalid-character-spec-error (read-emacs-character "\\@5"))
  (signals invalid-character-spec-error (read-emacs-character "\\da"))
  (signals invalid-character-spec-error (read-emacs-character "aa"))
  (extra-symbols+is (= 97 (read-emacs-character "a?a")))
  (extra-symbols+is (= 97 (read-emacs-character "a'a")))
  )

(defun test-me ()
  (run! 'cl-emacs/lib/character-reader))
