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

(uiop:define-package :cl-emacs/lib/character-reader
    (:use
     :common-lisp
     :cl-emacs/lib/log
     :defstar
     :cl-emacs/lib/elisp-packages
     :cl-emacs/lib/errors
     :cl-emacs/lib/reader-utils
     :cl-emacs/lib/commons)
  (:import-from :serapeum
                #:memq)
  (:export #:read-emacs-character
           #:read-string-character
           )
  (:local-nicknames (:el :cl-emacs/elisp)))
(in-package :cl-emacs/lib/character-reader)
(log-enable :cl-emacs/lib/character-reader :info)
;; (log-enable :cl-emacs/lib/character-reader :debug2)
(named-readtables:in-readtable elisp-function-syntax)

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
                do (unless (simple-digit-char-p char 16)
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
                      ((simple-digit-char-p char 8)
                       (change-mode 'octal)
                       (push (simple-digit-char-p char 8) octals))
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
                      ((simple-digit-char-p char 8)
                       (push (simple-digit-char-p char 8) octals))
                      ;; character is not octal
                      (t
                       (decf position)
                       (return-result (reversed-list-to-number octals 3)))
                      ))
                   (hexadecimal
                    (when (and char
                               (not (char-whitespace-p char))
                               (not (char-end-of-statement-p char)))
                      (if (simple-digit-char-p char 16)
                          (push (simple-digit-char-p char 16) hex)
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
                      (if (simple-digit-char-p char 16)
                          (push (simple-digit-char-p char 16) hex)
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
                      (if (simple-digit-char-p char 16)
                          (push (simple-digit-char-p char 16) hex)
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

