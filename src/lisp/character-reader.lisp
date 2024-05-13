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
(uiop:define-package :cl-emacs/character-reader
    (:use
     :common-lisp
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons)
  (:import-from :common-lisp-user
                #:memq)
  (:export #:read-emacs-character
           #:extra-symbols-in-character-spec-error
           #:invalid-character-spec-error)
  (:local-nicknames (:el :cl-emacs/elisp)))
(in-package :cl-emacs/character-reader)
(log-enable :cl-emacs/character-reader :debug1)
(def-suite cl-emacs/character-reader)
(in-suite cl-emacs/character-reader)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defclass character-reader-error (reader-error)
  ((input :initarg :input
          :initform nil
          :type string)
   (details :initarg :details
            :initform ""
            :type string)))
(defmethod print-object ((e character-reader-error) stream)
  (with-slots (input details) e
    (format stream "#<~a details:~s, input:~s>"
            (class-name (class-of e)) details input))
  )
(defclass invalid-character-spec-error (character-reader-error)
  ())
(defclass extra-symbols-in-character-spec-error (character-reader-error)
  ((position :initarg :position
             :initform -1
             :type fixnum)
   (parsed-code :initarg :parsed-code
                :initform 0
                :type fixnum)))
(defmethod initialize-instance :after ((e extra-symbols-in-character-spec-error) &key position input parsed-code)
  (with-slots (details) e
    (let ((rem (- (length input) position)))
      (setq details (format nil "~a last character~p not parsed. Current result ~a"
                            rem rem parsed-code)))))

(defun decode-named-char (raw-input name)
  (declare (string name))
  (let* ((clean-name (str:replace-all "\n" "" name))
         decoded 
         )
    (if (str:starts-with-p "U+" clean-name)
        (let ((hex-part (str:upcase (str:substring 2 t clean-name))))
          (loop for char across hex-part
                do (when (not (or (char<= #\0 char #\9)
                                  (char<= #\A char #\F)))
                     (error 'invalid-character-spec-error
                            :input raw-input
                            :details (format nil "invalid character in hex notation ~a" char))))
          (let ((parsed (parse-integer hex-part :radix 16)))
            (when (and (< parsed cl-unicode:+code-point-limit+) (code-char parsed))
              (setq decoded parsed))))
        (let ((parsed (cl-unicode:character-named clean-name)))
          (when parsed
            (setq decoded (char-code parsed)))))
    
    (or decoded
        (error 'invalid-character-spec-error
               :input raw-input
               :details (format nil "can't recognize unicode name ~a" name)))))

(defun octals-to-code (octals)
  (declare (list octals))
  (loop for oct in octals
        for shift from 0 by 3
        sum (ash oct shift)))
(defun hex-to-code (hex)
  (declare (list hex))
  (loop for h in hex
        for shift from 0 by 4
        sum (ash h shift)))

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

(defun read-emacs-character (input)
  #M"Read emacs character notation with all it's weird exceptions. 
     Initial ? sign should be omitted
     Return value: character code, because emacs has no special type for character"
  (block parsing
    (let ((mode 'toplevel)
          (n-chars (length input))
          (position 0)
          (modifiers 0)
          (caret 0)
          (control 0)
          (super 0)
          (named-list)
          (octals) (hex))
      (labels ((change-mode (new-mode)
                 (setq mode new-mode))
               (return-result (result)
                 (loop for _ below (+ caret control)
                       do (cond
                            ((= result 63) (incf result 64))
                            ((< 96 result 123) (decf result 96))
                            ((<= 64 result 95) (decf result 64))
                            (t
                             (setq result (logior result #x4000000)))))
                 (loop for _ below (+ super)
                       do (setq result (logior result #x800000)))
                 (setq result (logior result modifiers))
                 (unless (= position n-chars)
                   (error 'extra-symbols-in-character-spec-error
                          :input input
                          :position position
                          :parsed-code result))
                 (return-from parsing result))

               (process-one-character (char)
                 (ecase mode
                   (toplevel
                    (cond
                      ((null char)
                       (cond
                         ((or (> caret 0) (> super 0))
                          (setf modifiers 0)
                          (setf caret 0)
                          (setf super 0)
                          (return-result -1))
                         (t (error 'invalid-character-spec-error
                                   :input input
                                   :details "character specification is empty"))))
                      ((memq char '(#\" #\( #\) #\[ #\]))
                       (error 'invalid-character-spec-error
                              :input input
                              :details (format nil "invalid symbol found ~a" char)))
                      ((eq char #\\) (change-mode 'special))
                      (t (return-result (char-code char)))))
                   (special
                    (cond 
                      ((char<= #\0 char #\7)
                       (change-mode 'octal)
                       (push (- (char-code char) (char-code #\0)) octals))
                      ((eq char #\A) (setq modifiers (logior modifiers #x400000)) (change-mode 'modifier))
                      ((eq char #\C) (incf control) (change-mode 'modifier))
                      ((eq char #\H) (setq modifiers (logior modifiers #x1000000)) (change-mode 'modifier))
                      ((eq char #\M) (setq modifiers (logior modifiers #x8000000)) (change-mode 'modifier))
                      ((eq char #\s) (incf super) (change-mode 'super-modifier))
                      ((eq char #\S) (setq modifiers (logior modifiers #x2000000)) (change-mode 'modifier))
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
                      ((memq char '(#\a #\b #\f))
                       (return-result (- (char-code char) 90)))
                      (t (return-result (char-code char)))))
                   (octal
                    (cond
                      ((null char)
                       (return-result (octals-to-code octals)))
                      ((char<= #\0 char #\7)
                       (push (- (char-code char) (char-code #\0)) octals))
                      (t (error 'invalid-character-spec-error
                                :input input
                                :details (format nil "bad symbol in octal mode ~a" char)))))
                   ((hexadecimal 4-unicode 8-unicode)
                    (cond
                      ((null char)
                       (case mode
                         (4-unicode (unless (= 4 (length hex))
                                      (error 'invalid-character-spec-error
                                             :input input
                                             :details "spec should should contain exactly 4 hexadecimal symbols")))
                         (8-unicode
                          (unless (= 8 (length hex))
                            (error 'invalid-character-spec-error
                                   :input input
                                   :details "spec should should contain exactly 8 hexadecimal symbols"))
                          (when (> (hex-to-code hex) cl-unicode:+code-point-limit+)
                            (error 'invalid-character-spec-error
                                   :input input
                                   :details "character not in unicode range"))))
                       (return-result (hex-to-code hex)))
                      ((char<= #\0 char #\9)
                       (push (- (char-code char) (char-code #\0)) hex))
                      ((char<= #\a char #\f)
                       (push (+ 10 (- (char-code char) (char-code #\a))) hex))
                      ((char<= #\A char #\F)
                       (push (+ 10 (- (char-code char) (char-code #\A))) hex))
                      (t (error 'invalid-character-spec-error
                                :input input
                                :details (format nil "bad symbol in hexadecimal mode ~a" char)))))
                   (modifier
                    (cond
                      ((null char)
                       (error 'invalid-character-spec-error
                              :input input
                              :details "not found symbol - after the modifier"))
                      ((eq char #\-)
                       (change-mode 'toplevel))
                      (t (error 'invalid-character-spec-error
                                :input input
                                :details (format nil "bad symbol after the modifier ~a" char)))))
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
                              :input input
                              :details "curly brace should open after N"))
                      ((eq char #\{)
                       (change-mode 'named-in-braces))
                      (t (error 'invalid-character-spec-error
                                :input input
                                :details (format nil "bad symbol after N ~a" char)))))
                   (named-in-braces
                    (cond
                      ((null char)
                       (error 'invalid-character-spec-error
                              :input input
                              :details "no closing curly brace"))
                      ((eq char #\})
                       (return-result (decode-named-char input (char-list-to-string (nreverse named-list)))))
                      (t (push char named-list))))
                   )))
        (loop for char across input
              do (incf position)
                 (process-one-character char)
              finally (progn
                        (process-one-character nil)
                        (error 'invalid-character-spec-error
                               :input input
                               :details "unexpected end of character specification")))))))



(test read-single-character
  (is (= 1 (read-emacs-character "")))
  (is (= 26 (read-emacs-character "")))
  (is (= 27 (read-emacs-character "")))
  (is (= 28 (read-emacs-character "")))
  (is (= 29 (read-emacs-character "")))
  (is (= 30 (read-emacs-character "")))
  (is (= 31 (read-emacs-character "")))
  (is (= 32 (read-emacs-character " ")))
  (is (= 33 (read-emacs-character "!")))
  (is (= 35 (read-emacs-character "#")))
  (is (= 36 (read-emacs-character "$")))
  (is (= 37 (read-emacs-character "%")))
  (is (= 38 (read-emacs-character "&")))
  (is (= 39 (read-emacs-character "'")))
  (signals invalid-character-spec-error (read-emacs-character "("))
  (signals invalid-character-spec-error (read-emacs-character "()"))
  (signals invalid-character-spec-error (read-emacs-character ")"))
  (is (= 42 (read-emacs-character "*")))
  (is (= 43 (read-emacs-character "+")))
  (is (= 44 (read-emacs-character ",")))
  (is (= 45 (read-emacs-character "-")))
  (is (= 46 (read-emacs-character ".")))
  (is (= 47 (read-emacs-character "/")))
  (is (= 48 (read-emacs-character "0")))
  (is (= 49 (read-emacs-character "1")))
  (is (= 50 (read-emacs-character "2")))
  (is (= 51 (read-emacs-character "3")))
  (is (= 52 (read-emacs-character "4")))
  (is (= 53 (read-emacs-character "5")))
  (is (= 54 (read-emacs-character "6")))
  (is (= 55 (read-emacs-character "7")))
  (is (= 56 (read-emacs-character "8")))
  (is (= 57 (read-emacs-character "9")))
  (is (= 58 (read-emacs-character ":")))
  (is (= 59 (read-emacs-character ";")))
  (is (= 60 (read-emacs-character "<")))
  (is (= 61 (read-emacs-character "=")))
  (is (= 62 (read-emacs-character ">")))
  (is (= 63 (read-emacs-character "?")))
  (is (= 64 (read-emacs-character "@")))
  (is (= 65 (read-emacs-character "A")))
  (is (= 90 (read-emacs-character "Z")))
  (signals invalid-character-spec-error (read-emacs-character "["))
  (signals invalid-character-spec-error (read-emacs-character "]"))
  (is (= 94 (read-emacs-character "^")))
  (is (= 121 (read-emacs-character "y")))
  (is (= 123 (read-emacs-character "{")))
  (is (= 124 (read-emacs-character "|")))
  (is (= 125 (read-emacs-character "}")))
  (is (= 126 (read-emacs-character "~")))
  (is (= 127 (read-emacs-character "")))
  (is (= 160 (read-emacs-character " ")))
  (is (= 169 (read-emacs-character "©")))
  (is (= 1044 (read-emacs-character "Д")))
  (is (= 1069 (read-emacs-character "Э")))
  (is (= 9785 (read-emacs-character "☹")))
  (is (= 128169 (read-emacs-character "💩")))
  (is (= 128512 (read-emacs-character "😀"))))

(test read-one-special-character
  (is (= 1 (read-emacs-character "\\")))
  (is (= 33 (read-emacs-character "\\!")))
  (is (= 47 (read-emacs-character "\\/")))
  (is (= 0 (read-emacs-character "\\0")))
  (is (= 1 (read-emacs-character "\\1")))
  (is (= 3 (read-emacs-character "\\3")))
  (is (= 4 (read-emacs-character "\\4")))
  (is (= 5 (read-emacs-character "\\5")))
  (is (= 7 (read-emacs-character "\\7")))
  (is (= 56 (read-emacs-character "\\8")))
  (is (= 57 (read-emacs-character "\\9")))
  (is (= 64 (read-emacs-character "\\@")))
  (signals invalid-character-spec-error (read-emacs-character "\\A"))
  (signals extra-symbols-in-character-spec-error (read-emacs-character "\\\\A"))
  (signals invalid-character-spec-error (read-emacs-character "\\C"))
  (signals invalid-character-spec-error (read-emacs-character "\\H"))
  (signals invalid-character-spec-error (read-emacs-character "\\M"))
  (signals invalid-character-spec-error (read-emacs-character "\\N"))
  (signals invalid-character-spec-error (read-emacs-character "\\S"))
  (signals invalid-character-spec-error (read-emacs-character "\\U"))
  (is (= 90 (read-emacs-character "\\Z")))
  (is (= 93 (read-emacs-character "\\]")))
  (is (= -1 (read-emacs-character "\\^")))
  (is (= 67108896 (read-emacs-character "\\^ ")))
  (is (= 95 (read-emacs-character "\\_")))
  (is (= 96 (read-emacs-character "\\`")))

  (is (= 7 (read-emacs-character "\\a")))
  (is (= 8 (read-emacs-character "\\b")))
  (is (= 99 (read-emacs-character "\\c")))
  (is (= 127 (read-emacs-character "\\d")))
  (is (= 27 (read-emacs-character "\\e")))
  (is (= 12 (read-emacs-character "\\f")))
  (is (= 103 (read-emacs-character "\\g")))
  (is (= 104 (read-emacs-character "\\h")))
  (is (= 105 (read-emacs-character "\\i")))
  (is (= 106 (read-emacs-character "\\j")))
  (is (= 107 (read-emacs-character "\\k")))
  (is (= 108 (read-emacs-character "\\l")))
  (is (= 109 (read-emacs-character "\\m")))
  (is (= 10 (read-emacs-character "\\n")))
  (is (= 111 (read-emacs-character "\\o")))
  (is (= 112 (read-emacs-character "\\p")))
  (is (= 113 (read-emacs-character "\\q")))
  (is (= 13 (read-emacs-character "\\r")))
  (is (= 32 (read-emacs-character "\\s")))
  (is (= 9 (read-emacs-character "\\t")))
  (signals invalid-character-spec-error (read-emacs-character "\\u"))
  (is (= 11 (read-emacs-character "\\v")))
  (is (= 119 (read-emacs-character "\\w")))
  (is (= 0 (read-emacs-character "\\x")))
  (is (= 121 (read-emacs-character "\\y")))
  (is (= 122 (read-emacs-character "\\z")))
  (is (= 123 (read-emacs-character "\\{")))
  (is (= 124 (read-emacs-character "\\|")))
  (is (= 125 (read-emacs-character "\\}")))
  (is (= 126 (read-emacs-character "\\~")))
  (is (= 127 (read-emacs-character "\\")))
  (is (= 160 (read-emacs-character "\\ ")))
  (is (= 1044 (read-emacs-character "\\Д")))
  ) 

(test read-octal ()
  (is (= 0 (read-emacs-character "\\000")))
  (signals invalid-character-spec-error (read-emacs-character "\\002."))
  (is (= 59 (read-emacs-character "\\073")))
  (is (= 2 (read-emacs-character "\\02")))
  (is (= 137 (read-emacs-character "\\211")))
  (is (= 255 (read-emacs-character "\\377")))
  (is (= 292 (read-emacs-character "\\444")))
  (is (= 438 (read-emacs-character "\\666")))
  (is (= 448 (read-emacs-character "\\700")))
  (signals extra-symbols-in-character-spec-error (read-emacs-character "\\850"))
  (signals invalid-character-spec-error (read-emacs-character "\\508"))
  )

(test read-modifiers-general
  (signals invalid-character-spec-error (read-emacs-character "\\A"))
  (signals invalid-character-spec-error (read-emacs-character "\\A-"))
  (signals invalid-character-spec-error (read-emacs-character "\\AA"))
  (signals invalid-character-spec-error (read-emacs-character "\\C"))
  (signals invalid-character-spec-error (read-emacs-character "\\C-"))
  (signals invalid-character-spec-error (read-emacs-character "\\CC"))
  (signals extra-symbols-in-character-spec-error (read-emacs-character "\\$-a"))
  (signals invalid-character-spec-error (read-emacs-character "\\C"))
  (is (= 37748843 (read-emacs-character "\\A-\\S-k")))
  (is (= 71303216 (read-emacs-character "\\^\\A-0")))
  (is (= 71303168 (read-emacs-character "\\^\\A-\\0")))
  (is (= 67108865 (read-emacs-character "\\C-\\C-a")))
  (is (= 67108877 (read-emacs-character "\\C-\\C-m")))
  (is (= 16777240 (read-emacs-character "\\C-\\H-x")))
  (is (= 201326625 (read-emacs-character "\\C-\\M-!")))
  (is (= 201326649 (read-emacs-character "\\C-\\M-9")))
  (is (= 201326654 (read-emacs-character "\\C-\\M->")))
  (is (= 201326626 (read-emacs-character "\\C-\\M-\\\"")))
  (is (= 201326631 (read-emacs-character "\\C-\\M-\\'")))
  (is (= 201326651 (read-emacs-character "\\C-\\M-\\;")))
  (is (= 167772182 (read-emacs-character "\\C-\\M-\\S-v")))
  (is (= 134217756 (read-emacs-character "\\C-\\M-\\\\")))
  (is (= 201326624 (read-emacs-character "\\C-\\M-\\s")))
  (is (= 134217759 (read-emacs-character "\\C-\\M-_")))
  (is (= 134217729 (read-emacs-character "\\C-\\M-a")))
  (is (= 134217750 (read-emacs-character "\\C-\\M-v")))
  (is (= 201326825 (read-emacs-character "\\C-\\M-é")))
  (is (= 33554447 (read-emacs-character "\\C-\\S-O")))
  (is (= 33554433 (read-emacs-character "\\C-\\S-a")))
  (is (= 33554454 (read-emacs-character "\\C-\\S-v")))
  (is (= 75497504 (read-emacs-character "\\C-\\s- ")))
  (is (= -1 (read-emacs-character "\\C-\\s-")))
  (is (= 201326624 (read-emacs-character "\\M-\\C-\\ ")))
  (is (= 134217730 (read-emacs-character "\\M-\\C-b")))
  (is (= -1 (read-emacs-character "\\A-\\^")))
  )

(test read-caret-modifier
  (is (= 67108896 (read-emacs-character "\\^ ")))
  (is (= 67108897 (read-emacs-character "\\^!")))
  (signals invalid-character-spec-error (read-emacs-character "\\^\""))
  (is (= 67108926 (read-emacs-character "\\^>")))
  (is (= 127 (read-emacs-character "\\^?")))
  (is (= 0 (read-emacs-character "\\^@")))
  (is (= 1 (read-emacs-character "\\^A")))
  (is (= 26 (read-emacs-character "\\^Z")))
  (is (= 29 (read-emacs-character "\\^\\]")))
  (is (= 31 (read-emacs-character "\\^_")))
  (is (= 67108960 (read-emacs-character "\\^`")))
  (is (= 1 (read-emacs-character "\\^a")))
  (is (= 26 (read-emacs-character "\\^z")))
  (is (= 67108987 (read-emacs-character "\\^{")))
  (is (= 67108865 (read-emacs-character "\\^\\^a")))
  )

(test read-a-modifier
  (is (= 4194304 (read-emacs-character "\\A-\\0")))
  (is (= 4194304 (read-emacs-character "\\A-\\^@")))
  (is (= 4194403 (read-emacs-character "\\A-c")))
  )

(test read-c-modifier
  (is (= 67108896 (read-emacs-character "\\C-\\ ")))
  (is (= 67108898 (read-emacs-character "\\C-\\\"")))
  (is (= 67108864 (read-emacs-character "\\C-\\0")))
  (is (= 67108911 (read-emacs-character "\\C-/")))
  (is (= 67108925 (read-emacs-character "\\C-=")))
  (is (= 127 (read-emacs-character "\\C-?")))
  (is (= 0 (read-emacs-character "\\C-@")))
  (is (= 19 (read-emacs-character "\\C-S")))
  (is (= 67108864 (read-emacs-character "\\C-\\0")))
  (is (= 201326825 (read-emacs-character "\\C-\\M-é")))
  (is (= 27 (read-emacs-character "\\C-\\[")))
  (is (= 28 (read-emacs-character "\\C-\\\\")))
  (is (= 67108991 (read-emacs-character "\\C-\\d")))
  (is (= 30 (read-emacs-character "\\C-^")))
  (is (= 1 (read-emacs-character "\\C-a")))
  (is (= 26 (read-emacs-character "\\C-z")))
  (is (= 67109097 (read-emacs-character "\\C-é")))
  (is (= 67109207 (read-emacs-character "\\C-ŗ")))
  (is (= 67108991 (read-emacs-character "\\C-\\d")))
  (is (= 67108877 (read-emacs-character "\\C-\\r")))
  (is (= 67108896 (read-emacs-character "\\C-\\s")))
  (is (= 67108873 (read-emacs-character "\\C-\\t")))
  (is (= 3 (read-emacs-character "\\C-c")))
  (is (= 7 (read-emacs-character "\\C-g")))
  )

(test read-h-modifier
  (is (= 16777216 (read-emacs-character "\\H-\\0")))
  )

(test read-m-modifier
  (is (= 134217760 (read-emacs-character "\\M- ")))
  (is (= 134217773 (read-emacs-character "\\M--")))
  (is (= 134217776 (read-emacs-character "\\M-0")))
  (is (= 134217777 (read-emacs-character "\\M-1")))
  (is (= 134217778 (read-emacs-character "\\M-2")))
  (is (= 134217779 (read-emacs-character "\\M-3")))
  (is (= 134217780 (read-emacs-character "\\M-4")))
  (is (= 134217781 (read-emacs-character "\\M-5")))
  (is (= 134217782 (read-emacs-character "\\M-6")))
  (is (= 134217783 (read-emacs-character "\\M-7")))
  (is (= 134217784 (read-emacs-character "\\M-8")))
  (is (= 134217785 (read-emacs-character "\\M-9")))
  (is (= 134217728 (read-emacs-character "\\M-\\0")))
  (is (= 134217728 (read-emacs-character "\\M-\\C-@")))
  (is (= 134217728 (read-emacs-character "\\M-\\^@")))
  (is (= 134217825 (read-emacs-character "\\M-a")))
  (is (= 134217827 (read-emacs-character "\\M-c")))
  (is (= 134217842 (read-emacs-character "\\M-r")))
  (is (= 134217843 (read-emacs-character "\\M-s")))
  (is (= 134217846 (read-emacs-character "\\M-v")))
  (is (= 134217848 (read-emacs-character "\\M-x")))
  (is (= 134217728 (read-emacs-character "\\M-\\0")))
  (is (= 134217730 (read-emacs-character "\\M-\\002")))
  )

(test read-s-modifier
  (is (= 33554432 (read-emacs-character "\\S-\\0")))
  (is (= 33554432 (read-emacs-character "\\S-\\^@")))
  (is (= 33554464 (read-emacs-character "\\S-\\s")))
  (is (= 33554529 (read-emacs-character "\\S-a")))
  (is (= 33554531 (read-emacs-character "\\S-c")))
  (is (= 33554432 (read-emacs-character "\\S-\\0")))
  )

(test read-super-modifier
  (is (= 8388608 (read-emacs-character "\\s-\\0")))
  (is (= 8388707 (read-emacs-character "\\s-c")))
  (is (= -1 (read-emacs-character "\\s-")))
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
  (signals extra-symbols-in-character-spec-error (read-emacs-character "\\s."))
  (is (= 32 (handler-case
                (progn
                  (read-emacs-character "\\s.")
                  nil)
              (extra-symbols-in-character-spec-error (e)
                (with-slots (parsed-code) e
                  parsed-code)))))
  )

(test read-n-syntax
  (is (= 224 (read-emacs-character "\\N{LATIN SMALL LETTER A WITH GRAVE}"))) 
  (is (= 10052 (read-emacs-character "\\N{SNOWFLAKE}")))
  (is (= 1114111 (read-emacs-character "\\N{U+10ffff}")))
  (is (= 224 (read-emacs-character "\\N{U+E0}")))
  (is (= 65535 (read-emacs-character "\\N{U+ffff}")))
  (is (= 8324 (read-emacs-character "\\N{SUBSCRIPT FOUR}")))
  (is (= 43031 (read-emacs-character "\\N{U+A817}")))
  (is (= 43031  (read-emacs-character "\\N{SYLOTI  NAGRI LETTER   DHO}")))
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
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+D800}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+D801}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+Dffe}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+DFFF}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{0.5}"))
  (signals invalid-character-spec-error (read-emacs-character "\\N{U+-0}"))
  )

(test read-u-syntax
  (is (= 224 (read-emacs-character "\\U000000E0")))
  (signals invalid-character-spec-error (read-emacs-character "\\U00000E0"))
  (signals invalid-character-spec-error (read-emacs-character "\\U00220000"))

  (signals invalid-character-spec-error (read-emacs-character "\\u003"))
  (is (= 0 (read-emacs-character "\\u0000")))
  (is (= 33 (read-emacs-character "\\u0021")))
  (is (= 224 (read-emacs-character "\\u00e0")))
  (is (= 8364 (read-emacs-character "\\u20AC")))
  (is (= 64831 (read-emacs-character "\\uFD3F")))
  )

(test read-x-syntax
  (is (= 1114112 (read-emacs-character "\\x00110000")))
  (is (= 2228224 (read-emacs-character "\\x00220000")))
  (is (= 1 (read-emacs-character "\\x1")))
  (is (= 7 (read-emacs-character "\\x7")))
  (is (= 1792 (read-emacs-character "\\x700")))
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

(test read-other-corner-cases
  (signals invalid-character-spec-error (read-emacs-character ""))
  (signals invalid-character-spec-error (read-emacs-character "\")\""))
  )
(defun test-me ()
  (run! 'cl-emacs/character-reader))

