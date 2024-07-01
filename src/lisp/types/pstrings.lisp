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

(uiop::define-package :cl-emacs/types/pstrings
    (:use
     :defstar
     :common-lisp
     :cl-emacs/lib/log
     :fiveam
     :snakes
     :cl-emacs/lib/commons)
  (:import-from #:serapeum
                #:memq)
  (:export #:build-pstring
           #:compute-hash
           #:copy-pstring
           #:do-char
           #:emptyp
           #:generate-chars
           #:ninsert
           #:pstring
           #:pstring-p
           #:pstring-char=
           #:pstring=
           #:pstring-len
           #:set-properties
           #:write-pstring-to-cl-stream
           )
  )
(in-package :cl-emacs/types/pstrings)
(log-enable :cl-emacs/types/pstrings :info)
;; (log-enable :cl-emacs/types/pstrings :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/types/pstrings)
(in-suite cl-emacs/types/pstrings)

;;; smart self-balancing b-tree structure, containing string chunks and properties

(define-condition invalid-pstring-operation (error-with-description)
  ())

(defstruct (interval (:copier nil))
  (start 0 :type fixnum)
  (chunk "" :type string)
  (len 0 :type fixnum)
  ;; we use alist internally to sort properties by key easily
  ;; that should allow to compare alists by equalp
  (alist nil :type list)
  (prev nil :type (or null interval))
  (next nil :type (or null interval))
  )

(defun* (copy-interval -> interval) ((interval interval))
  #M"make copy of interval structure, destroing prev/next references"
  (with-slots (start chunk len alist prev next) interval
    (make-interval
     :start start
     :chunk (copy-seq chunk)
     :len len
     :alist (copy-alist alist)
     :prev nil
     :next nil)))


;; intervals are initialized with size <= +normal-interval-length+,
;; they can grow up to (* 2 +normal-interval-length+), then they will
;; break into two separate intervals
(defparameter *normal-interval-length* 1000)
(defparameter *max-interval-length* 2000)

(defun* (nsorted-alist -> list) ((alist list))
  (sort alist #'(lambda (c1 c2)
                  (let ((key1 (car c1))
                        (key2 (car c2)))
                    (declare (symbol key1 key2))
                    (string< (symbol-name key1) (symbol-name key2))))))

(defstruct (pstring (:print-function print-pstring)
                    (:copier nil))
  (tree nil :type trees:binary-tree)
  (len 0 :type fixnum)
  (first nil :type (or interval null)))

;; (defmethod make-load-form ((tree trees:binary-tree) &optional environment)
;;   (make-load-form-saving-slots tree :environment environment))

;; (defmethod make-load-form ((node trees::avl-tree-node) &optional environment)
;;   (make-load-form-saving-slots node :environment environment))


;; (defmethod make-load-form ((pstr pstring) &optional environment)
;;   (make-load-form-saving-slots pstr :environment environment))

;; (defmethod make-load-form ((interval interval) &optional environment)
;;   (make-load-form-saving-slots interval :environment environment))


(defgenerator generate-intervals (pstring-or-interval)
  #M"if you specify pstring, iteration will start from the first interval
     if you specify interval, it will be the first"
  (loop
    with iterator = (if (pstring-p pstring-or-interval)
                        (pstring-first pstring-or-interval)
                        pstring-or-interval)
    while iterator
    do (yield iterator)
       (setq iterator (interval-next iterator))))

(defgenerator generate-chars (pstr)
  (do-generator (interval (generate-intervals pstr))
    (loop for char across (interval-chunk interval)
          do (yield char))))

(defgenerator generate-property-intervals (pstr)
  (let (start end alist)
    (labels ((write-known-interval ()
               (when alist
                 (yield start end alist))
               (setq alist nil)
               (setq start nil)
               (setq end nil)))
      (do-generator (iterator (generate-intervals pstr))
        ;; (log-debug2 "printing interval ~s" ,iterator)
        (if (and start (equalp alist (interval-alist iterator)))
            (incf end (interval-len iterator))
            (progn
              (when start
                (write-known-interval))
              (setq alist (interval-alist iterator))
              (setq start (interval-start iterator))
              (setq end (+ (interval-start iterator) (interval-len iterator))))))
      (write-known-interval))))

(defun* print-pstring ((pstr pstring) (stream stream) depth)
  (declare (ignore depth))
  (write-pstring-to-cl-stream pstr stream :escaped :string)
  )

(defun* write-pstring-to-cl-stream ((pstr pstring) stream &key escaped)
  #M"escaped can be:
     nil - no escape processing: raw string
     :string - string escape mode: \"string mode\"
     :symbol - symbol escape mode: special\ symbol

     in :string mode:
     human interface operation, not designed to be uberfast,
     requires triple string scan to output in emacs-compatible form
     also it tries to combine alists to convert real internal smaller
     intervals to bigger ones with unique property alists in output
     "
  (let ((full-pstring-form nil))
    (when (eq escaped :string)
      (block look-for-properties
        (do-generator (iterator (generate-intervals pstr))
          (when (interval-alist iterator)
            (setq full-pstring-form t)
            (return-from look-for-properties))))
      (when full-pstring-form
        (write-sequence "#(" stream))
      (write-string "\"" stream))
    (do-generator (char (generate-chars pstr))
      (when  (or (and (eq escaped :symbol) (or (cl:char<= char #\space)
                                               (char-end-of-statement-p char)))
                 (and (eq escaped :string) (memq char '(#\\ #\"))))
        
        (write-char #\\ stream))
      (write-char char stream))
    (when (eq escaped :string)
      (write-char #\" stream)
      (when full-pstring-form
        ;; circular deprendency trick
        (let* ((pkg (find-package "CL-EMACS/LIB/PRINTER"))
               (print-func (find-symbol "PRINT-TO-CL-STREAM" pkg)))
          (do-generator (start end alist (generate-property-intervals pstr))
            (write-char #\space stream)
            (funcall print-func start stream nil)
            (write-char #\space stream)
            (funcall print-func end stream nil)
            (write-sequence " (" stream)
            (let ((first t))
              (dolist (cons alist)
                (unless first
                  (write-char #\space stream))
                (funcall print-func (car cons) stream nil)
                (write-char #\space stream)
                (funcall print-func (cdr cons) stream nil)
                (setq first nil)))
            (write-char #\) stream)
            ))
        (write-char #\) stream)))))

;; (defun test-tree ()
;;   (let ((tree (trees:make-binary-tree :avl #'< :key #'prop-interval-start)))
;;     (loop for start from 0 to 20 by 4
;;           do (trees:insert (make-prop-interval :start start :len 3) tree))
;;     (trees:pprint-tree tree))
;;   )

(defun* (make-empty-pstring -> pstring) ()
  (make-pstring :tree (trees:make-binary-tree :avl #'< :key #'interval-start))
  )

(defun* (emptyp -> boolean) ((pstr pstring))
  (zerop (pstring-len pstr)))

(defun* (find-interval-for-modification -> interval) ((pstr pstring) (position fixnum) &optional tree-node)
  #M"tree-node should be root node in the beginning
     function will return interval with (>= start position) and (< position end) "
  (log-debug2 "find-interval-for-modification position:~s tree-node:~s" position tree-node)
  (with-slots (tree) pstr
    (unless tree-node
      (error 'invalid-pstring-operation :details
             (format nil "can't find tree-node with position ~a" position)))
    (let ((interval (trees::datum tree-node)))
      (with-slots (start len) interval
        (let ((end (+ start len)))
          (cond
            ((< position start)
             (find-interval-for-modification pstr position (trees::left tree-node)))
            ((>= position end)
             (find-interval-for-modification pstr position (trees::right tree-node)))
            (t interval)
            ))))))

(defun* (split-interval -> interval) ((pstr pstring) (old-interval interval) (rel-cut-position fixnum))
  #M"returns new interval"
  (let* ((cut-position (+ (interval-start old-interval)
                          rel-cut-position))
         (new-interval (make-interval
                        :start cut-position
                        :chunk (str:substring rel-cut-position t (interval-chunk old-interval))
                        :alist (copy-alist (interval-alist old-interval))
                        :len (- (interval-len old-interval) rel-cut-position)
                        :prev old-interval
                        :next (interval-next old-interval))))
    (with-slots (next chunk len) old-interval
      (setq next new-interval)
      (setq chunk (str:substring 0 rel-cut-position chunk))
      (decf len (interval-len new-interval)))
    (trees:insert new-interval (pstring-tree pstr))
    new-interval))

(defun* insert-interval ((pstr pstring) (new-interval interval) (position fixnum))
  #M"insert interval into pstring at the required position"
  (log-debug2 "insert interval pstr:~s interval:~s position:~s" pstr new-interval position)
  (with-slots (len first tree) pstr
    (assert (<= 0 position len))
    (when (emptyp pstr)

      ;; interval modification
      (with-slots (start prev next chunk) new-interval
        (setq prev nil)
        (setq next nil)
        (setq start len))
      ;; append interval to string
      (incf len (interval-len new-interval))

      (unless first
        (setq first new-interval))
      (trees:insert new-interval tree)
      (return-from insert-interval))
    )
  ;; string is not empty
  (let* ((pstr-len (pstring-len pstr))
         (old-interval (if (= position pstr-len)
                           (find-interval-for-modification pstr (1- pstr-len) (trees::root (pstring-tree pstr)))
                           (find-interval-for-modification pstr position (trees::root (pstring-tree pstr)))))
         (delta-len (interval-len new-interval))
         changed-interval)
    (cond
      ;; intervals are compatible by alist
      ((equalp (interval-alist new-interval)
               (interval-alist old-interval))
       (log-debug2 "intervals are compatible")
       ;; adjust current interval
       (let ((rel-pos (- position (interval-start old-interval)))
             (old-chunk (interval-chunk old-interval))
             (new-chunk (interval-chunk new-interval))
             )
         (setf (interval-chunk old-interval)
               (concatenate 'string
                            (str:substring 0 rel-pos old-chunk)
                            new-chunk
                            (str:substring rel-pos t old-chunk)))
         (incf (interval-len old-interval) delta-len))
       (setq changed-interval old-interval))
      (t
       (log-debug2 "intervals are incompatible")
       ;; adjust position information for new interval
       (with-slots (start prev next) new-interval
         (setq start (+ (interval-start old-interval)
                        (interval-len old-interval)))
         (setq prev old-interval)
         (setq next (interval-next old-interval)))
       (setf (interval-next old-interval) new-interval)
       (setq changed-interval new-interval)))

    ;; move tree positions forward
    (do-generator (iterator (generate-intervals (interval-next changed-interval)))
      (incf (interval-start iterator) delta-len))
    ;; resize string
    (incf (pstring-len pstr) delta-len)
    ;; break long intervals
    (loop while (> (interval-len old-interval) *max-interval-length*)
          do (let* ((rel-cut-position (- (interval-len old-interval)
                                         *normal-interval-length*)))
               (split-interval pstr old-interval rel-cut-position)))))

(defun* ninsert ((what pstring) (where pstring) &optional position)
  #M"Insert string what into where at the specific position
     If position is NIL, works as concatenation
     Result is stored in where"
  (unless (emptyp what)
    (with-slots (tree) what
      (let ((insert-position (or position (pstring-len where))))
        (trees:dotree (interval2 tree)
          (insert-interval where (copy-interval interval2) insert-position)
          (incf insert-position (interval-len interval2)))))))

(defun* (get-alist-at-position -> list) ((pstr pstring) position)
  (let* ((interval (find-interval-for-modification pstr position (trees::root (pstring-tree pstr)))))
    (interval-alist interval)))

;; (defun* (set-properties-at-position -> list) ((pstr pstring) position)
;;   )

(defun* set-properties ((pstr pstring) (start fixnum) (end fixnum) (alist list) &optional overwrite chosen-interval)
  #M"if overwrite = t, all properties will be replaced on this region, otherwise it will be merge
     alist will be sorted

     if first argument is interval, assume that it is internal-call with correct interval selected"
  (assert (<= start end))
  (when (and (emptyp pstr) (or (> start 0) (> end 0)))
    (error 'invalid-pstring-operation :details
           "can't set properties on empty string"))
  (when (= start end)
    (return-from set-properties))
  (log-debug2 "set-properties start:~s end:~s alist:~s overwrite:~s"
              start end alist overwrite)
  (let* ((interval (or chosen-interval (find-interval-for-modification pstr start (trees::root (pstring-tree pstr)))))
         (len-to-set (- end start)))
    ;; change current interval properties
    (block property-change
      (when (equalp alist (interval-alist interval))
        (return-from property-change))
      (log-debug2 "start modification of interval ~s"
                  interval)
      ;; interval will be cut on 3 parts
      ;; (left | modified | right)
      ;; remainder will go to the next interval, if it is possible
      ;; that's why we keep right interval in variable
      (let* ((full-len (interval-len interval))
             (left-len (- start (interval-start interval)))
             (right-len (max 0 (- full-len left-len len-to-set)))
             (remainder (- len-to-set (- full-len right-len left-len)))
             (modified-interval interval)
             (right-interval interval))
        (when (> left-len 0)
          (setq modified-interval (split-interval pstr modified-interval left-len))
          (setq right-interval modified-interval))
        (when (> right-len 0)
          (setq right-interval (split-interval pstr modified-interval (- full-len right-len left-len))))
        (log-debug2 "selected interval for modification:~s" modified-interval)
        (let ((modified-alist (unless overwrite (interval-alist modified-interval))))
          (dolist (cons alist)
            (setf (alexandria:assoc-value modified-alist (car cons)) (cdr cons)))
          (setf (interval-alist modified-interval) (nsorted-alist modified-alist)))
        (log-debug2 "interval after modification:~s" modified-interval)
        (log-debug2 "remainder:~s" remainder)
        (when (and (> remainder 0) (interval-next right-interval))
          (set-properties pstr (+ start (interval-len modified-interval)) end alist overwrite (interval-next right-interval))
          )
        (log-debug2 "set-properties result: ~s" pstr)))))

(defun* (build-pstring -> pstring) ((cl-string string) &optional (alist nil))
  #M"basic function to create one simple pstring, alist will be sorted"
  (let ((s1 (make-empty-pstring))
        (init-len (length cl-string)))
    (loop for start from 0 below init-len by *normal-interval-length*
          do (let* ((cl-chunk (str:substring start (min init-len (+ start *normal-interval-length*)) cl-string))
                    (interval2 (make-interval
                                :start 0
                                :chunk cl-chunk
                                :alist (nsorted-alist (copy-alist alist))
                                :len (length cl-chunk)))
                    (s2 (make-empty-pstring)))
               (with-slots (tree len) s2
                 (trees:insert interval2 tree)
                 (setq len (length cl-chunk)))
               (ninsert s2 s1)))
    s1))

(defun* debug-print ((pstr pstring))
  (with-slots (len first tree) pstr
    (format t "pstring debug print: ~s length: ~s~%first: ~s~%"
            pstr len first)
    (trees:pprint-tree tree)))

(defun* (pstring-char= -> boolean) ((pstr1 pstring) (pstr2 pstring))
  (do-generators ((char1 (generate-chars pstr1) :fill-value nil)
                  (char2 (generate-chars pstr2) :fill-value nil))
    (unless (eq char1 char2)
      (return-from pstring-char= nil)))
  t)

(defun* (pstring-properties= -> boolean) ((pstr1 pstring) (pstr2 pstring))
  (loop
    with gen1 = (generate-property-intervals pstr1)
    with gen2 = (generate-property-intervals pstr2)
    with start1 and end1 and alist1
    with start2 and end2 and alist2
    do (setf (values start1 end1 alist1) (cl:funcall gen1))
       (setf (values start2 end2 alist2) (cl:funcall gen2))
       ;; (log-debug "prop comparison ~s ~s"
       ;;            (list start1 end1 alist1)
       ;;            (list start2 end2 alist2))
       (when (and (eq start1 'generator-stop)
                  (eq start2 'generator-stop))
         (return-from pstring-properties= t))
       (unless (and (eq start1 start2)
                    (eq end1 end2)
                    (equalp alist1 alist2)
                    (not (eq start1 'generator-stop))
                    (not (eq start2 'generator-stop)))
         (return-from pstring-properties= nil)))
  t)

(defun* (pstring= -> boolean) ((pstr1 pstring) (pstr2 pstring))
  (and (pstring-char= pstr1 pstr2)
       (pstring-properties= pstr1 pstr2)))

(defun* (copy-pstring -> pstring) ((pstr pstring))
  (let ((result (make-empty-pstring)))
    (ninsert pstr result)
    result))

(test test-copy-pstring
  (let* ((pstr1 (build-pstring "abc" '((a . 3))))
         (pstr1-c (copy-pstring pstr1))
         (pstr2 (make-empty-pstring))
         (pstr3 (build-pstring "")))
    (is (pstring= pstr1 pstr1-c))
    (is-false (eq pstr1 pstr1-c))
    (is (pstring= pstr2 (copy-pstring pstr2)))
    (is (pstring= pstr3 (copy-pstring pstr3)))))

(defun* (to-cl-string -> string) ((pstr pstring))
  (with-output-to-string (stream)
    (do-generator (char (generate-chars pstr))
      (write-char char stream))))

(defun* compute-hash ((pstr pstring) &optional with-properties)
  (let* ((cl-string (to-cl-string pstr))
         (obj (list cl-string)))
    (when with-properties
      (do-generator (start end alist (generate-property-intervals pstr))
        (push (sxhash (list start end alist)) obj)))
    (sxhash obj)))

(test test-compute-hash
  (let ((pstr1 (build-pstring "asdf"))
        (pstr2 (build-pstring "asdf" '((asdf . t))))
        (pstr3 (build-pstring "asdf" '((asdf . t))))
        )
    (is-false (= (compute-hash pstr1 t)
                 (compute-hash pstr2 t)))
    (is-true (= (compute-hash pstr1 nil)
                (compute-hash pstr2 nil)))
    (is-true (= (compute-hash pstr2 t)
                (compute-hash pstr3 t)
                ))))

;;; reader for common lisp

;; (defun* (pstring-reader -> pstring) ((stream stream) (subchar character) arg)
;;   #M"simple pstring reader, supports strings without properties
;;      used for basic interaction with pstrings from common lisp code"
;;   (declare (ignore arg))

;;   (let (result)
;;     (cond
;;       ((char= subchar #\P)
;;        (unless (char-equal (read-char stream) #\")
;;          (error "double quote not found after #P"))
;;        (let ((cl-string
;;                (with-output-to-string (str-stream)
;;                  (loop for next-char = (read-char stream)
;;                        until (char-equal #\" next-char)
;;                        do (write-char next-char str-stream)))))
;;          (setq result (build-pstring cl-string))))
;;       (t (error "Bad pstring reader subchar ~s" subchar)))
;;     result))


;; (named-readtables:defreadtable pstring-syntax
;;   (:merge mstrings:mstring-syntax)
;;   (:dispatch-macro-char #\# #\P #'pstring-reader))

;; TODO: add operations < > substr contains for pstring

(test test-basic-insertion
  (let* ((*normal-interval-length* 2)
         (*max-interval-length* 4))
    ;; insert test
    (let ((pstr1 (build-pstring "af"))
          (pstr2 (build-pstring "bc"))
          (pstr3 (build-pstring "de")))
      (ninsert pstr2 pstr1 1)
      (ninsert pstr3 pstr1 3)
      (is (string= "\"abcdef\"" (format nil "~s" pstr1)))))
  )

(test test-comparison
  (let ((pstr1 (build-pstring "abc"))
        (pstr2 (build-pstring "ab"))
        (pstr3 (build-pstring "abc"))
        (pstr4 (build-pstring "abc" '((prop1 . "value"))))
        (pstr5 (build-pstring "abc" '((prop2 . "value"))))
        (pstr6 (build-pstring "abc" '((prop2 . "value")))))
    (is-false (pstring= pstr1 pstr2))
    (is-true (pstring= pstr1 pstr3))
    (is-false (pstring= pstr1 pstr4))
    (is-true (pstring-char= pstr3 pstr4))
    (is-false (pstring= pstr4 pstr5))
    (is-true (pstring= pstr5 pstr6))
    ))

(test test-concatenation
  (let* ((*normal-interval-length* 2)
         (*max-interval-length* 4))
    ;; concat left test
    (let ((pstr1 (build-pstring "ef"))
          (pstr2 (build-pstring "cd"))
          (pstr3 (build-pstring "ab")))
      (ninsert pstr2 pstr1 0)
      (ninsert pstr3 pstr1 0)
      (is (string= "\"abcdef\"" (format nil "~s" pstr1))))
    ;; concat right test
    (let ((pstr1 (build-pstring "ab"))
          (pstr2 (build-pstring "cd"))
          (pstr3 (build-pstring "ef")))
      (ninsert pstr2 pstr1)
      (ninsert pstr3 pstr1)
      (is (string= "\"abcdef\"" (format nil "~s" pstr1))))
    )
  )

(test property-operations
  (let* ((*normal-interval-length* 2)
         (*max-interval-length* 4)
         )
    ;; one-interval test
    (let ((pstr (build-pstring "ab" '((prop2 . 2) (prop1 . val1)))))
      (is (string= "#(\"ab\" 0 2 (PROP1 VAL1 PROP2 2))" (format nil "~s" pstr))))

    ;; split test
    (let ((pstr1 (build-pstring "ab" '((prop3 . 2) (prop1 . val1))))
          (pstr2 (build-pstring "cd" '((prop3 . 2) (prop1 . val1)))))
      (ninsert pstr2 pstr1)
      ;; (debug-print pstr1)
      (is (= 1 (trees:size (pstring-tree pstr1))))
      (set-properties pstr1 1 3 '((prop3 . 3) (prop2 . 2)))
      ;; (log-debug "~s" pstr1)
      (is (= 3 (trees:size (pstring-tree pstr1))))
      (is (string= "#(\"abcd\" 0 1 (PROP1 VAL1 PROP3 2) 1 3 (PROP1 VAL1 PROP2 2 PROP3 3) 3 4 (PROP1 VAL1 PROP3 2))"
                   (format nil "~s" pstr1)))
      )

    ;; multi-interval test
    (let ((pstr1 (build-pstring "ab" '((prop3 . 2) (prop1 . val1))))
          (pstr2 (build-pstring "cd")))
      (ninsert pstr2 pstr1)
      (is (string= "#(\"abcd\" 0 2 (PROP1 VAL1 PROP3 2))"
                   (format nil "~s" pstr1)))
      )

    ;; print interval joinining
    (let ((pstr (build-pstring "abcdef")))
      (set-properties pstr 1 3 '((prop2 . 2) (prop1 . val1)))
      (set-properties pstr 3 5 '((prop2 . 2) (prop1 . val1)))
      (is (string= "#(\"abcdef\" 1 5 (PROP1 VAL1 PROP2 2))"
                   (format nil "~s" pstr)))
      )

    ;; different props not joining + partial overwrite
    (let ((pstr (build-pstring "abcdef")))
      (set-properties pstr 0 4 '((prop2 . 2) (prop1 . val1)))
      (set-properties pstr 3 5 '((prop2 . 3) (prop1 . val1)))
      (is (string= "#(\"abcdef\" 0 3 (PROP1 VAL1 PROP2 2) 3 5 (PROP1 VAL1 PROP2 3))"
                   (format nil "~s" pstr))))

    ;; property overwrite test
    (let ((pstr (build-pstring "abcdef" '((prop1 . val1)))))
      (set-properties pstr 1 5 '((prop2 . val2)) t)
      (set-properties pstr 3 5 '((prop3 . val3)) t)
      (is (string= "#(\"abcdef\" 0 1 (PROP1 VAL1) 1 3 (PROP2 VAL2) 3 5 (PROP3 VAL3) 5 6 (PROP1 VAL1))"
                   (format nil "~s" pstr)))
      )

    (let ((pstr (build-pstring "")))
      (signals invalid-pstring-operation (set-properties pstr 0 1 '((prop1 . val1)))))

    ;; TODO: use emacs symbols lower-case form in this test
    ))
(defun test-me ()
  (run! 'cl-emacs/types/pstrings))
