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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/chartables-test
    (:use 
     :cl-emacs/types/chartables
     :cl-emacs/lib/errors
     )
  (:local-nicknames (#:chartables #:cl-emacs/types/chartables))
  )
(in-package :cl-emacs/tests/chartables-test)
(log-enable :cl-emacs/tests/chartables-test :debug2)
(def-suite cl-emacs/tests/chartables-test)
(in-suite cl-emacs/tests/chartables-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-set-chartable-range
  ;; simple set in ascii + big range + useless optimization
  (is (cl:string= (cl:format cl:nil "~a~%~a~%~a"
                             "#^[8 nil test "
                             "#^^[3 0 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] #^^[1 0 #^^[2 0 "
                             "#^^[3 0 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
                  (cl:let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                    (set-chartable-range ct 3 3 3)
                    (chartables::optimize-chartable ct)
                    (cl:format cl:nil "~s" ct))))
  ;; simple set with zero-length range, still causes array expansion
  (is (cl:string= (cl:format cl:nil "~a~%~a~%~a"
                             "#^[8 nil test "
                             "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] #^^[1 0 #^^[2 0 "
                             "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
                  (cl:let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                    (set-chartable-range ct 4 3 3)
                    (cl:format cl:nil "~s" ct))))
  ;; simple set in big range only
  (is (cl:string= (cl:format cl:nil "~a~%~a"
                             "#^[8 nil test 8 #^^[1 0 #^^[2 0 8 "
                             "#^^[3 128 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
                  (cl:let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                    (set-chartable-range ct 128 128 3)
                    (cl:format cl:nil "~s" ct))))
  ;; complex set
  (is (cl:string= (cl:format cl:nil "~a~%~a~%~a~%~a"
                             "#^[8 nil test "
                             "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 0 #^^[2 0 "
                             "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 65536 3 3 #^^[2 73728 3 "
                             "#^^[3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
                  (cl:let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                    (set-chartable-range ct 80 73859 3)
                    (cl:format cl:nil "~s" ct))))


  ;; good optimization case
  (is (cl:string= "#^[8 nil test 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]"
                  (cl:let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                    (set-chartable-range ct 3 3 3)
                    (set-chartable-range ct 3 3 8)
                    (chartables::optimize-chartable ct)
                    (cl:format cl:nil "~s" ct))))

  )

(test test-get-chartable-value
  (cl:let* ((ct1 (make-simple-chartable :purpose 'test :default 1))
            (ct2 (make-simple-chartable :purpose 'test :parent ct1))
            (ct3 (make-simple-chartable :purpose 'test :parent ct2)))

    (set-chartable-range ct2 4 15000 2)
    (set-chartable-range ct3 10 15 3)
    (is (cl:= 3 (chartables::get-chartable-value ct3 12)))
    (is (cl:= 2 (chartables::get-chartable-value ct3 9)))
    (is (cl:= 1 (chartables::get-chartable-value ct3 15001)))
    (cl:setf (chartable-parent ct2) cl:nil)
    (is (cl:null (chartables::get-chartable-value ct3 15001)))
    (signals invalid-chartable-operation (chartables::get-chartable-value ct3 -1))
    (signals invalid-chartable-operation (chartables::get-chartable-value ct3 4194304))
    )
  )

(test test-generate-ranges
  (cl:let* ((ct1 (make-simple-chartable :purpose 'test :default 1))
            (ct2 (make-simple-chartable :purpose 'test :parent ct1))
            (ct3 (make-simple-chartable :purpose 'test :parent ct2))
            (ct4 (make-simple-chartable :purpose 'test :parent ct2 :default 4)))
    (set-chartable-range ct2 4 15000 2)
    (set-chartable-range ct3 10 15 3)
    (set-chartable-range ct4 10 15 3)

    (is (cl:equal '((0 4194303 1))
                  (get-chartable-ranges ct1)))
    (is (cl:equal '((0 3 CL:NIL) (4 15000 2) (15001 4194303 CL:NIL))
                  (chartables::get-chartable-ranges-without-parents ct2)))
    (is (cl:equal '((0 3 1) (4 15000 2) (15001 4194303 1))
                  (get-chartable-ranges ct2)))
    (is (cl:equal '((0 9 4)
                    (10 15 3)
                    (16 4194303 4))
                  (get-chartable-ranges ct4)))
    (is (cl:equal '((0 3 1)
                    (4 9 2)
                    (10 15 3)
                    (16 15000 2)
                    (15001 4194303 1))
                  (get-chartable-ranges ct3)))
    ))

(test test-print-chartable
  (cl:setf (cl:get 'el::test-purpose 'el::char-table-extra-slots) 6)
  (cl:let ((ct (make-simple-chartable :purpose 'el::test-purpose :default 10)))
    (cl:setf (cl:aref (chartable-extra-slots ct) 0) 310)
    (cl:setf (cl:aref (chartable-extra-slots ct) 1) 311)
    (is (cl:string= "#^[10 nil test-purpose 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 310 311 10 10 10 10]"
                    (cl:format cl:nil "~s" ct)))))


(defun test-me ()
  (run! 'cl-emacs/tests/chartables-test))
