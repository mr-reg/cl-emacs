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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/fn-substitute-in-file-name-test

  )
(in-package :cl-emacs/tests/fn-substitute-in-file-name-test)
(log-enable :cl-emacs/tests/fn-substitute-in-file-name-test :debug2)
(def-suite cl-emacs/tests/fn-substitute-in-file-name-test)
(in-suite cl-emacs/tests/fn-substitute-in-file-name-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-fn-substitute-in-file-name-test
  (is (equal 
       (reader:read-simple "\"loadup.el\"")
       (eval (reader:read-simple "(substitute-in-file-name \"loadup.el\")"))))
  
  )

;; (with-environment-variables (("_test_cl-emacs_var1" "123")
;;                              ("_test_cl-emacs var2" "456")
;;                              ("_test_cl_emacs_var3" "7 8")
;;                              ("_test_cl_emacs_var4" "abc$_test_cl_emacs_var3")
;;                              ("_test_cl_emacs_var5" "abc${_test_cl_emacs_var3}")
;;                              ("_test_cl_emacs_var6" "/test/dir")
;;                              ("_test_cl_emacs_var7" "~/abc")
;;                              ("_test_cl_emacs_var8" "~abc")
;;                              )
;;   (substitute-in-file-name "test$$test") ; test$test
;;   (substitute-in-file-name "test$var3") ; test$var3
;;   (substitute-in-file-name "test$_test_cl-emacs_var1") ; test$_test_cl-emacs_var1
;;   (substitute-in-file-name "test${_test_cl-emacs_var1}") ; test123
;;   (substitute-in-file-name "test$_test_cl_emacs_var3") ; test7 8
;;   (substitute-in-file-name "test${var3}") ; test${var3}
;;   (substitute-in-file-name "test$_test_cl_emacs_var4") ; "testabc$_test_cl_emacs_var3"
;;   (substitute-in-file-name "test${_test_cl_emacs_var5}") ; "testabc${_test_cl_emacs_var3}"
;;   (substitute-in-file-name "/test/dir//abc") ; "abc"
;;   (substitute-in-file-name "/test/dir//abc//def") ; "/def"
;;   (substitute-in-file-name "/test/dir//abc~/def") ; "/abc~/def"
;;   (substitute-in-file-name "/test/dir//abc/~//def") "/def"
;;   (substitute-in-file-name "/test/dir/~/abc//def/~/gh") "~/gh"
;;   (substitute-in-file-name "/test/dir/~abc") ; "/test/dir/~abc"
;;   (substitute-in-file-name "/test/dir/~/abc") ; "~/abc"
;;   (substitute-in-file-name "/test/dir/~/abc/~/def") ; "~/def"
;;   (substitute-in-file-name "/abc/def$_test_cl_emacs_var6") ; "/abc/def/test/dir"
;;   (substitute-in-file-name "/abc/def/$_test_cl_emacs_var7") ; "~/abc"
;;   (substitute-in-file-name "/abc/def/$_test_cl_emacs_var8") ; "/abc/def/~abc"

;;   )

(defun test-me ()
  (run! 'cl-emacs/tests/fn-substitute-in-file-name-test))
