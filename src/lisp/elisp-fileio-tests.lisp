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
(in-package :cl-emacs/elisp-tests)
(def-suite elisp/fileio :in cl-emacs)
(in-suite elisp/fileio)

(test expand-file-name
  (setf cl-emacs/elisp:*context* '((:buffer ((:default-directory . "/home/emacs/src/")))))
  (is (string= "/home/emacs/src/"
               (cl-emacs/elisp:expand-file-name "./" nil)))
  (is (string= "/home/emacs/src/emacs"
               (cl-emacs/elisp:expand-file-name "emacs" nil)))
  (is (string= "/home/emacs/src/my/emacs"
               (cl-emacs/elisp:expand-file-name "my/emacs" nil)))
  (is (string= "/full/path"
               (cl-emacs/elisp:expand-file-name "/full/path" nil)))
  (is (string= "/home/emacs/full/path"
               (cl-emacs/elisp:expand-file-name "..//full/path" nil)))
  (is (string= "/some/another/full/path"
               (cl-emacs/elisp:expand-file-name "..//full/path" "/some//another/path")))
  (is (string= "/some/another/"
               (cl-emacs/elisp:expand-file-name ".." "/some//another/path")))
  (is (string= "/some/another/path/"
               (cl-emacs/elisp:expand-file-name "." "/some//another/path/")))
  (is (string= "/some/another/path/"
               (cl-emacs/elisp:expand-file-name "." "/some//another/path/.")))
  (is (string= "/some/another/path/test/"
               (cl-emacs/elisp:expand-file-name "test/" "//some//another/path/./")))
  (is (string= "/home/emacs/src/zg.zip:g.gz"
               (cl-emacs/elisp:expand-file-name "zg.zip:g.gz" nil)))
  (is (string= (namestring (user-homedir-pathname))
               (cl-emacs/elisp:expand-file-name "~" nil)))
  (is (string= (namestring (user-homedir-pathname))
               (cl-emacs/elisp:expand-file-name "." "~")))
  (setf cl-emacs/elisp:*context* '((:buffer ((:default-directory . "~foo")))))
  (is (string= "/usr/bin/~foo/bar"
               (cl-emacs/elisp:expand-file-name "bar")))
  )


