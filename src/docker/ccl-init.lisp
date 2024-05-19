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
(let ((asdf-path (concatenate 'string (ccl:getenv "ASDF_DIR") "build/asdf.lisp")))
  (format t "loading asdf from ~s" asdf-path)
  (load asdf-path))

(unless (probe-file (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))
  (error "please install ocicl to continue"))
(load (truename "~/.local/share/ocicl/ocicl-runtime.lisp"))

(setq ocicl-runtime::*systems-dir* (concatenate 'string (uiop:getenv "OCICL_GLOBALDIR") "systems/"))
(setq ocicl-runtime::*systems-csv* (concatenate 'string (uiop:getenv "OCICL_GLOBALDIR") "systems.csv"))
(setq ocicl-runtime::*verbose* t)

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:tree ,(uiop:getenv "OCICL_GLOBALDIR"))))
