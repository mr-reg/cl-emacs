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

(uiop:define-package :cl-emacs/image
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/image)
(log-enable :cl-emacs/image :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* clear-image-cache () "Clear the image cache.
FILTER nil or a frame means clear all images in the selected frame.
FILTER t means clear the image caches of all frames.
Anything else means clear only those images that refer to FILTER,
which is then usually a filename.

This function also clears the image animation cache.  If
ANIMATION-CACHE is non-nil, only the image spec ‘eq’ with
ANIMATION-CACHE is removed, and other image cache entries are not
evicted.

(fn &optional FILTER ANIMATION-CACHE)"
  (error ’unimplemented-error))
(defun* image-cache-size () "Return the size of the image cache.

(fn)"
  (error ’unimplemented-error))
(defun* image-flush () "Flush the image with specification SPEC on frame FRAME.
This removes the image from the Emacs image cache.  If SPEC specifies
an image file, the next redisplay of this image will read from the
current contents of that file.

FRAME nil or omitted means use the selected frame.
FRAME t means refresh the image on all frames.

(fn SPEC &optional FRAME)"
  (error ’unimplemented-error))
(defun* image-mask-p () "Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.

(fn SPEC &optional FRAME)"
  (error ’unimplemented-error))
(defun* image-metadata () "Return metadata for image SPEC.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.

(fn SPEC &optional FRAME)"
  (error ’unimplemented-error))
(defun* image-size () "Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.

FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.

Calling this function will result in the image being stored in the
image cache.  If this is not desirable, call ‘image-flush’ after
calling this function.

(fn SPEC &optional PIXELS FRAME)"
  (error ’unimplemented-error))
(defun* image-transforms-p () "Test whether FRAME supports image transformation.
Return list of capabilities if FRAME supports native transforms, nil otherwise.
FRAME defaults to the selected frame.
The list of capabilities can include one or more of the following:

 - the symbol ‘scale’ if FRAME supports image scaling
 - the symbol ‘rotate90’ if FRAME supports image rotation only by angles
    that are integral multiples of 90 degrees.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* imagep () "Value is non-nil if SPEC is a valid image specification.

(fn SPEC)"
  (error ’unimplemented-error))
(defun* init-image-library () "Initialize image library implementing image type TYPE.
Return t if TYPE is a supported image type.

If image libraries are loaded dynamically (currently the case only on
MS-Windows), load the library for TYPE if it is not yet loaded, using
the library file(s) specified by ‘dynamic-library-alist’.

(fn TYPE)"
  (error ’unimplemented-error))
