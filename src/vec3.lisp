;; This software is Copyright © cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; vec3-cross-product is derived from:

;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :nodgui.vec3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec3-type ()
    'single-float)

  (deftype vec3 ()
    "A 3d vector of single floats."
    `(simple-array vec3-type (3)))

  (defun vec3p (a)
    (typep a 'vec3))

  (defun vec3 (x y z)
    (let ((res (make-array-frame 3 (to:d 0.0) 'vec3-type t)))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z)
      res))

  (u:definline vec3= (a b)
    (declare (vec3 a b))
    (and  (= (elt a 0) (elt b 0))
          (= (elt a 1) (elt b 1))
          (= (elt a 2) (elt b 2)))))

(u:define-compiler-macro* vec3= a b)

(a:define-constant +vec3-zero+ (vec3 (to:d 0.0) (to:d 0.0) (to:d 0.0))
  :test #'vec3=)

(u:definline make-fresh-vec3 ()
  (make-array-frame 3 (to:d 0.0) 'vec3-type t))

(defun copy-vec3 (old)
  (let ((res (make-array-frame 4 (to:d 0.0) 'vec3-type t)))
    (declare (vec3 res))
    (setf (elt res 0) (elt old 0)
          (elt res 1) (elt old 1)
          (elt res 2) (elt old 2))
    res))

(u:definline vec3* (vec val)
  (vec3 (to:d* (elt vec 0) val)
        (to:d* (elt vec 1) val)
        (to:d* (elt vec 2) val)))

(u:define-compiler-macro* vec3* vec val)

(u:definline vec3/ (vec val)
  (vec3 (to:d/ (elt vec 0) val)
        (to:d/ (elt vec 1) val)
        (to:d/ (elt vec 2) val)))

(u:define-compiler-macro* vec3/ vec val)

(u:definline vec3~ (a b)
  (every #'u:epsilon= a b))

(u:define-compiler-macro* vec3~ a b)

(u:definline vec3+ (a b)
  (vec3 (to:d+ (elt a 0) (elt b 0))
        (to:d+ (elt a 1) (elt b 1))
        (to:d+ (elt a 2) (elt b 2))))

(u:define-compiler-macro* vec3+ a b)

(u:definline vec3- (a b)
  (vec3 (to:d- (elt a 0) (elt b 0))
        (to:d- (elt a 1) (elt b 1))
        (to:d- (elt a 2) (elt b 2))))

(u:define-compiler-macro* vec3- a b)

(u:definline vec3-length (a)
  (sqrt (to:d+ (expt (elt a 0) 2)
               (expt (elt a 1) 2)
               (expt (elt a 2) 2))))

(u:define-compiler-macro* vec3-length a)

(u:definline vec3-normalize (a)
  (declare (vec3 a))
  (let ((length (vec3-length a)))
    (vec3 (to:d/ (elt a 0) length)
          (to:d/ (elt a 1) length)
          (to:d/ (elt a 2) length))))

(u:define-compiler-macro* vec3-normalize a)

(defun vec3-safe-normalize (a &key (epsilon u:*default-epsilon*))
  (if (u:with-epsilon (epsilon)
        (vec3~ a +vec3-zero+))
      a
      (vec3-normalize a)))

(u:definline vec3-dot-product (a b)
  (declare (vec3 a b))
  (to:d+ (to:d* (elt a 0) (elt b 0))
         (to:d* (elt a 1) (elt b 1))
         (to:d* (elt a 2) (elt b 2))))

(u:define-compiler-macro* vec3-dot-product a b)

(u:definline vec3-negate (a)
  (vec3 (to:d- (elt a 0))
        (to:d- (elt a 1))
        (to:d- (elt a 2))))

(u:define-compiler-macro* vec3-negate a)

(u:definline vec3-average (&rest vecs)
  (vec3/ (reduce #'(lambda (a b) (vec3+ a b)) vecs :initial-value +vec3-zero+)
         (coerce (length vecs) 'vec3-type)))

(u:definline vec3-average* (vecs)
  (vec3/ (reduce #'(lambda (a b) (vec3+ a b)) vecs :initial-value +vec3-zero+)
         (coerce (length vecs) 'vec3-type)))

(defun vec3-cross-product (a b)
  (let* ((result (vec3 (to:d 0) (to:d 0) (to:d 0)))
         (a1 (aref a 0))
         (a2 (aref a 1))
         (a3 (aref a 2))
         (b1 (aref b 0))
         (b2 (aref b 1))
         (b3 (aref b 2)))
    (setf (aref result 0) (to:d- (to:d* a2 b3) (to:d* a3 b2))
          (aref result 1) (to:d- (to:d* a3 b1) (to:d* a1 b3))
          (aref result 2) (to:d- (to:d* a1 b2) (to:d* a2 b1)))
    result))
