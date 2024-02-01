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

;; derived from:

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

(in-package :nodgui.matrix)

;;;; ACCESSORS

(deftype matrix ()
  "4x4 matrix of single floats, represented as a one-dimensional vector stored
in column-major order."
  `(simple-array single-float (16)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mref (matrix row column)
    "Accessor for value in the specificed ROW and COLUMN in MATRIX."
    (declare (fixnum row column))
    (declare (matrix matrix))
    (declare (optimize (speed 3)))
    (aref matrix (to:f+ row (to:f* column 4)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun (setf mref) (value matrix row column)
    (declare (fixnum row column))
    (declare (matrix matrix))
    (declare (optimize (speed 3)))
    (setf (aref matrix (to:f+ row (to:f* column 4))) value)))

;;; PRETTY-PRINTING

(defun pprint-matrix (stream matrix)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (matrix stream :type nil :identity nil)
      (dotimes (i 4)
        (format stream "[~s, ~s, ~s, ~s]"
                (mref matrix i 0)
                (mref matrix i 1)
                (mref matrix i 2)
                (mref matrix i 3))
        (unless (= 3 i)
          (pprint-newline :mandatory stream))))))
(set-pprint-dispatch 'matrix 'pprint-matrix)

;;;; PREDICATES

(defun matrixp (object)
  "Return true of OBJECT is a matrix."
  (typep object 'matrix))

;;;; COMPARISON

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun matrix= (m1 m2)
    "Return true if MATRIX M1 is elementwise equal to MATRIX M1."
    (declare (matrix m1 m2))
    (declare (optimize (speed 3)))
    (dotimes (i 16 t)
      (unless (to:d= (aref m1 i) (aref m2 i))
        (return nil)))))

(defun matrix~ (m1 m2 &optional (epsilon u:*default-epsilon*))
  "Return true if MATRIX M1 and MATRIX M2 are elementwise within EPSILON of each other.
EPSILON defaults to +DEFAULT-EPSILON+"
  (declare (matrix m1 m2))
  (declare (optimize (speed 3)))
  (dotimes (i 16 t)
    (unless (u:epsilon= (aref m1 i) (aref m2 i) epsilon)
      (return nil))))

;;;; CONSTRUCTORS


(eval-when (:compile-toplevel :load-toplevel :execute)
  (u:definline matrix (m11 m12 m13 m14
                           m21 m22 m23 m24
                           m31 m32 m33 m34
                           m41 m42 m43 m44)
    "Construct MATRIX with the given elements (arguments are provided in row
major order.)"
    (make-array 16
                :element-type 'single-float
                :initial-contents (list m11 m21 m31 m41
                                        m12 m22 m32 m42
                                        m13 m23 m33 m43
                                        m14 m24 m34 m44))))

(u:definline zero-matrix ()
  "Construct a zero matrix."
  (matrix (to:d 0) (to:d 0) (to:d 0) (to:d 0)
          (to:d 0) (to:d 0) (to:d 0) (to:d 0)
          (to:d 0) (to:d 0) (to:d 0) (to:d 0)
          (to:d 0) (to:d 0) (to:d 0) (to:d 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun identity-matrix ()
    "Construct an identity matrix."
    (matrix (to:d 1) (to:d 0) (to:d 0) (to:d 0)
            (to:d 0) (to:d 1) (to:d 0) (to:d 0)
            (to:d 0) (to:d 0) (to:d 1) (to:d 0)
            (to:d 0) (to:d 0) (to:d 0) (to:d 1))))

(a:define-constant +identity-matrix+ (identity-matrix) :test #'matrix=
  :documentation "Constant identity matrix.")

;;;; MATRIX MULTIPLICATION

(defun matrix* (&rest matrices)
  "Multiply MATRICES. The result might not be freshly allocated if all,
or all but one multiplicant is an identity matrix."
  (declare (optimize (speed 3)))
  (macrolet ((inline-mul (left right dest)
               `(progn
                  ,@(loop for i below 4
                       append (loop for j below 4
                            collect
                              `(setf
                                (mref ,dest ,i ,j)
                                (+ ,@(loop for k below 4
                                        collect `(* (mref ,left ,i ,k) (mref ,right ,k ,j))))))))))
   (labels ((mul (left more)
              (declare (matrix left))
              (if more
                  (if (eq left +identity-matrix+)
                      (mul (pop more) more)
                      (let ((right (pop more)))
                        (declare (matrix right))
                        (if (eq +identity-matrix+ right)
                            (mul left more)
                            (let ((result (zero-matrix)))
                              (declare (matrix result))
                              (inline-mul left right result)
                              (mul result more)))))
                  left)))
     (cond ((not matrices)
            +identity-matrix+)
           ((cdr matrices)
            (mul (car matrices) (cdr matrices)))
           (t
            (car matrices))))))

;;;; TRANSFORMATIONS

(defun translate* (x y z)
  "Construct a translation matrix from translation factors X, Y and Z."
  (declare (optimize (speed 3)))
  (matrix 1f0 0f0 0f0 x
          0f0 1f0 0f0 y
          0f0 0f0 1f0 z
          0f0 0f0 0f0 1f0))

(defun translate (vec)
  "Construct a translation matrix using first three elements of VEC as the
translation factors."
  (declare (vec3 vec))
  (declare (optimize (speed 3)))
  (translate* (aref vec 0) (aref vec 1) (aref vec 2)))

(defun scale* (x y z)
  "Construct a scaling matrix from scaling factors X, Y, and Z."
  (declare (optimize (speed 3)))
  (matrix x    0f0  0f0  0f0
          0f0  y    0f0  0f0
          0f0  0f0  z    0f0
          0f0  0f0  0f0  1f0))

(defun scale (vec)
  "Construct a scaling matrix using first threee elements of VEC as the
scaling factors."
  (declare (vec3 vec))
  (declare (optimize (speed 3)))
  (scale* (aref vec 0) (aref vec 1) (aref vec 2)))

(defun rotate* (x y z)
  "Construct a rotation matrix from rotation factors X, Y, Z."
  (declare (optimize (speed 3)))
  (let ((rotate (identity-matrix)))
    (unless (to:d= 0f0 z)
      (let ((c (to:dcos z))
            (s (to:dsin z)))
        (setf rotate (matrix* rotate
                              (matrix c     (to:d- s) 0f0    0f0
                                      s     c         0f0    0f0
                                      0f0   0f0       1f0    0f0
                                      0f0   0f0       0f0    1f0)))))
    (unless (to:d= 0f0 y)
      (let ((c (to:dcos y))
            (s (to:dsin y)))
        (setf rotate (matrix* rotate
                              (matrix c          0f0  s     0f0
                                      0f0        1f0  0f0   0f0
                                      (to:d- s)  0f0  c     0f0
                                      0f0        0f0  0f0   1f0)))))
    (unless (to:d= 0f0 x)
      (let ((c (to:dcos x))
            (s (to:dsin x)))
        (setf rotate (matrix* rotate
                              (matrix 1f0   0f0   0f0        0f0
                                      0f0   c     (to:d- s)  0f0
                                      0f0   s     c          0f0
                                      0f0   0f0   0f0        1f0)))))
    rotate))

(defun rotate (vec)
  "Construct a rotation matrix using first three elements of VEC as the
rotation factors."
  (declare (vec3 vec))
  (declare (optimize (speed 3)))
  (rotate* (aref vec 0) (aref vec 1) (aref vec 2)))

(defun rotate-around (v radians)
  "Construct a rotation matrix that rotates by RADIANS around VEC V. 4th
element of V is ignored."
  (declare (vec3 v))
  (declare (optimize (speed 3)))
  (cond ((vec3= v (vec3 1f0 0f0 0f0))
         (rotate* radians 0f0 0f0))
        ((vec3= v (vec3 0f0 1f0 0f0))
         (rotate* 0f0 radians 0f0))
        ((vec3= v (vec3 0f0 0f0 1f0))
         (rotate* 0f0 0f0 radians))
        (t
         (let ((c (to:dcos radians))
               (s (to:dsin radians))
               (g (to:d- 1f0 (cos radians))))
           (let* ((x (aref v 0))
                  (y (aref v 1))
                  (z (aref v 2))
                  (gxx (to:d* g x x)) (gxy (to:d* g x y)) (gxz (to:d* g x z))
                  (gyy (to:d* g y y)) (gyz (to:d* g y z)) (gzz (to:d* g z z)))
             (matrix
              (to:d+ gxx c)           (to:d- gxy (to:d* s z)) (to:d+ gxz (to:d* s y)) 0f0
              (to:d+ gxy (to:d* s z)) (to:d+ gyy c)           (to:d- gyz (to:d* s x)) 0f0
              (to:d- gxz (to:d* s y)) (to:d+ gyz (to:d* s x)) (to:d+ gzz c)           0f0
              0f0                        0f0                  0f0                     1f0))))))

(defun reorient (v1 v2)
  "Construct a transformation matrix to reorient V1 with V2."
  (declare (vec3 v1 v2))
  (declare (optimize (speed 3)))
  (let ((nv1 (vec3-normalize v1))
	(nv2 (vec3-normalize v2)))
    (declare (vec3 nv1 nv2))
    (if (vec3~ nv1 nv2)
	(identity-matrix)
	(rotate-around (vec3-normalize (vec3-cross-product nv1 nv2))
		       (to:dacos (vec3-dot-product nv1 nv2))))))

(defun transpose-matrix (matrix)
  "Transpose of MATRIX."
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (let ((transpose (zero-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (mref transpose i j) (mref matrix j i))))
    transpose))

(defun inverse-matrix (matrix)
  "Inverse of MATRIX. Signals an error if there is no inverse."
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (if (eq matrix +identity-matrix+)
      +identity-matrix+
      (if (and (= 0f0 (mref matrix 3 0) (mref matrix 3 1) (mref matrix 3 2))
               (= 1f0 (mref matrix 3 3)))
          ;; Affine matrix, fast track (and less loss of accuracy from multiplications)
          (let ((inverse (zero-matrix)))
            ;; Inverse of the upper left 3x3
            (let ((det (submatrix-determinant matrix)))
              (if (zerop (the to::desired-type det))
                  ;; If the 3x3 is zero, we're fine -- otherwise punt to the complete
                  ;; implementation.
                  (dotimes (i 3)
                    (dotimes (j 3)
                      (unless (= 0f0 (mref matrix i j))
                        (return-from inverse-matrix (generic-inverse-matrix matrix)))))
                  (macrolet ((inv ((i j) (ai aj bi bj) (ci cj di dj))
                             `(setf (mref inverse ,(1- i) ,(1- j))
                                    (to:d/ (to:d- (to:d* (mref matrix ,(1- ai) ,(1- aj))
                                                         (mref matrix ,(1- bi) ,(1- bj)))
                                                  (to:d* (mref matrix ,(1- ci) ,(1- cj))
                                                         (mref matrix ,(1- di) ,(1- dj))))
                                           det))))
                  (inv (1 1) (2 2 3 3) (2 3 3 2))
                  (inv (1 2) (1 3 3 2) (1 2 3 3))
                  (inv (1 3) (1 2 2 3) (1 3 2 2))
                  (inv (2 1) (2 3 3 1) (2 1 3 3))
                  (inv (2 2) (1 1 3 3) (1 3 3 1))
                  (inv (2 3) (1 3 2 1) (1 1 2 3))
                  (inv (3 1) (2 1 3 2) (2 2 3 1))
                  (inv (3 2) (1 2 3 1) (1 1 3 2))
                  (inv (3 3) (1 1 2 2) (1 2 2 1)))))
            ;; translation: negation after dotting with upper rows
            (let ((x (mref matrix 0 3))
                  (y (mref matrix 1 3))
                  (z (mref matrix 2 3)))
              (dotimes (i 3)
                (setf (mref inverse i 3) (to:d- (to:d+ (to:d* x (mref inverse i 0))
                                                       (to:d* y (mref inverse i 1))
                                                       (to:d* z (mref inverse i 2)))))))
            ;; affine bottom row (0 0 0 1)
            (setf (mref inverse 3 3) 1f0)
            inverse)
          (generic-inverse-matrix matrix))))

(defun submatrix-determinant (matrix)
  "Determinant of the upper left 3x3 submatrix of MATRIX."
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (macrolet ((a (i j)
               `(mref matrix ,(- i 1) ,(- j 1))))
    (- (+ (* (a 1 1) (a 2 2) (a 3 3))
          (* (a 2 1) (a 3 2) (a 1 3))
          (* (a 3 1) (a 1 2) (a 2 3)))
       (* (a 1 1) (a 3 2) (a 2 3))
       (* (a 3 1) (a 2 2) (a 1 3))
       (* (a 2 1) (a 1 2) (a 3 3)))))

(defun matrix-determinant (matrix)
  "Determinant of MATRIX."
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (macrolet ((a (i j)
               `(mref matrix (- ,i 1) (- ,j 1))))
    (- (+ (* (a 1 1) (a 2 2) (a 3 3) (a 4 4))
          (* (a 1 1) (a 2 3) (a 3 4) (a 4 2))
          (* (a 1 1) (a 2 4) (a 3 2) (a 4 3))

          (* (a 1 2) (a 2 1) (a 3 4) (a 4 3))
          (* (a 1 2) (a 2 3) (a 3 1) (a 4 4))
          (* (a 1 2) (a 2 4) (a 3 3) (a 4 1))

          (* (a 1 3) (a 2 1) (a 3 2) (a 4 4))
          (* (a 1 3) (a 2 2) (a 3 4) (a 4 1))
          (* (a 1 3) (a 2 4) (a 3 1) (a 4 2))

          (* (a 1 4) (a 2 1) (a 3 3) (a 4 2))
          (* (a 1 4) (a 2 2) (a 3 1) (a 4 3))
          (* (a 1 4) (a 2 3) (a 3 2) (a 4 1)))

       (* (a 1 1) (a 2 2) (a 3 4) (a 4 3))
       (* (a 1 1) (a 2 3) (a 3 2) (a 4 4))
       (* (a 1 1) (a 2 4) (a 3 3) (a 4 2))

       (* (a 1 2) (a 2 1) (a 3 3) (a 4 4))
       (* (a 1 2) (a 2 3) (a 3 4) (a 4 1))
       (* (a 1 2) (a 2 4) (a 3 1) (a 4 3))

       (* (a 1 3) (a 2 1) (a 3 4) (a 4 2))
       (* (a 1 3) (a 2 2) (a 3 1) (a 4 4))
       (* (a 1 3) (a 2 4) (a 3 2) (a 4 1))

       (* (a 1 4) (a 2 1) (a 3 2) (a 4 3))
       (* (a 1 4) (a 2 2) (a 3 3) (a 4 1))
       (* (a 1 4) (a 2 3) (a 3 1) (a 4 2)))))

;;; KLUDGE: Default is too low to do a good job with GENERIC-INVERSE-MATRIX.
#+sbcl
(eval-when (:compile-toplevel)
  (setf sb-ext:*inline-expansion-limit* 1000))

(defun generic-inverse-matrix (matrix)
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (let ((det (matrix-determinant matrix)))
    (if (to:d< (to:dabs det) u:*default-epsilon*)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (multiple-value-bind (r1 c1) (truncate (- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (- z 11) 10)
                           `(* (mref matrix ,r1 ,c1)
                               (mref matrix ,r2 ,c2)
                               (mref matrix ,r3 ,c3)))))))
          (let ((m
                 (matrix
                  ;; row 1
                  (- (+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
                     (a 22 34 43) (a 23 32 44) (a 24 33 42))
                  (- (+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
                     (a 12 33 44) (a 13 34 42) (a 14 32 43))
                  (- (+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
                     (a 12 24 43) (a 13 22 44) (a 14 23 42))
                  (- (+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
                     (a 12 23 34) (a 13 24 32) (a 14 22 33))
                  ;; row 2
                  (- (+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
                     (a 21 33 44) (a 23 34 41) (a 24 31 43))
                  (- (+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
                     (a 11 34 43) (a 13 31 44) (a 14 33 41))
                  (- (+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
                     (a 11 23 44) (a 13 24 41) (a 14 21 43))
                  (- (+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
                     (a 11 24 33) (a 13 21 34) (a 14 23 31))
                  ;; row 3
                  (- (+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
                     (a 21 34 42) (a 22 31 44) (a 24 32 41))
                  (- (+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
                     (a 11 32 44) (a 12 34 41) (a 14 31 42))
                  (- (+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
                     (a 11 24 42) (a 12 21 44) (a 14 22 41))
                  (- (+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
                     (a 11 22 34) (a 12 24 31) (a 14 21 32))
                  ;; row 4
                  (- (+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
                     (a 21 32 43) (a 22 33 41) (a 23 31 42))
                  (- (+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
                     (a 11 33 42) (a 12 31 43) (a 13 32 41))
                  (- (+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
                     (a 11 22 43) (a 12 23 41) (a 13 21 42))
                  (- (+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
                     (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
            (dotimes (i 4)
              (dotimes (j 4)
                (setf (mref m i j) (/ (mref m i j) det))))
            m)))))

(defun extract-traslation-vec (matrix)
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (vec3 (mref matrix 0 3) (mref matrix 1 3) (mref matrix 2 3)))

(defun extract-traslation-mat (matrix)
  (declare (matrix matrix))
  (declare (optimize (speed 3)))
  (let ((res (identity-matrix)))
    (setf (mref res 0 3) (mref matrix 0 3)
          (mref res 1 3) (mref matrix 1 3)
          (mref res 2 3) (mref matrix 2 3))
    res))

(defun copy-matrix-element (from to row column)
  (setf (mref to row column) (mref from row column)))

(defmacro clone-matrix (matrix)
  (alexandria:with-gensyms (results)
    `(let ((,results (zero-matrix)))
       ,@(loop for r from 0 below 4 collect
              `(progn
                 ,@(loop for c from 0 below 4 collect
                        `(copy-matrix-element ,matrix ,results ,r ,c))))
       ,results)))

(u:definline nremove-rotation (m)
  (declare (matrix m))
  (declare (optimize (speed 3)))
  (setf (mref m 0 0) 1f0)
  (setf (mref m 1 1) 1f0)
  (setf (mref m 2 2) 1f0))

(defun remove-rotation (m)
  (declare (matrix m))
  (declare (optimize (speed 3)))
  (nremove-rotation (clone-matrix m)))

(u:definline ortho (left right bottom top near far)
  (declare (optimize (speed 3)))
  (matrix (to:d/ 2f0 (to:d- right left)) 0f0 0f0 (to:d/ (to:d- (to:d+ right left)) (to:d- right left))
          0f0 (to:d/ 2f0 (to:d- top  bottom)) 0f0 (to:d/ (to:d- (to:d+ top bottom)) (to:d- top bottom))
          0f0 0f0 (to:d/ -2f0 (to:d- far near)) (to:d/ (to:d- (to:d+ far near)) (to:d- far near))
          0f0 0f0 0f0 1f0))

(u:define-compiler-macro* ortho left right bottom top near far)

(u:definline ortho* (left right bottom top)
  (declare (optimize (speed 3)))
  (matrix (to:d/ 2f0 (to:d- right left)) 0f0 0f0 (to:d/ (to:d- (to:d+ right left)) (to:d- right  left))
          0f0 (to:d/ 2f0 (to:d- top bottom)) 0f0 (to:d/ (to:d+ top  bottom) (to:d- top  bottom))
          0f0 0f0 -1f0 0f0
          0f0 0f0 0f0 1f0))

(u:define-compiler-macro* ortho* left right bottom top)

(u:definline perspective (fovy aspect near far)
  (declare (optimize (speed 3)))
  (let* ((rad (to:degree->radians fovy))
         (tan-half-fovy (to:dtan (to:d/ rad 2f0)))
         (el-0-0 (to:d/ 1f0 (to:d* aspect tan-half-fovy)))
         (el-1-1 (to:d/ 1f0 tan-half-fovy))
         (el-2-2 (to:d- (to:d/ (to:d+ near far) (to:d- far near))))
         (el-3-2 (to:d- (/ (to:d* 2f0 near far) (to:d- far near)))))
    (matrix el-0-0 0f0     0f0    0f0
            0f0    el-1-1  0f0    0f0
            0f0    0f0     el-2-2 el-3-2
            0f0    0f0    -1f0    0f0   )))

(u:define-compiler-macro* perspective fovy aspect near far)

(u:definline perspective-fov (fov width height near far)
  (declare (optimize (speed 3)))
  (let* ((rad (to:degree->radians fov))
         (h   (to:d/ (to:dcos (to:d* 0.5 rad)) (to:dsin (to:d* 0.5 rad))))
         (w   (to:d/ (to:d* h height) width))
         (el-3-2 (to:d- (to:d/ (to:d* 2f0 far near) (to:d- far near))))
         (el-2-2 (to:d- (to:d/ (to:d+ far near) (to:d- far near)))))
    (matrix w   0f0  0f0    0f0
            0f0 h    0f0    0f0
            0f0 0f0  el-2-2 el-3-2
            0f0 0f0 -1f0    0f0)))

(u:define-compiler-macro* perspective-fov fovy width height near far)

(u:definline infinite-perspective (fovy aspect near)
  (declare (optimize (speed 3)))
  (let* ((range (to:d* (tan (to:degree->radians (to:d/ fovy 2f0))) near))
         (left (to:d* (to:d- range) aspect))
         (right (to:d* range aspect))
         (bottom (to:d- range))
         (top range)
         (el-0-0 (to:d/ (to:d* 2f0 near) (to:d- right left)))
         (el-1-1 (to:d/ (to:d* 2f0 near) (to:d- top bottom)))
         (el-3-2 (to:d* -2f0 near)))
    (matrix el-0-0 0f0     0f0  0f0
            0f0    el-1-1  0f0  0f0
            0f0    0f0    -1f0  el-3-2
            0f0    0f0    -1f0  0f0   )))

(u:define-compiler-macro* infinite-perspective fovy aspect near)

(u:definline frustum (left right bottom top near far)
  (declare (optimize (speed 3)))
  (let ((el-0-0 (to:d/ (to:d* 2f0 near) (to:d- right left)))
        (el-1-1 (to:d/ (to:d* 2f0 near) (to:d- top bottom)))
        (el-2-0 (to:d/ (to:d+ right left) (to:d- right left)))
        (el-2-1 (to:d/ (to:d+ top bottom) (to:d- top bottom)))
        (el-2-2 (to:d/ (to:d- (to:d+ far  near)) (to:d- far - near)))
        (el-3-2 (to:d/ (to:d- (to:d* 2f0  far near)) (to:d- far near))))
    (matrix el-0-0 0f0     el-2-0 0f0
            0f0    el-1-1  el-2-1 0f0
            0f0    0f0     el-2-2 el-3-2
            0f0    0f0    -1f0    0f0)))

(u:define-compiler-macro* frustum left right bottom top near far)

(defun look@ (eye center up)
  (declare (vec3 eye center up))
  (declare (optimize (speed 3)))
  (let* ((f (vec3-normalize (vec3- center eye)))
         (s (vec3-normalize (vec3-cross-product f up)))
         (u (vec3-cross-product s f)))
    (declare (vec3 f s u))
    (matrix (elt s 0) (elt s 1) (elt s 2) (to:d- (vec3-dot-product s eye))
            (elt u 0) (elt u 1) (elt u 2) (to:d- (vec3-dot-product u eye))
            (to:d- (elt f 0)) (to:d- (elt f 1)) (to:d- (elt f 2)) (vec3-dot-product f eye)
            0f0 0f0 0f0 1f0)))

(u:definline look@* (eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)
  (declare (optimize (speed 3)))
  (look@ (vec3 eye-x eye-y eye-z)
         (vec3 center-x center-y center-z)
         (vec3 up-x up-y up-z)))

(u:define-compiler-macro* look@* eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)
