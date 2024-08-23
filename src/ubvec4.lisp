;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.ubvec4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype ubvec4-type ()
    '(unsigned-byte 8))

  (deftype ubvec4 ()
    "A 4d vector of unsigned integer."
    `(simple-array ubvec4-type (4)))

  (defun ubvec4p (a)
    (typep a 'ubvec4))

  (defun ubvec4 (x y z &optional (w 0))
    #.nodgui.config:default-optimization
    (declare (ubvec4-type x y z w))
    (let ((res (make-array-frame 4 0 'ubvec4-type t)))
      (declare (ubvec4 res))
      (setf (elt res 0) x
            (elt res 1) y
            (elt res 2) z
            (elt res 3) w)
      res))

  (defun copy-ubvec4 (old)
    #.nodgui.config:default-optimization
    (declare (ubvec4 old))
    (let ((res (make-array-frame 4 0 'ubvec4-type t)))
      (declare (ubvec4 res))
      (setf (elt res 0) (elt old 0)
            (elt res 1) (elt old 1)
            (elt res 2) (elt old 2)
            (elt res 3) (elt old 3))
      res))

  (defun ubvec4= (a b)
    (every #'= a b))

  (alexandria:define-constant +ubvec4-zero+ (ubvec4 0 0 0 0)
    :test #'ubvec4=)

  (defun make-fresh-ubvec4 ()
    (make-array-frame 4 0 'ubvec4-type t)))

(defun ubvec4* (vec val)
  (ubvec4 (* (elt vec 0) val)
           (* (elt vec 1) val)
           (* (elt vec 2) val)
           (* (elt vec 3) val)))

(defun ubvec4/ (vec val)
  (ubvec4 (round (/ (elt vec 0) val))
           (round (/ (elt vec 1) val))
           (round (/ (elt vec 2) val))
           (round (/ (elt vec 3) val))))

(defun ubvec4~ (a b)
  (every #'epsilon= a b))

(defun ubvec4+ (a b)
  (ubvec4 (+ (elt a 0) (elt b 0))
           (+ (elt a 1) (elt b 1))
           (+ (elt a 2) (elt b 2))
           (+ (elt a 3) (elt b 3))))

(defun ubvec4- (a b)
  (ubvec4 (- (elt a 0) (elt b 0))
           (- (elt a 1) (elt b 1))
           (- (elt a 2) (elt b 2))
           (- (elt a 3) (elt b 3))))

(defun ubvec4-length (a)
  (sqrt (+ (expt (elt a 0) 2)
           (expt (elt a 1) 2)
           (expt (elt a 2) 2)
           (expt (elt a 3) 2))))

(defun ubvec4-normalize (a)
  (let ((length (truncate (ubvec4-length a))))
    (flet ((normalize-component (component length)
             (truncate (* 255 (/ component length)))))
      (ubvec4 (normalize-component (elt a 0) length)
              (normalize-component (elt a 1) length)
              (normalize-component (elt a 2) length)
              (normalize-component (elt a 3) length)))))

(defun ubvec4-dot-product (a b)
  (+ (* (elt a 0) (elt b 0))
     (* (elt a 1) (elt b 1))
     (* (elt a 2) (elt b 2))
     (* (elt a 3) (elt b 3))))
