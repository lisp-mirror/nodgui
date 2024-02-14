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

(in-package :nodgui.vec2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec2-type ()
    'single-float)

  (deftype vec2 ()
    "A 2d vector of single floats."
    `(simple-array vec2-type (2)))

  (defun vec2p (a)
    (typep a 'vec2))

  (defun vec2 (x y)
    (let ((v (make-array-frame 2 0f0 'vec2-type t)))
      (declare (vec2 v))
      (setf (elt v 0) (->f x)
            (elt v 1) (->f y))
      v))

  (defun vec2= (a b)
    (and (= (elt a 0) (elt b 0))
         (= (elt a 1) (elt b 1))))

  (alexandria:define-constant +vec2-zero+ (vec2 0f0 0f0)
    :test #'vec2=)

  (defun vec2-x (a)
    (elt a 0))

  (defun vec2-y (a)
    (elt a 1))

  (defun sequence->vec2 (vec)
    (vec2 (->f (elt vec 0))
          (->f (elt vec 1))))

  (defun make-fresh-vec2 ()
    (make-array-frame 2 0 'vec2-type t)))

(defun copy-vec2 (old)
  (let ((res (make-array-frame 2 0.0 'vec2-type t)))
    (declare (vec2 res))
    (setf (elt res 0) (elt old 0)
          (elt res 1) (elt old 1))
    res))

(defun vec2* (vec val)
  (declare (vec2 vec))
  (vec2 (* (elt vec 0) val)
        (* (elt vec 1) val)))

(defun vec2/ (vec val)
  (declare (vec2 vec))
  (vec2 (/ (elt vec 0) val)
        (/ (elt vec 1) val)))

(defun vec2~ (a b)
  (and (epsilon= (elt a 0) (elt b 0))
       (epsilon= (elt a 1) (elt b 1))))

(defun vec2+ (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (vec2 (+ (elt a 0) (elt b 0))
        (+ (elt a 1) (elt b 1))))

(defun vec2- (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (vec2 (- (elt a 0) (elt b 0))
        (- (elt a 1) (elt b 1))))

(defun vec2-negate (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (vec2 (- (elt a 0))
        (- (elt a 1))))

(defun vec2-length (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (sqrt (+ (expt (elt a 0) 2.0)
                     (expt (elt a 1) 2.0))))

(defun vec2-normalize (a)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (let ((length (vec2-length a)))
    (vec2 (/ (elt a 0) length)
          (/ (elt a 1) length))))

(defun vec2-dot-product (a b)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a b))
  (+ (* (elt a 0) (elt b 0)) (* (elt a 1) (elt b 1))))

(defun vec2-perpendicular (a)
  (declare (vec2 a))
  (vec2 (- (vec2-y a)) (vec2-x a)))

(defun vec2-perp-dot-product (a b)
  "|v1| |v2| sin(theta)"
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (vec2 a))
  (vec2-dot-product (vec2-perpendicular a) b))

(defun vec2-rotate (v angle)
  (declare (vec2 v))
  (declare (single-float angle))
  (vec2 (- (* (vec2-x v) (cos angle)) (* (vec2-y v) (sin angle)))
        (+ (* (vec2-x v) (sin angle)) (* (vec2-y v) (cos angle)))))
