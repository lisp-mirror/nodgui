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

  (defun vec2 (&optional (x 0f0) (y 0f0))
    (let ((v (make-array-frame 2 0f0 'vec2-type t)))
      (declare (vec2 v))
      (setf (elt v 0) (to:d x)
            (elt v 1) (to:d y))
      v))

  (defun vec2-insecure (&optional (x 0f0) (y 0f0))
    #.nodgui.config:default-optimization
    (declare (to::desired-type x y))
    (let ((v (make-array-frame 2 0f0 'vec2-type t)))
      (declare (vec2 v))
      (setf (elt v 0) x
            (elt v 1) y)
      v))

  (defun vec2= (a b)
    (and (= (elt a 0) (elt b 0))
         (= (elt a 1) (elt b 1))))

  (alexandria:define-constant +vec2-zero+ (vec2 0f0 0f0)
    :test #'vec2=)

  (u:definline vec2-x (a)
    #.nodgui.config:default-optimization
    (declare (vec2 a))
    (elt a 0))

  (defsetf vec2-x (v) (new-val)
    `(setf (elt ,v 0) ,new-val))

  (u:definline vec2-y (a)
    #.nodgui.config:default-optimization
    (declare (vec2 a))
    (elt a 1))

  (defsetf vec2-y (v) (new-val)
    `(setf (elt ,v 1) ,new-val))

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

(defun vec2-scaling (v scaling)
  (declare (vec2 v scaling))
  (vec2 (to:d* (vec2-x v)
               (vec2-x scaling))
        (to:d* (vec2-y v)
               (vec2-y scaling))))

(defun vec2/ (vec val)
  (declare (vec2 vec))
  (vec2 (/ (elt vec 0) val)
        (/ (elt vec 1) val)))

(defun vec2~ (a b)
  (and (epsilon= (elt a 0) (elt b 0))
       (epsilon= (elt a 1) (elt b 1))))

(defun vec2+ (a b)
  #.nodgui.config:default-optimization
  (declare (vec2 a b))
  (vec2 (+ (elt a 0) (elt b 0))
        (+ (elt a 1) (elt b 1))))

(defun vec2-translate (a delta-x &optional (delta-y delta-x))
  #.nodgui.config:default-optimization
  (declare (vec2 a))
  (declare (to::desired-type delta-x delta-y))
  (vec2 (to:d+ delta-x (vec2-x a))
        (to:d+ delta-y (vec2-y a))))

(defun vec2- (a b)
  #.nodgui.config:default-optimization
  (declare (vec2 a b))
  (vec2 (- (elt a 0) (elt b 0))
        (- (elt a 1) (elt b 1))))

(defun vec2-negate (a)
  #.nodgui.config:default-optimization
  (declare (vec2 a))
  (vec2 (- (elt a 0))
        (- (elt a 1))))

(defun vec2-length (a)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2 a))
  (to:dsqrt (to:d+ (to:dexpt (the to::desired-type (vec2-x a)) 2f0)
                   (to:dexpt (the to::desired-type (vec2-y a)) 2f0))))

(defun vec2-normalize (a)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2 a))
  (let ((length (vec2-length a)))
    (declare (to::desired-type length))
    (vec2-insecure (/ (the to::desired-type (vec2-x a))
                      length)
                   (/ (the to::desired-type (vec2-y a))
                      length))))

(defun vec2-dot-product (a b)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2 a b))
  (+ (* (vec2-x a) (vec2-x b))
     (* (vec2-y a) (vec2-y b))))

(defun vec2-perpendicular (a)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2 a))
  (vec2-insecure (- (vec2-y a)) (vec2-x a)))

(defun vec2-perp-dot-product (a b)
  "|v1| |v2| sin(theta)"
  #.nodgui.config:default-optimization
  (declare (vec2 a))
  (vec2-dot-product (vec2-perpendicular a) b))

(defun vec2-rotate (v angle)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2 v))
  (declare (single-float angle))
  (vec2-insecure (- (* (vec2-x v) (cos angle)) (* (vec2-y v) (sin angle)))
                 (+ (* (vec2-x v) (sin angle)) (* (vec2-y v) (cos angle)))))

(deftype uivec2-type ()
  'fixnum)

(deftype uivec2 ()
  "A 2d vector of unsigned integer."
  `(simple-array fixnum (2)))

(defun uivec2 (x y)
  (let ((v (make-array-frame 2 0 'uivec2-type t)))
    (declare (uivec2 v))
    (setf (elt v 0) (truncate x)
          (elt v 1) (truncate y))
    v))

(defun uivec2-x (a)
  (elt a 0))

(defsetf uivec2-x (v) (new-val)
  `(setf (elt ,v 1) ,new-val))

(defun uivec2-y (a)
  (elt a 1))

(defsetf uivec2-y (v) (new-val)
  `(setf (elt ,v 1) ,new-val))

(defun copy-uivec2 (old)
  (let ((res (make-array-frame 2 0.0 'uivec2-type t)))
    (declare (uivec2 res))
    (setf (elt res 0) (elt old 0)
          (elt res 1) (elt old 1))
    res))

(defun uivec2* (vec val)
  (declare (uivec2 vec))
  (uivec2 (* (elt vec 0) val)
          (* (elt vec 1) val)))

(defun uivec2/ (vec val)
  (declare (uivec2 vec))
  (uivec2 (/ (elt vec 0) val)
          (/ (elt vec 1) val)))

(defun uivec2~ (a b)
  (and (epsilon= (elt a 0) (elt b 0))
       (epsilon= (elt a 1) (elt b 1))))

(defun uivec2+ (a b)
  #.nodgui.config:default-optimization
  (declare (uivec2 a b))
  (uivec2 (to:f+ (elt a 0) (elt b 0))
          (to:f+ (elt a 1) (elt b 1))))

(defun uivec2- (a b)
  #.nodgui.config:default-optimization
  (declare (uivec2 a b))
  (uivec2 (to:f- (elt a 0) (elt b 0))
          (to:f- (elt a 1) (elt b 1))))

(defun uivec2-negate (a)
  #.nodgui.config:default-optimization
  (declare (uivec2 a))
  (uivec2 (to:f- (elt a 0))
          (to:f- (elt a 1))))
