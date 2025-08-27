;; This software is Copyright © cage

;; cage  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.demo)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defparameter *pixel-buffer-context* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (a:define-constant +sin-lut-step-per-degree+ 20 :test #'=)

  (a:define-constant +float-sin-lut-step-per-degree+ (to:d +sin-lut-step-per-degree+)
    :test #'=)

  (defun populate-sin-lut ()
    (let ((lut (make-fresh-array (* +sin-lut-step-per-degree+ 361)
                                 0.01f0
                                 'to::desired-type
                                 t)))
      (loop for angle from 0f0 to 360.0f0 by (/ 1f0 +sin-lut-step-per-degree+)
            for i from 0
            do
               (setf (aref lut i) (to:dsin (to:degree->radians angle))))
      lut))

  (a:define-constant +sin-lut+ (populate-sin-lut) :test #'equalp))

(a:define-constant +1/2pi+ (to:d (/ 1 to:+2pi+)) :test #'=)

(a:define-constant +pi/2+ (to:d (/ pi 2)) :test #'=)

(defun sin-lut (angle)
  (declare (to::desired-type angle))
  #.nodgui.config:default-optimization
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (actual-angle     (if (< normalized-angle 0)
                              (to:d+ normalized-angle to:+2pi+)
                              normalized-angle))
         (index            (truncate (to:d* +float-sin-lut-step-per-degree+
                                            (to:radians->degree actual-angle)))))
    (declare (dynamic-extent normalized-angle actual-angle index))
    (aref +sin-lut+ index)))

(definline sin-lut-fire (angle)
  (declare (to::desired-type angle))
  #.nodgui.config:default-optimization
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (index            (to:f* +sin-lut-step-per-degree+
                             (the fixnum (truncate (to:radians->degree normalized-angle))))))
    (declare (dynamic-extent normalized-angle index))
    (aref +sin-lut+ index)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (a:define-constant +cos-lut-step-per-degree+ 20 :test #'=)

  (a:define-constant +float-cos-lut-step-per-degree+ (to:d +cos-lut-step-per-degree+)
    :test #'=)

  (defun populate-cos-lut ()
    (let ((lut (make-fresh-array (* +cos-lut-step-per-degree+ 360)
                                 0.01f0
                                 'to::desired-type
                                 t)))
      (loop for angle from 0.0f0 below 359.9 by (/ 1f0 +cos-lut-step-per-degree+)
            for i from 0
            do
               (setf (aref lut i) (to:dcos (to:degree->radians angle))))
      lut))

  (a:define-constant +cos-lut+ (populate-cos-lut) :test #'equalp))

(a:define-constant +1/2pi+ (to:d (/ 1 to:+2pi+)) :test #'=)

(definline cos-lut-positive-angle (angle)
  (declare (to::desired-type angle))
  #.nodgui.config:default-optimization
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (index            (truncate (to:d* +float-cos-lut-step-per-degree+
                                            (to:radians->degree normalized-angle)))))
    (declare (dynamic-extent normalized-angle normalized-angle index))
    (aref +cos-lut+ index)))

;; plasma

(definline wave->color (v)
  (declare (to::desired-type v))
  #.nodgui.config:default-optimization
  (truncate (to:d* (to:d/ (to:d+ 1.0f0 (sin-lut (to:d* 5.0 v 3.141))) 2.0f0)
                   255.0f0)))

(defun horizontal-wave (x frequency phase tick)
  (declare (to::desired-type frequency x))
  #.nodgui.config:default-optimization
  (sin-lut (to:d+ phase
                  (to:d* frequency
                         (to:d+ x tick)))))

(definline rotating-wave (x y frequency phase tick)
  (declare (to::desired-type frequency x phase tick))
  #.nodgui.config:default-optimization
  (let* ((cosine    (cos-lut-positive-angle (to:d* 8.0f0 tick)))
         (sinus     (sin-lut (to:d* 8.0f0 tick)))
         (rotated-x (to:d- (to:d* x cosine)
                           (to:d* y sinus))))
    (declare (dynamic-extent cosine sinus rotated-x))
    (sin-lut (to:d+ phase (to:d* 2.0f0
                                 (to:dabs (sin-lut (to:d* 30.0f0 tick)))
                                 frequency
                                 rotated-x)))))

(definline circular-wave (x y frequency phase tick)
  #.nodgui.config:default-optimization
  (let* ((translated-x (to:d+ (sin-lut (to:d* 6.0f0 tick))
                              (to:d- x 0.5f0)))
         (translated-y (to:d+ (sin-lut (to:d* 2.0f0 tick))
                              (to:d- y 0.5f0)))
         (dist         (to:dsqrt (to:d+ (to:dexpt translated-x 2.0f0)
                                        (to:dexpt translated-y 2.0f0)))))
    (declare (dynamic-extent translated-x translated-y dist))
    (sin-lut (to:d+ (to:d+ phase (to:d* -200.0 tick)) (to:d* frequency dist)))))

(definline normalize-coordinate (v max)
  (declare (fixnum v max))
  #.nodgui.config:default-optimization
  (to:d/ (to:d v) (to:d max)))

(definline plasma-value (x y frequency phase tick)
  #.nodgui.config:default-optimization
  (to:d+ (circular-wave x y frequency phase tick)
         (horizontal-wave x frequency phase tick)
         (rotating-wave x y frequency phase tick)))

(defun draw-plasma (buffer width height tick)
  (loop for i fixnum from 0 below width do
    (loop for j fixnum from 0 below height do
      (let ((color (wave->color (plasma-value (normalize-coordinate i width)
                                              (normalize-coordinate j height)
                                              8.0f0
                                              0.0f0
                                              tick))))
        (declare (dynamic-extent color))
        (px:set-pixel@ buffer width i j color color color)))))

;;; end plasma ;;;

;; fire

(defmacro generate-cached-random (max)
  (a:with-gensyms (cache)
    (let ((random-function-name (format-fn-symbol t "random-0-~a" max)))
      `(progn
         (let ((,cache '()))
           (defun ,random-function-name ()
             #.nodgui.config:default-optimization
             (if ,cache
                 (pop ,cache)
                 (progn
                   (setf ,cache (loop repeat 100000 collect (random ,max)))
                   (,random-function-name)))))
         (defun ,(format-fn-symbol t "1-of-~a-passes" max) ()
           #.nodgui.config:default-optimization
           (= (rem (the fixnum
                        (,random-function-name))
                   ,max)
              0))))))

(generate-cached-random 2)

(generate-cached-random 3)

(generate-cached-random 50)

(generate-cached-random 1000)

(a:define-constant +smoke-color+ (pixmap:assemble-color 10 10 10 255) :test #'=)

(definline blur-kernel (buffer width index x y time shift-spike smoke-threshold shift-down-2)
  (declare (fixnum shift-spike index x y width smoke-threshold shift-down-2))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (to::desired-type time))
  #.nodgui.config:default-optimization
  (let* ((shift-up             width)
         (shift-down           (to:f- width))
         (shift-left           -1)
         (shift-left-2         -2)
         (shift-right-2        2)
         (shift-right          1)
         (pixel-up             (aref buffer (to:f+ index shift-up)))
         (pixel-down           (aref buffer (to:f+ index shift-down)))
         (pixel-left           (aref buffer (to:f+ index shift-left)))
         (pixel-right          (aref buffer (to:f+ index shift-right)))
         (sum-red (to:f+ (the (unsigned-byte 8)
                              (pixmap:extract-red-component pixel-up))
                    (the (unsigned-byte 8)
                         (pixmap:extract-red-component pixel-down))
                    (the (unsigned-byte 8)
                         (pixmap:extract-red-component pixel-left))
                    (the (unsigned-byte 8)
                         (pixmap:extract-red-component pixel-right))))
         (sum-green (to:f+ (the (unsigned-byte 8)
                                (pixmap:extract-green-component pixel-up))
                      (the (unsigned-byte 8)
                           (pixmap:extract-green-component pixel-down))
                      (the (unsigned-byte 8)
                           (pixmap:extract-green-component pixel-left))
                      (the (unsigned-byte 8)
                           (pixmap:extract-green-component pixel-right))))
         (sum-blue (to:f+ (the (unsigned-byte 8)
                               (pixmap:extract-blue-component pixel-up))
                     (the (unsigned-byte 8)
                          (pixmap:extract-blue-component pixel-down))
                     (the (unsigned-byte 8)
                          (pixmap:extract-blue-component pixel-left))
                     (the (unsigned-byte 8)
                          (pixmap:extract-blue-component pixel-right))))
         (r-average   (ash sum-red -2))
         (g-average   (ash sum-green -2))
         (b-average   (ash sum-blue -2))
         (new-color   (pixmap:assemble-color r-average g-average b-average)))
    (declare (dynamic-extent sum-red sum-green sum-blue r-average g-average b-average
                             shift-up
                             shift-down
                             shift-left
                             shift-left-2
                             shift-right-2
                             shift-right
                             pixel-up
                             pixel-down
                             pixel-left
                             pixel-right))
    (cond
      ((and (not (< 0.0
	            (to:dabs (to:d* 10.0 (sin-lut-fire (to:d* 500.0 time))))
	            9.95))
	    (< (rem x shift-spike) 10))
       (setf (aref buffer
                   (to:f+ index
                     (cond
                       ((1-of-2-passes)
                        shift-left-2)
                       ((1-of-2-passes)
                        shift-right-2)
                       (t 0))
                     (if (1-of-2-passes)
                         0
                         shift-down)))
             new-color))
      ((and (to:f> y smoke-threshold)
            (or (and (< r-average 200)
                     (1-of-50-passes))
                (1-of-1000-passes)))
       (setf (aref buffer (to:f+ index shift-down-2))
             +smoke-color+))
      (t
       (setf (aref buffer (to:f+ index
                            (cond
                              ((1-of-2-passes)
                               shift-left)
                              ((1-of-2-passes)
                               shift-right)
                              (t 0))
                            (if (1-of-3-passes)
                                shift-down
                                0)))
             new-color)))))

(defun blur (buffer width height time float-width smoke-threshold shift-down-2)
  (declare (fixnum width height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (to::desired-type time))
  #.nodgui.config:default-optimization
  (let ((shift-spike (truncate (abs (to:d+ 20.0
                                           (to:d* 10.0
                                                  (sin-lut-fire (to:d* 1000000.0
                                                                       (to:d+ 10.0
                                                                              time)))))))))
    (declare (dynamic-extent shift-spike))
    (loop for i fixnum from (to:f* width 2)
            below (to:f- (to:f* width height)
                    width)
          do
             (multiple-value-bind (y fraction-row)
                 (truncate (to:d/ (to:d i) float-width))
               (let ((x (truncate (to:d* (to:d fraction-row) float-width))))
                 (declare (dynamic-extent x))
                 (blur-kernel buffer width i x y time
                              shift-spike
                              smoke-threshold
                              shift-down-2))))))

(defun reinforce-fire (buffer width height howmany)
  (declare (fixnum width height howmany))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  #.nodgui.config:default-optimization
  (loop repeat howmany do
    (let* ((x              (random  width))
           (minimum-seed-y (random (truncate (/ height 50))))
           (y              (- (to:f- height minimum-seed-y) 2))
           (pixel          (px:pixel@ buffer width x y))
           (seed-color     (cond
			     ((= (random 8) 0)
                              (pixmap:assemble-color 255
                                                     2
                                                     0
                                                     255))
			     ((= (random 10)
				 0)
			      (pixmap:assemble-color 255
						     200
						     0
						     255))
			     (t
			      (pixmap:assemble-color 255
						     10
						     0
						     255))))
           (new-pixel      (px:sum-pixels seed-color
                                          pixel)))
      (declare (dynamic-extent x y minimum-seed-y pixel new-pixel))
      (px:set-pixel@ buffer
                     width
                     x
                     y
                     (pixmap:extract-red-component new-pixel)
                     (pixmap:extract-green-component new-pixel)
                     (pixmap:extract-blue-component new-pixel)))))

(defun draw-fire (buffer width height howmany-seed time float-width smoke-threshold shift-down-2)
  (reinforce-fire buffer width height howmany-seed)
  (blur buffer width height time float-width smoke-threshold shift-down-2))

;;;; end fire ;;;

;;;; blit ;;;;

(defun make-blitting-rectangle (width height test-clip)
  (declare (fixnum width height))
  (let* ((rectangle-width  (+ 20 (random (truncate (- (* width  1/4) 40)))))
         (rectangle-height (+ 20 (random (truncate (- (* height 1/4) 40)))))
         (rectangle        (pixmap:make-buffer rectangle-width rectangle-height))
         (destination-x    (if test-clip
                               (+ -40 (random (+ width 40)))
                               (+ (truncate (* 3/4
                                               (random width)))
                                  20)))
         (destination-y    (if test-clip
                               (+ -40 (random (+ height 40)))
                               (+ (truncate (* 3/4
                                               (random height)))
                                  20))))
    (px:clear-buffer rectangle
                     rectangle-width
                     rectangle-height
                     255
                     50
                     20
                     10)
    (values rectangle
            rectangle-width
            rectangle-height
            destination-x
            destination-y)))

;;;; end blit ;;;;

(defstruct animation
  (thread)
  (lock (make-lock))
  (stop-p nil))

(defgeneric stop-drawing-thread (object))

(defgeneric stop-drawing-thread-p (object))

(defgeneric wait-thread (object))

(defmethod stop-drawing-thread ((object animation))
  (with-lock-held ((animation-lock object))
    (setf (animation-stop-p object) t)))

(defmethod stop-drawing-thread-p ((object animation))
  (with-lock-held ((animation-lock object))
    (animation-stop-p object)))

(defmethod wait-thread ((object animation))
  (join-thread (animation-thread object)))

(defparameter *animation* nil)

(defun draw-plasma-thread ()
  (with-accessors ((buffer px:buffer)
                   (width  ctx:width)
                   (height ctx:height)) *pixel-buffer-context*
    (let ((tick (to:d 0.0)))
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (ctx:sync *pixel-buffer-context*)
        (ctx:push-for-updating *pixel-buffer-context*
                               (lambda (dt)
                                 (declare (fixnum dt))
                                 #.nodgui.config:default-optimization
                                 (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))
                               :force-push nil)
        (ctx:push-for-rendering *pixel-buffer-context*
                                (lambda (dt)
                                  (declare (ignore dt))
                                  #.nodgui.config:default-optimization
                                  (draw-plasma buffer width height tick))
                                :force-push nil))
      (format t "STOP PlASMA!~%"))))

(defun clear-sdl-window (&key (context *pixel-buffer-context*) (force nil))
  (with-accessors ((buffer px:buffer)
                   (width  ctx:width)
                   (height ctx:height)) context
    (ctx:push-for-updating *pixel-buffer-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             #.nodgui.config:default-optimization
                             t)
                           :force-push force)
    (ctx:push-for-rendering context
                            (lambda (dt)
                              (declare (ignore dt))
                              (px:clear-buffer buffer width height 0 0 0))
                            :force-push force)))

(defun draw-fire-thread ()
  (with-accessors ((buffer px:buffer)
                   (width  ctx:width)
                   (height ctx:height)) *pixel-buffer-context*
    (let ((tick (to:d 0.0))
          (float-width     (to:d width))
          (smoke-threshold (truncate (to:d* 0.76 (to:d height))))
          (shift-down-2    (to:f* -2 width)))
      (declare (dynamic-extent float-width smoke-threshold shift-down-2))
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (ctx:sync *pixel-buffer-context*)
        (ctx:push-for-updating *pixel-buffer-context*
                               (lambda (dt)
                                 (declare (fixnum dt))
                                 #.nodgui.config:default-optimization
                                 (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))
                               :force-push nil)
        (ctx:push-for-rendering *pixel-buffer-context*
                                (lambda (dt)
                                  (declare (ignore dt))
                                  #.nodgui.config:default-optimization
                                  (draw-fire buffer
                                             width height
                                             500
                                             tick
                                             float-width smoke-threshold shift-down-2))
                                :force-push nil))
      (format t "STOP FIRE!~%"))))

(let ((test-clip nil))
  (defun draw-rectangles-thread ()
    (with-accessors ((buffer px:buffer)
                     (width  ctx:width)
                     (height ctx:height)) *pixel-buffer-context*
      ;; I should  use :force-push t  in this function call  to ensure
      ;; the  event  of clearing  the  buffer  is not  discarded,  but
      ;; instead i prefer to leave the  key parameter as nil because it
      ;; could  give a  nice transition,  if  the queue  is filled  by
      ;; leftover of plasma rendering events
      (clear-sdl-window)
      (let ((rectangles (loop repeat 1000
                              collect
                              (multiple-value-list (make-blitting-rectangle width
                                                                            height
                                                                            test-clip)))))
        (setf test-clip (not test-clip))
        (mapcar (lambda (rectangle)
                  (ctx:sync *pixel-buffer-context*)
                  (ctx:push-for-updating *pixel-buffer-context*
                                         (lambda (dt)
                                           (declare (ignore dt))
                                           #.nodgui.config:default-optimization
                                           t)
                                         :force-push t)
                  (ctx:push-for-rendering *pixel-buffer-context*
                                          (lambda (dt)
                                            (declare (ignore dt))
                                            (let ((rectangle-buffer (first  rectangle))
                                                  (rectangle-width  (second rectangle))
                                                  (rectangle-height (third  rectangle))
                                                  (x                (fourth rectangle))
                                                  (y                (fifth  rectangle)))
                                              ;; (assert (< (+ rectangle-width x)
                                              ;;            width))
                                              ;; (assert (< (+ rectangle-height y)
                                              ;;            height))
                                              (let ((px:*blending-function* #'px:blending-function-add))
                                                (px:blit rectangle-buffer
                                                         rectangle-width
                                                         buffer
                                                         width
                                                         height
                                                         0
                                                         0
                                                         y
                                                         x
                                                         rectangle-height
                                                         rectangle-width))))
                                          :force-push t))
                rectangles)
        (format t "STOP RECTANGLES!~%")))))

(defun load-bell-sprite ()
  (let ((px (make-instance 'nodgui.pixmap:png)))
    (nodgui.pixmap:load-from-vector px (nodgui.base64:decode nodgui.demo::+bell-icon+))
    px))

(defparameter *bell-sprite* (load-bell-sprite))

;; +-----+------+
;; | red | green|
;; +-----+------+
;; | blue| gray |
;; +-----+------+
;; 10px
(a:define-constant +test-sprite+
  "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAAIUlEQVQY02P8z4ACGFH5TAx4AU2lGRkYUByzatXqweI0ABCcBRCKObYxAAAAAElFTkSuQmCC"
  :test #'string=)

;; 2px
;; (a:define-constant +test-sprite+
;;   "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAFklEQVQI12P4z8DA8J+BkYHh/6pVqwEd+AT+DoCjJQAAAABJRU5ErkJggg=="
;;   :test #'string=)

(defun load-encoded-sprite (encoded-data)
  (let ((px (make-instance 'nodgui.pixmap:png)))
    (nodgui.pixmap:load-from-vector px (nodgui.base64:decode encoded-data))
    px))

(defun load-test-sprite ()
  (load-encoded-sprite  +test-sprite+))

(defparameter *test-sprite* (load-test-sprite))

(defun draw-test-sprite (buffer width height x y)
  (ctx:push-for-updating *pixel-buffer-context*
                          (lambda (dt)
                            (declare (ignore dt))
                            t)
                          :force-push t)
  (ctx:push-for-rendering *pixel-buffer-context*
                          (lambda (dt)
                            (declare (ignore dt))
                            (px:blit-transform (nodgui.pixmap:bits   *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               (nodgui.pixmap:height *test-sprite*)
                                               buffer
                                               width
                                               height
                                               0
                                               0
                                               y
                                               x
                                               (nodgui.pixmap:height *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               0.0
                                               10.0
                                               10.0
                                               0
                                               0)
                            (px:blit-transform (nodgui.pixmap:bits   *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               (nodgui.pixmap:height *test-sprite*)
                                               buffer
                                               width
                                               height
                                               0
                                               0
                                               y
                                               x
                                               (nodgui.pixmap:height *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               45.0
                                               10.0
                                               10.0
                                               0
                                               0)
                            (px:blit-transform (nodgui.pixmap:bits   *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               (nodgui.pixmap:height *test-sprite*)
                                               buffer
                                               width
                                               height
                                               5
                                               5
                                               y
                                               x
                                               (nodgui.pixmap:height *test-sprite*)
                                               (nodgui.pixmap:width  *test-sprite*)
                                               45.0
                                               10.0
                                               10.0
                                               5
                                               5))
                          :force-push t))

(defun draw-bell-sprite (buffer width height x y)
  (ctx:push-for-updating *pixel-buffer-context*
                          (lambda (dt)
                            (declare (ignore dt))
                            t)
                          :force-push t)
  (ctx:push-for-rendering *pixel-buffer-context*
                          (lambda (dt)
                            (declare (ignore dt))
                            (px:blit-transform (nodgui.pixmap:bits   *bell-sprite*)
                                               (nodgui.pixmap:width  *bell-sprite*)
                                               (nodgui.pixmap:height *bell-sprite*)
                                               buffer
                                               width
                                               height
                                               0
                                               0
                                               y
                                               x
                                               (nodgui.pixmap:height *bell-sprite*)
                                               (nodgui.pixmap:width  *bell-sprite*)
                                               (to:d (random 360.0))
                                               (to:d+ 0.2 (to:d (random 1.8)))
                                               (to:d+ 0.2 (to:d (random 1.8)))
                                               0
                                               0))
                          :force-push t))

(defun draw-lines (buffer width height x y)
  (loop for degree from 0 below 360 by 1
        for color = 0 then (truncate (abs (* 255 (sin (* 12 (/ degree 360))))))
        do
           (let ((radius 100.0))
             (ctx:push-for-updating *pixel-buffer-context*
                                    (lambda (dt)
                                      (declare (ignore dt))
                                      t)
                                    :force-push t)
             (ctx:push-for-rendering *pixel-buffer-context*
                                     (let* ((current-color color)
                                            (actual-degree degree)
                                            (radians   (to:d/ (to:d* (to:d pi)
                                                                     (to:d actual-degree))
                                                              180.0f0))
                                            (current-x (to:f+ x
                                                         (truncate (to:d* radius (to:dcos radians)))))
                                            (current-y (to:f+ y
                                                         (truncate (* radius (sin radians))))))
                                       (lambda (dt)
                                         (declare (ignore dt))
                                         (px:draw-line buffer
                                                       width
                                                       height
                                                       x y
                                                       current-x current-y
                                                       255 0 current-color  255)))
                                     :force-push t)))
  ;;parallel to x
  (loop for i from 0 below width by (truncate (/ width 20))
        for color = 0 then (truncate (to:dlerp (to:d (/ i width)) (to:d 0) (to:d 255)))
        do
    (ctx:push-for-updating *pixel-buffer-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             t)
                           :force-push t)
    (ctx:push-for-rendering *pixel-buffer-context*
                            (let ((current-color color)
                                  (x i))
                              (lambda (dt)
                                (declare (ignore dt))
                                (px:draw-line buffer
                                              width
                                              height
                                              x 0
                                              x height
                                              255 0 current-color)))
                            :force-push t))
  ;; parallel to y
  (loop for i from 0 below height by (truncate (/ height 20))
        for color = 0 then (truncate (to:dlerp (to:d (/ i height)) (to:d 0) (to:d 255)))
        do
    (ctx:push-for-updating *pixel-buffer-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             t)
                           :force-push t)
    (ctx:push-for-rendering *pixel-buffer-context*
                            (let ((current-color color)
                                  (y i))
                              (lambda (dt)
                                (declare (ignore dt))
                                (px:draw-line buffer
                                              width
                                              height
                                              0     y
                                              width y
                                              255 0 current-color)))
                            :force-push t))
  (loop for i from 0 below 300 by 10
        for color = 0 then (truncate (to:dlerp (to:d (/ i 300)) (to:d 0) (to:d 255)))
        for v0 = (vec2:uivec2 -400 -400) then (vec2:uivec2+ v0 (vec2:uivec2 -10 10))
        for v1 = (vec2:uivec2 400 400) then (vec2:uivec2+ v1 (vec2:uivec2 -10 10))
        do
           (ctx:push-for-updating *pixel-buffer-context*
                                  (lambda (dt)
                                    (declare (ignore dt))
                                    t)
                                  :force-push t)
           (ctx:push-for-rendering *pixel-buffer-context*
                                   (let* ((current-color color)
                                          (actual-x0 (vec2:uivec2-x v0))
                                          (actual-y0 (vec2:uivec2-y v0))
                                          (actual-x1 (vec2:uivec2-x v1))
                                          (actual-y1 (vec2:uivec2-y v1))
                                          (v2        (vec2:vec2-rotate (vec2:vec2 (to:d actual-x0)
                                                                                 (to:d actual-y0))
                                                                      (to:d pi)))
                                          (v3        (vec2:vec2-rotate (vec2:vec2 (to:d actual-x1)
                                                                                 (to:d actual-y1))
                                                                      (to:d pi)))

                                          (actual-x2 (truncate (vec2:uivec2-x v2)))
                                          (actual-y2 (truncate (vec2:uivec2-y v2)))
                                          (actual-x3 (truncate (vec2:uivec2-x v3)))
                                          (actual-y3 (truncate (vec2:uivec2-y v3))))
                                     (lambda (dt)
                                       (declare (ignore dt))
                                       (px:draw-line buffer
                                                     width
                                                     height
                                                     actual-x0
                                                     actual-y0
                                                     actual-x1
                                                     actual-y1
                                                     255 0 current-color)
                                       (px:draw-line buffer
                                                     width
                                                     height
                                                     actual-x2
                                                     actual-y2
                                                     actual-x3
                                                     actual-y3
                                                     255 0 current-color)))
                                    :force-push t)))

(defun make-bouncing-rectangle (w h)
  (let* ((rectangle (pixmap:make-buffer w h)))
    (px:clear-buffer rectangle w h 255 0 255)
    rectangle))

(defun bouncing-rectangle-initial-velocity ()
  (let ((v (vec2:vec2 (random 10.0f0)
                      (random 10.0f0))))
    (if (or (epsilon= (vec2:vec2-x v) 0.0 1e-3)
            (epsilon= (vec2:vec2-y v) 0.0 1e-3))
        (bouncing-rectangle-initial-velocity)
        v)))

(defun draw-bouncing-rectangle-thread ()
  (with-accessors ((buffer px:buffer)
                   (width  ctx:width)
                   (height ctx:height)) *pixel-buffer-context*
    (declare (fixnum width height))
    (let* ((rectangle-width    30)
           (rectangle-height   10)
           (rectangle          (make-bouncing-rectangle rectangle-width rectangle-height))
           (rectangle-position (vec2:vec2 (to:d (/ width 2))
                                          (to:d (/ height 2))))
           (rectangle-velocity (bouncing-rectangle-initial-velocity))
           (polygon-vertices (make-polygon-vertices))
           (polygon-color    (pixmap:assemble-color 255 0 255 255)))
      (declare (vec2:vec2 rectangle-position rectangle-velocity))
      (ctx:push-for-updating *pixel-buffer-context*
                              (lambda (dt)
                                (declare (ignore dt))
                                (setf (ctx:time-spent *pixel-buffer-context*)
                                      (ctx:get-milliseconds)))
                              :force-push nil)
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (ctx:sync *pixel-buffer-context*)
        (ctx:push-for-updating *pixel-buffer-context*
                               (lambda (dt)
                                 (declare (fixnum dt))
                                 ;;#.nodgui.config:default-optimization
                                 (when (or (to:d> (vec2:vec2-x rectangle-position)
                                                  (to:d* 2.0 (to:d width)))
                                           (to:d< (to:d+ (vec2:vec2-x rectangle-position)
                                                         (to:d rectangle-width))
                                                  (to:d* -2.0 (to:d width))))
                                   (setf (vec2:vec2-x rectangle-velocity)
                                         (to:d- (vec2:vec2-x rectangle-velocity))))
                                 (when (or (to:d> (vec2:vec2-y rectangle-position)
                                                  (to:d* 2.0 (to:d width)))
                                           (to:d< (to:d+ (vec2:vec2-y rectangle-position)
                                                         (to:d rectangle-height))
                                                  (to:d* -2.0 (to:d width))))
                                   (setf (vec2:vec2-y rectangle-velocity)
                                         (to:d- (vec2:vec2-y rectangle-velocity))))
                                 (let ((delta-pos (vec2:vec2* rectangle-velocity
                                                              (to:d* 8e-2 (to:d dt)))))
                                   (setf rectangle-position
                                         (vec2:vec2+ rectangle-position delta-pos))
                                   (setf polygon-vertices
                                         (translate-polygon-vertices polygon-vertices
                                                                     delta-pos))))
                               :force-push nil)
        (ctx:push-for-rendering *pixel-buffer-context*
                                (lambda (dt)
                                  (declare (ignore dt))
                                  ;;#.nodgui.config:default-optimization
                                  (px:clear-buffer buffer width height 0 0 0)
                                  (px:fill-rectangle buffer
                                                     width
                                                     height
                                                     (round (vec2:vec2-x rectangle-position))
                                                     (round (- (vec2:vec2-y rectangle-position)
                                                               rectangle-width))
                                                     (round (+ (vec2:vec2-x rectangle-position)
                                                               rectangle-width))
                                                     (round (+  (vec2:vec2-y rectangle-position)
                                                                rectangle-height))
                                                     255
                                                     255
                                                     0)
                                  (px:fill-circle buffer
                                                  width
                                                  height
                                                  (round (+ (vec2:vec2-x rectangle-position)
                                                            (* 2 rectangle-width)))
                                                  (round (vec2:vec2-y rectangle-position))
                                                  rectangle-width
                                                  0
                                                  255
                                                  0)
                                  (px:draw-circle buffer
                                                  width
                                                  height
                                                  (round (- (vec2:vec2-x rectangle-position)
                                                            (* 2 rectangle-width)))
                                                  (round (vec2:vec2-y rectangle-position))
                                                  rectangle-width
                                                  0
                                                  255
                                                  255)
                                  (px:blit rectangle
                                           rectangle-width
                                           buffer
                                           width
                                           height
                                           0
                                           0
                                           (round (vec2:vec2-y rectangle-position))
                                           (round (vec2:vec2-x rectangle-position))
                                           rectangle-height
                                           rectangle-width)
                                  (px:draw-polygon buffer
                                                   width
                                                   height
                                                   polygon-vertices
                                                   polygon-color))
                                :force-push nil))
      (format t "STOP BOUNCING SHAPES!~%"))))

(defun stop-animation ()
  (when (and *animation*
             (threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)
    (format t
            "anim ~a queue ~a~%"
            *animation*
            (ctx::rendering-queue *pixel-buffer-context*))))

(a:define-constant +context-width+ 320 :test #'=)

(a:define-constant +context-height+ 240 :test #'=)

(a:define-constant +sdl-frame-width+ 800 :test #'=)

(a:define-constant +sdl-frame-height+ 600 :test #'=)

(defun demo-pixel-buffer-animation (&optional (start-fire-demo nil))
  (with-nodgui ()
    (let* ((warning-label (make-instance 'label
                                         :wraplength 800
                                         :text "WARNING: This animation may potentially trigger seizures for people with photosensitive epilepsy. Viewer discretion is advised."
                                         :font (font-create "serif" :size 20 :weight :bold)))
           (sdl-frame     (ctx:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
           (buttons-frame (make-instance 'nodgui:frame
                                         :borderwidth 2
                                         :relief :groove))
           (quit-frame    (make-instance 'nodgui:frame
                                         :master buttons-frame))
           (buttons-label (make-instance 'label
                                         :master buttons-frame
                                         :text "Select animation:"))
           (notice-label  (make-instance 'label
                                         :master quit-frame
                                         :text "Please note that the button will responds with a delay because we need to wait the rendering queue to be empty"))
           (interaction-label (make-instance 'label
                                             :master nil
                                             :text "Click on the sdl window to draw a sprite, be aware that many drawing functions does not bounds checking, check the docstrings."))
           (radio-plasma  (make-instance 'radio-button
                                         :master   buttons-frame
                                         :text     "Plasma"
                                         :value    :plasma
                                         :variable "dummy"
                                         :command
                                         (lambda (value)
                                           (format t "button ~a pressed~%" value)
                                           (stop-animation)
                                           (setf *animation* (make-animation))
                                           (setf (animation-thread *animation*)
                                                 (make-thread #'draw-plasma-thread
                                                              :name "plasma"))
                                           (format t "tk event returned~%"))))
           (radio-fire    (make-instance 'radio-button
                                         :master   buttons-frame
                                         :value    :fire
                                         :variable "dummy"
                                         :text     "Fire"
                                         :command
                                         (lambda (value)
                                           (format t "button ~a pressed~%" value)
                                           (stop-animation)
                                           (setf *animation* (make-animation))
                                           (setf (animation-thread *animation*)
                                                 (make-thread #'draw-fire-thread))
                                           (format t "tk event returned~%"))))
           (radio-rectangles (make-instance 'radio-button
                                            :master   buttons-frame
                                            :value    :rectangles
                                            :variable "dummy"
                                            :text     "Rectangles"
                                            :command
                                            (lambda (value)
                                              (format t "button ~a pressed~%" value)
                                              (stop-animation)
                                              (setf *animation* (make-animation))
                                              (setf (animation-thread *animation*)
                                                    (make-thread #'draw-rectangles-thread
                                                                 :name "rectangles"))
                                              (format t "tk event returned~%"))))
           (radio-bouncing-rectangle (make-instance 'radio-button
                                                  :master   buttons-frame
                                                  :value    :bouncing-shapes
                                                  :variable "dummy"
                                                  :text     "Bouncing shapes"
                                                  :command
                                                  (lambda (value)
                                                    (format t "button ~a pressed~%" value)
                                                    (stop-animation)
                                                    (setf *animation* (make-animation))
                                                    (setf (animation-thread *animation*)
                                                          (make-thread #'draw-bouncing-rectangle-thread
                                                                       :name "bouncing rectangle"))
                                                    (format t "tk event returned~%"))))
           (button-quit  (make-instance 'button
                                        :master  quit-frame
                                        :text    "quit"
                                        :command (lambda ()
                                                   (stop-animation)
                                                   (ctx:quit-sdl *pixel-buffer-context*)
                                                   (exit-nodgui)))))
      (grid warning-label            0 0 :columnspan 2)
      (grid interaction-label        1 0 :columnspan 2)
      (grid sdl-frame                2 0)
      (grid buttons-frame            2 1 :sticky :nws)
      (grid buttons-label            1 0 :sticky :nw :padx 5 :pady 10)
      (grid radio-rectangles         2 0 :sticky :nw :padx 5)
      (grid radio-fire               3 0 :sticky :nw :padx 5)
      (grid radio-plasma             4 0 :sticky :nw :padx 5)
      (grid radio-bouncing-rectangle 5 0 :sticky :nw :padx 5)
      (grid quit-frame               5 0 :sticky :wes)
      (grid button-quit              0 0 :sticky :s)
      (grid notice-label             1 0 :sticky :sw)
      (grid-columnconfigure (root-toplevel) :all :weight 1)
      (grid-rowconfigure    (root-toplevel) :all :weight 1)
      (grid-rowconfigure    buttons-frame 5 :weight 1)
      (grid-columnconfigure buttons-frame :all :weight 1)
      (bind sdl-frame
            #$<1>$
            (let ((what-to-draw -1))
              (lambda (event)
                (incf what-to-draw)
                (with-accessors ((buffer px:buffer)
                                 (width  ctx:width)
                                 (height ctx:height)) *pixel-buffer-context*
                  (let ((scaled-x (truncate (* (event-x event)
                                               (/ +context-width+
                                                  +sdl-frame-width+))))
                        (scaled-y (truncate (* (event-y event)
                                               (/ +context-height+
                                                  +sdl-frame-height+)))))
                    (cond
                      ((= (rem what-to-draw 4) 0)
                       (draw-bell-sprite buffer width height scaled-x scaled-y))
                      ((= (rem what-to-draw 4) 1)
                       (draw-test-sprite buffer width height scaled-x scaled-y))
                      ((= (rem what-to-draw 4) 2)
                       (draw-lines buffer width height scaled-x scaled-y))
                      (t
                        (do-msg "Hello!" :title "Message4you"))))))))
      (wait-complete-redraw)
      (setf *pixel-buffer-context* (make-instance 'px:pixel-buffer-context
                                                  :non-blocking-queue-maximum-size 16
                                                  :classic-frame sdl-frame
                                                  :buffer-width  +context-width+
                                                  :buffer-height +context-height+))
      (clear-sdl-window :force t)
      (when start-fire-demo
        (stop-animation)
        (setf *animation*
              (make-animation :thread
                              (make-thread #'draw-fire-thread)))))))

(defun approx-gaussian-random ()
  (/ (- 6
        (loop repeat 12 sum (random 1.0)))
     6))

(defun make-polygon-vertices ()
  (let* (#+sbcl (*random-state* (sb-kernel::seed-random-state 4))
         (vertices (loop for i from 0 below (* 2 pi) by (/ pi 16)
                        for radius = 50.0 then (+ 50.0 (* 50 (approx-gaussian-random)))
                        collect
                        (nodgui.vec2:vec2+ (nodgui.vec2:vec2 (* radius (cos i))
                                                             (* radius (sin i)))
                                           (nodgui.vec2:vec2 100f0 100f0)))))
    (px:make-polygon-vertex-array vertices)))

(defun make-textured-polygon-vertices ()
  (let* ((vertices (list (nodgui.vec2:vec2 300f0 0f0)
                         (nodgui.vec2:vec2 400f0 100f0)
                         (nodgui.vec2:vec2 400f0 200f0)
                         (nodgui.vec2:vec2 300f0 200f0)))
         (texels   (list (nodgui.vec2:vec2 0f0 0f0)
                         (nodgui.vec2:vec2 2f0 0f0)
                         (nodgui.vec2:vec2 2f0 2f0)
                         (nodgui.vec2:vec2 0f0 2f0))))
    (values (px:make-polygon-vertex-array vertices)
            (px:make-polygon-texture-coordinates-array texels))))

(defun make-textured-polygon-vertices-2 ()
  (let* ((vertices (list (nodgui.vec2:vec2 300f0 100f0)
                         (nodgui.vec2:vec2 400f0 100f0)
                         (nodgui.vec2:vec2 400f0 200f0)
                         (nodgui.vec2:vec2 300f0 200f0)))
         (texels   (list (nodgui.vec2:vec2 0f0 0f0)
                         (nodgui.vec2:vec2 1f0 0f0)
                         (nodgui.vec2:vec2 1f0 1f0)
                         (nodgui.vec2:vec2 0f0 1f0))))
    (values (px:make-polygon-vertex-array vertices)
            (px:make-polygon-texture-coordinates-array texels))))

(defun translate-polygon-vertices (vertices offset)
  (map 'vector
       (lambda (a) (vec2:vec2+ a offset))
       vertices))

(defun make-star-polygon ()
  (let* ((vertices (list (vec2:vec2 154.4639 483.86707)
                         (vec2:vec2 128.5122 420)
                         (vec2:vec2 196.59258 377)
                         (vec2:vec2 122.62741 377)
                         (vec2:vec2 105.23359 300.13705)
                         (vec2:vec2 85.472305 377)
                         (vec2:vec2 6.6419525 377)
                         (vec2:vec2 68.393974 420)
                         (vec2:vec2 37.06797 477.7146)
                         (vec2:vec2 94.9941 431.60602))))
    (px:make-polygon-vertex-array vertices)))

(defun make-bulky-polygon ()
  (let* ((vertices (list (vec2:vec2 400 400)
                         (vec2:vec2 450 400)
                         (vec2:vec2 500 400)
                         (vec2:vec2 500 350)
                         (vec2:vec2 600 350)
                         (vec2:vec2 600 450)
                         (vec2:vec2 500 450)
                         (vec2:vec2 400 450))))
    (px:make-polygon-vertex-array vertices)))

(defun make-buggy-polygon ()
  (let* ((vertices  (list (vec2:vec2 187 197)
                          (vec2:vec2 188 221)
                          (vec2:vec2 167 231)
                          (vec2:vec2 152 242)
                          (vec2:vec2 131 248)
                          (vec2:vec2 111 238)
                          (vec2:vec2 108 214)
                          (vec2:vec2 103 197)
                          (vec2:vec2 102 177)
                          (vec2:vec2 106 150)
                          (vec2:vec2 132 150)
                          (vec2:vec2 152 150)
                          (vec2:vec2 173 156)
                          (vec2:vec2 183 176))))
    (px:make-polygon-vertex-array vertices)))

(defun spline-control-points ()
  (list (vec2:vec2 -100 -100)
        (vec2:vec2  300 -100)
        (vec2:vec2    0    0)
        (vec2:vec2  300   10)
        (vec2:vec2   50   50)
        (vec2:vec2   100  -100)
        (vec2:vec2   200  300)))

(defun demo-pixel-buffer ()
  (px:init-font-system)
  (let* ((sdl-context      nil)
         (scaling          1f0)
         (rotation         0f0)
         (translating-x    0f0)
         (translating-y    0f0)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (font-path        (asdf:system-relative-pathname 'sdl2-ttf-examples
                                                          "examples/PROBE_10PX_OTF.otf"))
         (font             (px:open-font font-path 20))
         (polygon-vertices (make-polygon-vertices))
         (polygon-color    (pixmap:assemble-color 255 0 255 255)))
    (multiple-value-bind (textured-polygon-vertices textured-polygon-texture-coords)
        (make-textured-polygon-vertices)
      (multiple-value-bind (textured-polygon-vertices-2 textured-polygon-texture-coords-2)
          (make-textured-polygon-vertices-2)
      (flet ((make-button (master label callback)
               (make-instance 'button
                              :master  master
                              :text    label
                              :command callback))
             (update-info (label)
               (setf (text label)
                     (format nil
                             "rotation: ~,1f° scaling: ~a translate x: ~a translate y: ~a"
                             rotation scaling translating-x translating-y)))
             (draw ()
               (ctx:push-for-rendering sdl-context
                                       (lambda (dt)
                                         (declare (ignore dt))
                                         (px:clear-buffer context-buffer
                                                          context-width
                                                          context-height
                                                          100 100 100)
                                         (px:draw-text context-buffer
                                                       context-width
                                                       context-height
                                                       "Hello there!"
                                                       font
                                                       10
                                                       20
                                                       255
                                                       255
                                                       255
                                                       0)
                                         (let ((delta-pos (vec2:vec2 (to:d translating-x)
                                                                     (to:d translating-y))))
                                           (px:draw-polygon context-buffer
                                                            context-width
                                                            context-height
                                                            (translate-polygon-vertices polygon-vertices
                                                                                        delta-pos)
                                                            polygon-color)
                                           (px:draw-polygon context-buffer
                                                            context-width
                                                            context-height
                                                            (make-star-polygon)
                                                            polygon-color)
                                           (px:draw-texture-mapped-polygon context-buffer
                                                                           context-width
                                                                           context-height
                                                                           (translate-polygon-vertices textured-polygon-vertices
                                                                                                       delta-pos)

                                                                           textured-polygon-texture-coords
                                                                           *test-sprite*)
                                           (px:draw-texture-mapped-polygon context-buffer
                                                                           context-width
                                                                           context-height
                                                                           (translate-polygon-vertices textured-polygon-vertices-2
                                                                                                       (vec2:vec2+ delta-pos
                                                                                                                   (vec2:vec2 0f0 200f0)))

                                                                           textured-polygon-texture-coords-2
                                                                           *test-sprite*)
                                           (px:draw-polygon context-buffer
                                                            context-width
                                                            context-height
                                                            (make-bulky-polygon)
                                                            polygon-color)
                                           (px:draw-polygon context-buffer
                                                            context-width
                                                            context-height
                                                            (make-buggy-polygon)
                                                            polygon-color))
                                         (px:draw-spline (spline-control-points)
                                                         context-buffer
                                                         context-width
                                                         context-height
                                                         0 255 0 255)
                                         (let ((px:*blending-function* (px:make-blending-fn-replace-with-transparent-color 0 0 0)))
                                           (px:blit-transform (nodgui.pixmap:bits   *bell-sprite*)
                                                              (nodgui.pixmap:width  *bell-sprite*)
                                                              (nodgui.pixmap:height *bell-sprite*)
                                                              context-buffer
                                                              context-width
                                                              context-height
                                                              0
                                                              0
                                                              (truncate (/ context-height 2))
                                                              (truncate (/ context-width 2))
                                                              (nodgui.pixmap:height *bell-sprite*)
                                                              (nodgui.pixmap:width  *bell-sprite*)
                                                              rotation
                                                              scaling
                                                              scaling
                                                              (truncate (/ (nodgui.pixmap:width *bell-sprite*)
                                                                           2))
                                                              (truncate (/ (nodgui.pixmap:height *bell-sprite*)
                                                                           2))
                                                              translating-x
                                                              translating-y))))))
        (with-nodgui ()
          (let* ((sdl-frame         (ctx:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
                 (info              (make-instance 'label))
                 (buttons-frame     (make-instance 'nodgui:frame
                                                   :borderwidth 2
                                                   :relief :groove))
                 (quit-frame        (make-instance 'nodgui:frame
                                                   :master buttons-frame))
                 (button-rotate-cw  (make-button buttons-frame
                                                 "rotate clockwise"
                                                 (lambda ()
                                                   (incf rotation 10f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-rotate-ccw (make-button buttons-frame
                                                 "rotate counterclockwise"
                                                 (lambda ()
                                                   (incf rotation -10f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-enlarge    (make-button buttons-frame
                                                 "enlarge"
                                                 (lambda ()
                                                   (incf scaling 0.5f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-shrink    (make-button buttons-frame
                                                "shrink"
                                                (lambda ()
                                                  (incf scaling -0.5f0)
                                                  (update-info info)
                                                  (draw))))
                 (button-move-left  (make-button buttons-frame
                                                 "move left"
                                                 (lambda ()
                                                   (incf translating-x -5f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-move-right (make-button buttons-frame
                                                 "move right"
                                                 (lambda ()
                                                   (incf translating-x 5f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-move-up    (make-button buttons-frame
                                                 "move up"
                                                 (lambda ()
                                                   (incf translating-y -5f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-move-down  (make-button buttons-frame
                                                 "move down"
                                                 (lambda ()
                                                   (incf translating-y 5f0)
                                                   (update-info info)
                                                   (draw))))
                 (button-quit      (make-instance 'button
                                                  :master  quit-frame
                                                  :text    "quit"
                                                  :command (lambda ()
                                                             (stop-animation)
                                                             (px:close-font font)
                                                             (px:terminate-font-system)
                                                             (ctx:quit-sdl sdl-context)
                                                             (exit-nodgui)))))
            (grid info               0 0)
            (grid sdl-frame          1 0)
            (grid buttons-frame      1 1 :sticky :news)
            (grid button-rotate-cw   0 0 :sticky :nw)
            (grid button-rotate-ccw  1 0 :sticky :nw)
            (grid button-enlarge     2 0 :sticky :nw)
            (grid button-shrink      3 0 :sticky :nw)
            (grid button-move-up     4 0 :sticky :nw)
            (grid button-move-down   5 0 :sticky :nw)
            (grid button-move-left   6 0 :sticky :nw)
            (grid button-move-right  7 0 :sticky :nw)
            (grid quit-frame         8 0 :sticky :s)
            (grid button-quit        0 0 :sticky :s)
            (grid-columnconfigure (root-toplevel) :all :weight 1)
            (grid-rowconfigure    (root-toplevel) :all :weight 1)
            (grid-rowconfigure    buttons-frame 8 :weight 1)
            (grid-columnconfigure buttons-frame :all :weight 1)
            (update-info info)
            (wait-complete-redraw)
            (setf sdl-context (make-instance 'px:pixel-buffer-context
                                             :event-loop-type :serving
                                             :classic-frame   sdl-frame
                                             :buffer-width    +sdl-frame-width+
                                             :buffer-height   +sdl-frame-height+))
            (setf context-buffer (px:buffer sdl-context))
            (setf context-width  (ctx:width  sdl-context))
            (setf context-height (ctx:height sdl-context))
            (draw))))))))

(defun test-poly ()
  (let* ((sdl-context      nil)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (polygon-vertices (px:make-polygon-vertex-array (list (vec2:vec2 0 0)
                                                               (vec2:vec2 10 0)
                                                               (vec2:vec2 10 10)
                                                               (vec2:vec2 0 10))))

         (polygon-color    (pixmap:assemble-color 255 0 255 255)))
    (flet ((draw ()
             (ctx:push-for-rendering sdl-context
                                     (lambda (dt)
                                       (declare (ignore dt))
                                       (px:clear-buffer context-buffer
                                                        context-width
                                                        context-height
                                                        0 0 0)
                                       (px:draw-polygon context-buffer
                                                        context-width
                                                        context-height
                                                        polygon-vertices
                                                        polygon-color)))))
      (with-nodgui ()
        (let* ((sdl-frame        (ctx:make-sdl-frame 400 400))
               (button-quit      (make-instance 'button
                                                :text    "quit"
                                                :command (lambda ()
                                                           (ctx:quit-sdl sdl-context)
                                                           (exit-nodgui)))))
          (grid sdl-frame          1 0)
          (grid button-quit        2 0 :sticky :s)
          (wait-complete-redraw)
          (setf sdl-context (make-instance 'px:pixel-buffer-context
                                           :event-loop-type :serving
                                           :classic-frame   sdl-frame
                                           :buffer-width    10
                                           :buffer-height   10))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (ctx:width  sdl-context))
          (setf context-height (ctx:height sdl-context))
          (draw))))))

(a:define-constant +texture-vertical-strips+ "iVBORw0KGgoAAAANSUhEUgAAAAQAAAAECAYAAACp8Z5+AAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAdnJLH8AAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAABlJREFUCNdjePf/wH8GBgYGGM3EgAYICwAAxM8HX7Z6mmoAAAAASUVORK5CYII="
  :test #'string=)

(a:define-constant +texture-horizontal-strips+ "iVBORw0KGgoAAAANSUhEUgAAAAQAAAAECAYAAACp8Z5+AAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAdnJLH8AAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAABNJREFUCNdjPPD/3X8GfICRoAoAZDkHW6rgqKsAAAAASUVORK5CYII="
  :test #'string=)

(defun test-multi-texture-polygon ()
  (let* ((sdl-context      nil)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (polygon-vertices (px:make-polygon-vertex-array (list (vec2:vec2 0 0)
                                                               (vec2:vec2 10 0)
                                                               (vec2:vec2 10 10)
                                                               (vec2:vec2 0 10))))
         (polygon-tex-coordinates (px:make-polygon-texture-coordinates-array
                                   (list (vec2:vec2 0 0)
                                         (vec2:vec2 1 0)
                                         (vec2:vec2 1 1)
                                         (vec2:vec2 0 1))))
         (textures                (list (load-encoded-sprite +texture-vertical-strips+)
                                        (load-encoded-sprite +texture-horizontal-strips+))))
    (flet ((draw ()
             (ctx:push-for-rendering sdl-context
                                     (lambda (dt)
                                       (declare (ignore dt))
                                       (px:clear-buffer context-buffer
                                                        context-width
                                                        context-height
                                                        255 0 255)
                                       (px:draw-multi-texture-mapped-polygon context-buffer
                                                                             context-width
                                                                             context-height
                                                                             polygon-vertices
                                                                             polygon-tex-coordinates
                                                                             textures)))))
      (with-nodgui ()
        (let* ((sdl-frame   (ctx:make-sdl-frame 600 600))
               (button-quit (make-instance 'button
                                           :text    "quit"
                                           :command (lambda ()
                                                      (ctx:quit-sdl sdl-context)
                                                      (exit-nodgui)))))
          (grid sdl-frame 1 0)
          (grid button-quit 2 0 :sticky :s)
          (wait-complete-redraw)
          (setf sdl-context (make-instance 'px:pixel-buffer-context
                                           :event-loop-type :serving
                                           :classic-frame   sdl-frame
                                           :buffer-width    10
                                           :buffer-height   10))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (ctx:width  sdl-context))
          (setf context-height (ctx:height sdl-context))
          (draw))))))

(defun test-clipped-multi-texture-polygon ()
  (let* ((sdl-context      nil)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (polygon-vertices (px:make-polygon-vertex-array (list (vec2:vec2 0 0)
                                                               (vec2:vec2 600 0)
                                                               (vec2:vec2 600 600)
                                                               (vec2:vec2 0 600))))
         (polygon-tex-coordinates (px:make-polygon-texture-coordinates-array
                                   (list (vec2:vec2 0 0)
                                         (vec2:vec2 1 0)
                                         (vec2:vec2 1 1)
                                         (vec2:vec2 0 1))))
         (textures                (list (load-encoded-sprite +texture-vertical-strips+)
                                        (load-encoded-sprite +texture-horizontal-strips+))))
    (flet ((draw ()
             (ctx:push-for-rendering sdl-context
                                     (lambda (dt)
                                       (declare (ignore dt))
                                       (px:clear-buffer context-buffer
                                                        context-width
                                                        context-height
                                                        255 0 255)
                                       (let ((aabb-clipping (px:make-iaabb2 100 200 125 235)))
                                         (px:draw-multi-texture-mapped-polygon context-buffer
                                                                               context-width
                                                                               context-height
                                                                               polygon-vertices
                                                                               polygon-tex-coordinates
                                                                               textures
                                                                               :aabb-clipping
                                                                               aabb-clipping))))))
      (with-nodgui ()
        (let* ((sdl-frame   (ctx:make-sdl-frame 600 600))
               (button-quit (make-instance 'button
                                           :text    "quit"
                                           :command (lambda ()
                                                      (ctx:quit-sdl sdl-context)
                                                      (exit-nodgui)))))
          (grid sdl-frame 1 0)
          (grid button-quit 2 0 :sticky :s)
          (wait-complete-redraw)
          (setf sdl-context (make-instance 'px:pixel-buffer-context
                                           :event-loop-type :serving
                                           :classic-frame   sdl-frame
                                           :buffer-width    600
                                           :buffer-height   600))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (ctx:width  sdl-context))
          (setf context-height (ctx:height sdl-context))
          (draw))))))

(defun multi-texture-shader-wrap-replace (s-tex
                                          t-tex
                                          textures
                                          texture-width
                                          texture-height
                                          buffer
                                          buffer-width
                                          buffer-height
                                          x-buffer
                                          y-buffer)
  #.nodgui.config:default-optimization
  (declare (ignore buffer-height))
  (declare (to::desired-type texture-width texture-height))
  (declare (fixnum buffer-width buffer-height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (list textures))
  (let* ((wrapped-s (px:texture-border-wrap s-tex))
         (wrapped-t (px:texture-border-wrap t-tex))
         (pixmap-x (truncate (to:d* wrapped-s
                                    (1- texture-width))))
         (pixmap-y (truncate (to:d* wrapped-t
                                    (1- texture-height)))))
    (declare (fixnum pixmap-x pixmap-y))
    (declare (to::desired-type wrapped-s wrapped-t))
    (declare (dynamic-extent wrapped-s wrapped-t pixmap-x pixmap-y))
    (let ((pixel-out (pixmap:assemble-color 0 0 0 0)))
      (declare ((unsigned-byte 32) pixel-out))
      (loop for texture in textures do
        (let ((pixel (px:pixel@ texture
                                (truncate texture-width)
                                pixmap-x
                                pixmap-y)))
          (declare ((unsigned-byte 32) pixel))
          (declare (dynamic-extent pixel))
          (setf pixel-out pixel)))
      (px:set-pixel-color@ buffer buffer-width x-buffer y-buffer pixel-out))))

(defun profile-clipped-multi-texture-polygon ()
  (setf px:*multi-texture-shader* #'multi-texture-shader-wrap-replace)
  (let* ((sdl-context      nil)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (polygon-vertices (px:make-polygon-vertex-array (list (vec2:vec2 0 0)
                                                               (vec2:vec2 600 0)
                                                               (vec2:vec2 600 600)
                                                               (vec2:vec2 0 600))))
         (polygon-tex-coordinates (px:make-polygon-texture-coordinates-array
                                   (list (vec2:vec2 0 0)
                                         (vec2:vec2 1 0)
                                         (vec2:vec2 1 1)
                                         (vec2:vec2 0 1))))
         (textures                (list (load-encoded-sprite +texture-vertical-strips+)
                                        (load-encoded-sprite +texture-horizontal-strips+)
                                        (load-encoded-sprite +texture-vertical-strips+)
                                        (load-encoded-sprite +texture-horizontal-strips+)
                                        (load-encoded-sprite +texture-vertical-strips+)
                                        (load-encoded-sprite +texture-horizontal-strips+)))

         (aabbs-clipping (list (px:make-iaabb2 0 0   600 100)
                               (px:make-iaabb2 0 100 600 200)
                               (px:make-iaabb2 0 200 600 300)
                               (px:make-iaabb2 0 300 600 400)
                               (px:make-iaabb2 0 400 600 500)
                               (px:make-iaabb2 0 500 600 600))))
    (flet ((draw-multi ()
               (loop while (not (stop-drawing-thread-p *animation*)) do
                 (ctx:sync *pixel-buffer-context*)
                 (ctx:push-for-updating *pixel-buffer-context*
                                        (lambda (dt)
                                          (declare (ignore dt))
                                          t)
                                        :force-push nil)
                 (ctx:push-for-rendering *pixel-buffer-context*
                                         (lambda (dt)
                                           (declare (ignore dt))
                                           (time
                                           (px:draw-multi-texture-mapped-polygon context-buffer
                                                                                 context-width
                                                                                 context-height
                                                                                 polygon-vertices
                                                                                 polygon-tex-coordinates
                                                                                 textures))))))
       (draw-single ()
         (loop while (not (stop-drawing-thread-p *animation*)) do
           (ctx:sync *pixel-buffer-context*)
           (ctx:push-for-updating *pixel-buffer-context*
                                  (lambda (dt)
                                    (declare (ignore dt))
                                    t)
                                  :force-push nil)
            (ctx:push-for-rendering *pixel-buffer-context*
                                    (lambda (dt)
                                      (declare (ignore dt))
                                      (time
                                       (loop for texture in textures
                                             for clipping in aabbs-clipping do
                                        (px:draw-texture-mapped-polygon context-buffer
                                                                        context-width
                                                                        context-height
                                                                        polygon-vertices
                                                                        polygon-tex-coordinates
                                                                        texture
                                                                        :aabb-clipping
                                                                        clipping))))))))
      (with-nodgui ()
        (let* ((sdl-frame   (ctx:make-sdl-frame 600 600))
               (button-quit (make-instance 'button
                                           :text    "quit"
                                           :command (lambda ()
                                                      (stop-animation)
                                                      (ctx:quit-sdl *pixel-buffer-context*)
                                                      (exit-nodgui))))
               (button-change (make-instance 'button
                                             :text    "to single"
                                             :command
                                             (lambda ()
                                               (stop-animation)
                                               (setf *animation* (make-animation))
                                               (setf (animation-thread *animation*)
                                                     (make-thread #'draw-single
                                                                  :name "single"))))))
          (grid sdl-frame 1 0)
          (grid button-quit 2 0 :sticky :s)
          (grid button-change 3 0 :sticky :s)
          (wait-complete-redraw)
          (setf sdl-context (make-instance 'px:pixel-buffer-context
                                           :minimum-delta-t (ctx:fps->delta-t 30)
                                           :non-blocking-queue-maximum-size 16
                                           :event-loop-type :polling
                                           :classic-frame   sdl-frame
                                           :buffer-width    600
                                           :buffer-height   600))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (ctx:width  sdl-context))
          (setf context-height (ctx:height sdl-context))
          (setf *pixel-buffer-context*  sdl-context)
          (stop-animation)
          (setf *animation*
                (make-animation :thread
                                (make-thread #'draw-multi))))))))

(defun circular-wave-texture (&optional (width 4096) (height 4096))
  (let ((pixmap (pixmap::make-pixmap width height))
        (center (vec2:vec2 (to:d 0.5) (to:d 0.5))))
    (pixmap::loop-matrix (pixmap x y)
      (let* ((normalized  (vec2:vec2 (to:d (/ x width))
                                     (to:d (/ y height))))
             (dist        (vec2:vec2- normalized center))
             (color-level (to:dsin (to:d* (to:d 24.0) (vec2:vec2-length dist))))
             (color       (truncate (to:d* (to:d/ (to:d+ (to:d 1.0)
                                                         color-level)
                                                  (to:d 2.0))
                                           (to:d 255)))))
        (setf (pixmap:pixel@ pixmap x y)
              (nodgui.ubvec4:ubvec4 color color color 255))))
    (pixmap:sync-data-to-bits pixmap)
    pixmap))

;; single thread: ~0.43s
;; 4 threads:     ~0.11s
;; 12 threads:    ~0.07s
(defun profile-multi-texture-polygon ()
  (setf px:*multi-texture-shader* #'multi-texture-shader-wrap-replace)
  (let* ((sdl-context      nil)
         (context-buffer   nil)
         (context-width    nil)
         (context-height   nil)
         (texture-size     4096)
         (polygon-vertices (px:make-polygon-vertex-array (list (vec2:vec2 0 0)
                                                               (vec2:vec2 texture-size 0)
                                                               (vec2:vec2 texture-size texture-size)
                                                               (vec2:vec2 0 texture-size))))
         (polygon-tex-coordinates (px:make-polygon-texture-coordinates-array
                                   (list (vec2:vec2 0 0)
                                         (vec2:vec2 1 0)
                                         (vec2:vec2 1 1)
                                         (vec2:vec2 0 1))))
         (textures                (progn
                                    (format t "generating texture~%")
                                    (prog1
                                        (list (circular-wave-texture))
                                      (format t "texture generated~%")))))
    (flet ((draw-multi ()
               (loop while (not (stop-drawing-thread-p *animation*)) do
                 (ctx:sync *pixel-buffer-context*)
                 (ctx:push-for-updating *pixel-buffer-context*
                                        (lambda (dt)
                                          (declare (ignore dt))
                                          t)
                                        :force-push nil)
                 (ctx:push-for-rendering *pixel-buffer-context*
                                         (lambda (dt)
                                           (declare (ignore dt))
                                           (time
                                            (px:draw-multi-texture-mapped-polygon context-buffer
                                                                                 context-width
                                                                                 context-height
                                                                                 polygon-vertices
                                                                                 polygon-tex-coordinates
                                                                                 textures)))))))
      (with-nodgui ()
        (let* ((sdl-frame   (ctx:make-sdl-frame 1024 1024))
               (button-quit (make-instance 'button
                                           :text    "quit"
                                           :command (lambda ()
                                                      (stop-animation)
                                                      (ctx:quit-sdl *pixel-buffer-context*)
                                                      (exit-nodgui)))))
          (grid sdl-frame 1 0)
          (grid button-quit 1 0 :sticky :s)
          (wait-complete-redraw)
          (setf sdl-context (make-instance 'px:pixel-buffer-context
                                           :minimum-delta-t (ctx:fps->delta-t 30)
                                           :non-blocking-queue-maximum-size 16
                                           :event-loop-type :polling
                                           :classic-frame   sdl-frame
                                           :buffer-width    texture-size
                                           :buffer-height   texture-size))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (ctx:width  sdl-context))
          (setf context-height (ctx:height sdl-context))
          (setf *pixel-buffer-context*  sdl-context)
          (stop-animation)
          (setf *animation*
                (make-animation :thread
                                (make-thread #'draw-multi))))))))
