(in-package :nodgui.demo)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defparameter *sdl-context* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (a:define-constant +sin-lut-step-per-degrees+ 20 :test #'=)

  (a:define-constant +float-sin-lut-step-per-degrees+ (to:d +sin-lut-step-per-degrees+)
    :test #'=)

  (defun populate-sin-lut ()
    (let ((lut (make-fresh-array (* +sin-lut-step-per-degrees+ 360)
                                 0.01
                                 'to::desired-type
                                 t)))
      (loop for angle from 0.0 below 359.9 by (/ 1.0 +sin-lut-step-per-degrees+)
            for i from 0
            do
               (setf (aref lut i) (to:dsin (to:degree->radians angle))))
      lut))

  (a:define-constant +sin-lut+ (populate-sin-lut) :test #'equalp))

(a:define-constant +1/2pi+ (to:d (/ 1 to:+2pi+)) :test #'=)

(definline sin-lut (angle)
  (declare (to::desired-type angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (actual-angle     (if (< normalized-angle 0)
                               (to:d+ normalized-angle to:+2pi+)
                               normalized-angle))
         (index            (truncate (to:d* +float-sin-lut-step-per-degrees+
                                            (to:radians->degree actual-angle)))))
    (declare (dynamic-extent normalized-angle actual-angle index))
    (aref +sin-lut+ index)))

(definline sin-lut-fire (angle)
  (declare (to::desired-type angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (index            (to:f* +sin-lut-step-per-degrees+
                                  (the fixnum (truncate (to:radians->degree normalized-angle))))))
    (declare (dynamic-extent normalized-angle index))
    (aref +sin-lut+ index)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (a:define-constant +cos-lut-step-per-degrees+ 20 :test #'=)

  (a:define-constant +float-cos-lut-step-per-degrees+ (to:d +cos-lut-step-per-degrees+)
    :test #'=)

  (defun populate-cos-lut ()
    (let ((lut (make-fresh-array (* +cos-lut-step-per-degrees+ 360)
                                 0.01
                                 'to::desired-type
                                 t)))
      (loop for angle from 0.0 below 359.9 by (/ 1.0 +cos-lut-step-per-degrees+)
            for i from 0
            do
               (setf (aref lut i) (to:dcos (to:degree->radians angle))))
      lut))

  (a:define-constant +cos-lut+ (populate-cos-lut) :test #'equalp))

(a:define-constant +1/2pi+ (to:d (/ 1 to:+2pi+)) :test #'=)

(definline cos-lut-positive-angle (angle)
  (declare (to::desired-type angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((normalized-angle (to:d* to:+2pi+ (nth-value 1 (truncate (to:d* angle +1/2pi+)))))
         (index            (truncate (to:d* +float-cos-lut-step-per-degrees+
                                            (to:radians->degree normalized-angle)))))
    (declare (dynamic-extent normalized-angle normalized-angle index))
    (aref +cos-lut+ index)))

;; plasma

(definline wave->color (v)
  (declare (to::desired-type v))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (truncate (to:d* (to:d/ (to:d+ 1.0 (sin-lut (to:d* 5.0 v 3.141))) 2.0)
                   255.0)))

(defun horizontal-wave (x frequency phase tick)
  (declare (to::desired-type frequency x))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (sin-lut (to:d+ phase
                  (to:d* frequency
                         (to:d+ x tick)))))

(definline rotating-wave (x y frequency phase tick)
  (declare (to::desired-type frequency x phase tick))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((cosine    (cos-lut-positive-angle (to:d* 8.0 tick)))
         (sinus     (sin-lut (to:d* 8.0 tick)))
         (rotated-x (to:d- (to:d* x cosine)
                           (to:d* y sinus))))
    (declare (dynamic-extent cosine sinus rotated-x))
    (sin-lut (to:d+ phase (to:d* 2.0
                                 (to:dabs (sin-lut (to:d* 30.0 tick)))
                                 frequency
                                 rotated-x)))))

(definline circular-wave (x y frequency phase tick)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((translated-x (to:d+ (sin-lut (to:d* 6.0 tick))
                              (to:d- x 0.5)))
         (translated-y (to:d+ (sin-lut (to:d* 2.0 tick))
                              (to:d- y 0.5)))
         (dist         (to:dsqrt (to:d+ (to:dexpt translated-x 2.0)
                                        (to:dexpt translated-y 2.0)))))
    (declare (dynamic-extent translated-x translated-y dist))
    (sin-lut (to:d+ (to:d+ phase (to:d* -200.0 tick)) (to:d* frequency dist)))))

(definline normalize-coordinate (v max)
  (declare (fixnum v max))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d/ (to:d v) (to:d max)))

(definline plasma-value (x y frequency phase tick)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d+ (circular-wave x y frequency phase tick)
         (horizontal-wave x frequency phase tick)
         (rotating-wave x y frequency phase tick)))

(defun draw-plasma (buffer width height tick)
  (loop for i fixnum from 0 below width do
    (loop for j fixnum from 0 below height do
      (let ((color (wave->color (plasma-value (normalize-coordinate i width)
                                              (normalize-coordinate j height)
                                              8.0
                                              0.0
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
             (declare (optimize (speed 3) (debug 0) (safety 0)))
             (if ,cache
                 (pop ,cache)
                 (progn
                   (setf ,cache (loop repeat 100000 collect (random ,max)))
                   (,random-function-name)))))
         (defun ,(format-fn-symbol t "1-of-~a-passes" max) ()
           (declare (optimize (speed 3) (debug 0) (safety 0)))
           (= (rem (the fixnum
                        (,random-function-name))
                   ,max)
              0))))))

(generate-cached-random 2)

(generate-cached-random 3)

(generate-cached-random 50)

(generate-cached-random 1000)

(defun blur-kernel (buffer width height kernel-x kernel-y time shift-spike)
  (declare (fixnum shift-spike kernel-x kernel-y width height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((up                   (to:f+ kernel-y 1))
         (down                 (to:f- kernel-y 1))
         (shift-down-2         (to:f- kernel-y 2))
         (left                 (to:f- kernel-x 1))
         (shift-left-2         (to:f- kernel-x 2))
         (right                (to:f+ kernel-x 1))
         (shift-right-2        (to:f+ kernel-x 2))
         (black-dots-threshold (truncate (* 0.70 height)))
         (sum-red (to:f+ (the (unsigned-byte 8)
                              (pixmap:extract-red-component (px:pixel@ buffer width kernel-x up)))
                         (the (unsigned-byte 8)
                              (pixmap:extract-red-component (px:pixel@ buffer width kernel-x down)))
                         (the (unsigned-byte 8)
                              (pixmap:extract-red-component (px:pixel@ buffer width left kernel-y)))
                         (the (unsigned-byte 8)
                              (pixmap:extract-red-component (px:pixel@ buffer width right kernel-y)))))
         (sum-green (to:f+ (the (unsigned-byte 8)
                                (pixmap:extract-green-component (px:pixel@ buffer width kernel-x up)))
                           (the (unsigned-byte 8)
                                (pixmap:extract-green-component (px:pixel@ buffer width kernel-x down)))
                           (the (unsigned-byte 8)
                                (pixmap:extract-green-component (px:pixel@ buffer width left kernel-y)))
                           (the (unsigned-byte 8)
                                (pixmap:extract-green-component (px:pixel@ buffer width right kernel-y)))))
         (sum-blue (to:f+ (the (unsigned-byte 8)
                               (pixmap:extract-blue-component (px:pixel@ buffer width kernel-x up)))
                          (the (unsigned-byte 8)
                               (pixmap:extract-blue-component (px:pixel@ buffer width kernel-x down)))
                          (the (unsigned-byte 8)
                               (pixmap:extract-blue-component (px:pixel@ buffer width left kernel-y)))
                          (the (unsigned-byte 8)
                               (pixmap:extract-blue-component (px:pixel@ buffer width right kernel-y)))))
         (r-average (ash sum-red -2))
         (g-average (ash sum-green -2))
         (b-average (ash sum-blue -2)))
    (declare (dynamic-extent up
                             down
                             shift-down-2
                             left
                             shift-left-2
                             right
                             shift-right-2
                             black-dots-threshold
                             sum-red sum-green sum-blue))
    (cond
      ((and (not (< 0.0
		    (to:dabs (to:d* 10.0 (sin-lut-fire (to:d* 500.0 time))))
		    9.995))
	    (< (rem kernel-x shift-spike) 10))
       (px:set-pixel@ buffer
                      width
		      (cond
                        ((1-of-2-passes)
                         shift-left-2)
                        ((1-of-2-passes)
                         shift-right-2)
                        (t kernel-x))
		      (if (1-of-2-passes)
                          kernel-y
                          down)
                      r-average
                      g-average
                      b-average
                      255))
      ((and (to:f> kernel-y black-dots-threshold)
            (or (and (< r-average 200)
                     (1-of-50-passes))
                (1-of-1000-passes)))
       (px:set-pixel@ buffer
                      width
                      kernel-x
                      shift-down-2
                      10
                      10
                      10
                      255))
      (t
       (px:set-pixel@ buffer
                      width
                      (cond
                        ((1-of-2-passes)
                         left)
                        ((1-of-2-passes)
                         right)
                        (t kernel-x))
                      (if (1-of-3-passes)
                          down
                          kernel-y)
                      r-average
                      g-average
                      b-average
                      255)))))

(defun blur (buffer width height time)
  (declare (fixnum width height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (to::desired-type time))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((shift-spike (truncate (abs (to:d+ 20.0
                                           (to:d* 10.0
                                                  (sin-lut-fire (to:d* 1000000.0
                                                                       (to:d+ 10.0
                                                                              time)))))))))
    (declare (dynamic-extent shift-spike))
    (loop for i fixnum from 1 below (1- width) do
      (loop for j fixnum from 1 below (1- height) do
	(blur-kernel buffer width height i j time shift-spike)))))

(defun reinforce-fire (buffer width height howmany)
  (declare (fixnum width height howmany))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
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

(defun draw-fire (buffer width height howmany-seed time)
  (reinforce-fire buffer width height howmany-seed)
  (blur buffer width height time))

;;;; end fire ;;;

;;;; blit ;;;;

(defun make-blitting-rectangle (width height)
  (declare (fixnum width height))
  (let* ((rectangle-width  (+ 20 (random (truncate (- (* width  1/4) 40)))))
         (rectangle-height (+ 20 (random (truncate (- (* height 1/4) 40)))))
         (rectangle        (pixmap:make-buffer rectangle-width rectangle-height))
         (destination-x    (+ (truncate (* 3/4
                                           (random width)))
                              20))
         (destination-y     (+ (truncate (* 3/4
                                            (random height)))
                               20)))
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
                   (width  px:width)
                   (height px:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (px:sync *sdl-context*)
        (px:push-for-rendering *sdl-context*
                                 (lambda (dt)
                                   (declare (fixnum dt))
                                   (declare (optimize (speed 3) (debug 0)))
                                   (draw-plasma buffer width height tick)
                                   (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))))
      (format t "STOP!~%"))))

(defun clear-sdl-window (&key (context *sdl-context*) (force nil))
  (with-accessors ((buffer px:buffer)
                   (width  px:width)
                   (height px:height)) context
    (px:push-for-rendering context
                           (lambda (dt)
                             (declare (ignore dt))
                             (px:clear-buffer buffer width height 0 0 0))
                           :force-push force)))

(defun draw-fire-thread ()
  (with-accessors ((buffer px:buffer)
                   (width  px:width)
                   (height px:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (px:sync *sdl-context*)
        (px:push-for-rendering *sdl-context*
                                 (lambda (dt)
                                   (declare (fixnum dt))
                                   (draw-fire buffer width height 500 tick)
                                   (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))))
      (format t "STOP FIRE!~%"))))

(defun draw-rectangles-thread ()
  (with-accessors ((buffer px:buffer)
                   (width  px:width)
                   (height px:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      ;; I should  use :force-push t  in this function call  to ensure
      ;; the  event  of clearing  the  buffer  is not  discarded,  but
      ;; instead i prefer to leav the  key parameter as nil because it
      ;; could  give a  nice transition,  if  the queue  is filled  by
      ;; leftover of plasma rendering events
      (clear-sdl-window)
      (let ((rectangles (loop repeat 1000
                              collect
                              (multiple-value-list (make-blitting-rectangle width
                                                                            height)))))
        (mapcar (lambda (rectangle)
                  (px:push-for-rendering *sdl-context*
                                   (lambda (dt)
                                     (declare (fixnum dt))
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
                                                    0
                                                    0
                                                    y
                                                    x
                                                    rectangle-height
                                                    rectangle-width))
                                       (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt))))))
                                   :force-push t))
                rectangles)
        (format t "STOP RECTANGLES!~%")))))

(defun load-bell-sprite ()
  (let ((px (make-instance 'nodgui.pixmap:png)))
    (nodgui.pixmap:load-from-vector px (nodgui.base64:decode nodgui.demo::+bell-icon+))
    px))

(defparameter *bell-sprite* (load-bell-sprite))

;; 10px
(a:define-constant +test-sprite+
   "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAAIUlEQVQY02P8z4ACGFH5TAx4AU2lGRkYUByzatXqweI0ABCcBRCKObYxAAAAAElFTkSuQmCC"
  :test #'string=)

;; 2px
;; (a:define-constant +test-sprite+
;;   "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAFklEQVQI12P4z8DA8J+BkYHh/6pVqwEd+AT+DoCjJQAAAABJRU5ErkJggg=="
;;   :test #'string=)

(defun load-test-sprite ()
  (let ((px (make-instance 'nodgui.pixmap:png)))
    (nodgui.pixmap:load-from-vector px (nodgui.base64:decode +test-sprite+))
    px))

(defparameter *test-sprite* (load-test-sprite))

(defun draw-test-sprite (buffer width height x y)
  (px:push-for-rendering *sdl-context*
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
  (px:push-for-rendering *sdl-context*
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
  (loop for degree from 0 below 360 by 2
        for color = 0 then (truncate (abs (* 255 (sin (* 12 (/ degree 360))))))
        do
           (let ((radius (min (to:d x)
                              (to:d y)
                              (to:d (- width x))
                              (to:d (- height y))
                              50.0)))
             (px:push-for-rendering *sdl-context*
                                      (let* ((current-color color)
                                             (actual-degree degree)
                                             (radians   (to:d/ (to:d* (to:d pi)
                                                                      (to:d actual-degree))
                                                               180.0))
                                             (current-x (to:f+ x
                                                               (truncate (to:d* radius (to:dcos radians)))))
                                             (current-y (to:f+ y
                                                               (truncate (* radius (sin radians))))))
                                        (lambda (dt)
                                          (declare (ignore dt))
                                          (px:draw-line buffer width
                                                          x y
                                                          current-x current-y
                                                          255 0 current-color  255)))
                                      :force-push t))))

(defun stop-animation ()
  (when (and *animation*
             (bt:threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)
    (format t "anim ~a queue ~a~%" *animation* (px::queue *sdl-context*))))

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
           (sdl-frame     (px:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
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
                                           (setf *animation*
                                                 (make-animation :thread
                                                                 (make-thread #'draw-plasma-thread
                                                                              :name "plasma")))
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
                                           (setf *animation*
                                                 (make-animation :thread
                                                                 (make-thread #'draw-fire-thread)))
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
                                           (setf *animation*
                                                 (make-animation :thread
                                                                 (make-thread #'draw-rectangles-thread)))
                                           (format t "tk event returned~%"))))
           (button-quit  (make-instance 'button
                                        :master  quit-frame
                                        :text    "quit"
                                        :command (lambda ()
                                                   (stop-animation)
                                                   (px:quit-sdl *sdl-context*)
                                                   (exit-nodgui)))))
      (grid warning-label     0 0 :columnspan 2)
      (grid interaction-label 1 0 :columnspan 2)
      (grid sdl-frame         2 0)
      (grid buttons-frame     2 1 :sticky :nws)
      (grid buttons-label     1 0 :sticky :nw :padx 5 :pady 10)
      (grid radio-plasma      2 0 :sticky :nw :padx 5)
      (grid radio-fire        3 0 :sticky :nw :padx 5)
      (grid radio-rectangles  4 0 :sticky :nw :padx 5)
      (grid quit-frame        5 0 :sticky :wes)
      (grid button-quit       0 0 :sticky :s)
      (grid notice-label      1 0 :sticky :sw)
      (grid-columnconfigure (root-toplevel) :all :weight 1)
      (grid-rowconfigure    (root-toplevel) :all :weight 1)
      (grid-rowconfigure    buttons-frame 5 :weight 1)
      (grid-columnconfigure buttons-frame :all :weight 1)
      (bind sdl-frame
            #$<1>$
            (let ((what-to-draw 0))
              (lambda (event)
                (incf what-to-draw)
                (with-accessors ((buffer px:buffer)
                                 (width  px:width)
                                 (height px:height)) *sdl-context*
                  (let ((scaled-x (truncate (* (event-x event)
                                               (/ +context-width+
                                                  +sdl-frame-width+))))
                        (scaled-y (truncate (* (event-y event)
                                               (/ +context-height+
                                                  +sdl-frame-height+)))))
                    (cond
                      ((= (rem what-to-draw 3) 0)
                       (draw-bell-sprite buffer width height scaled-x scaled-y))
                      ((= (rem what-to-draw 3) 1)
                       (draw-test-sprite buffer width height scaled-x scaled-y))
                      (t
                       (draw-lines buffer width height scaled-x scaled-y))))))))
      (wait-complete-redraw)
      (setf *sdl-context* (make-instance 'px:context
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

(defun demo-pixel-buffer ()
  (let ((sdl-context nil)
        (scaling        1.0)
        (rotation       0.0)
        (translating-x  0)
        (translating-y  0)
        (context-buffer nil)
        (context-width   nil)
        (context-height  nil))
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
             (px:push-for-rendering sdl-context
                                    (lambda (dt)
                                      (declare (ignore dt))
                                      (px:clear-buffer context-buffer
                                                       context-width
                                                       context-height
                                                       0 0 0)
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
                                                         translating-y)))))
      (with-nodgui ()
        (let* ((sdl-frame         (px:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
               (info              (make-instance 'label))
               (buttons-frame     (make-instance 'nodgui:frame
                                                 :borderwidth 2
                                                 :relief :groove))
               (quit-frame        (make-instance 'nodgui:frame
                                                 :master buttons-frame))
               (button-rotate-cw  (make-button buttons-frame
                                               "rotate clockwise"
                                               (lambda ()
                                                 (incf rotation 10.0)
                                                 (update-info info)
                                                 (draw))))
               (button-rotate-ccw (make-button buttons-frame
                                               "rotate counterclockwise"
                                               (lambda ()
                                                 (incf rotation -10.0)
                                                 (update-info info)
                                                 (draw))))
               (button-enlarge    (make-button buttons-frame
                                               "enlarge"
                                               (lambda ()
                                                 (incf scaling 0.5)
                                                 (update-info info)
                                                 (draw))))
               (button-shrink    (make-button buttons-frame
                                              "shrink"
                                              (lambda ()
                                                (incf scaling -0.5)
                                                (update-info info)
                                                (draw))))
               (button-move-left  (make-button buttons-frame
                                              "move left"
                                              (lambda ()
                                                (incf translating-x -5)
                                                (update-info info)
                                                (draw))))
               (button-move-right (make-button buttons-frame
                                               "move right"
                                               (lambda ()
                                                 (incf translating-x 5)
                                                (update-info info)
                                                (draw))))
               (button-move-up    (make-button buttons-frame
                                               "move up"
                                               (lambda ()
                                                 (incf translating-y -5)
                                                 (update-info info)
                                                 (draw))))
               (button-move-down  (make-button buttons-frame
                                               "move down"
                                               (lambda ()
                                                 (incf translating-y 5)
                                                 (update-info info)
                                                 (draw))))
               (button-quit      (make-instance 'button
                                                :master  quit-frame
                                                :text    "quit"
                                                :command (lambda ()
                                                           (stop-animation)
                                                           (px:quit-sdl sdl-context)
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
          (setf sdl-context (make-instance 'px:context
                                           :event-loop-type :serving
                                           :classic-frame   sdl-frame
                                           :buffer-width    +sdl-frame-width+
                                           :buffer-height   +sdl-frame-height+))
          (setf context-buffer (px:buffer sdl-context))
          (setf context-width  (px:width  sdl-context))
          (setf context-height (px:height sdl-context))
          (draw))))))
