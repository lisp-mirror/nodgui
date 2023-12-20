(in-package :nodgui.demo)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defparameter *sdl-context* nil)

;; plasma

(defun wave->color (v)
  (declare (to::desired-type v))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (truncate (to:d* (to:d/ (to:d+ 1.0 (to:dsin (to:d* 5.0 v 3.141))) 2.0)
                   255.0)))

(defun horizontal-wave (x frequency phase tick)
  (declare (to::desired-type frequency x))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:dsin (to:d+ phase
                  (to:d* frequency
                         (to:d+ x tick)))))

(defun rotating-wave (x y frequency phase tick)
  (declare (to::desired-type frequency x phase tick))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((cosine    (to:dcos (to:d* 8.0 tick)))
         (sinus     (to:dsin (to:d* 8.0 tick)))
         (rotated-x (to:d- (to:d* x cosine)
                           (to:d* y sinus))))
    (to:dsin (to:d+ phase (to:d* 2.0
                                 (to:dabs (to:dsin (to:d* 30.0 tick)))
                                 frequency
                                 rotated-x)))))

(defun circular-wave (x y frequency phase tick)
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((translated-x (to:d+ (to:dsin (to:d* 6.0 tick))
                              (to:d- x 0.5)))
         (translated-y (to:d+ (to:dsin (to:d* 2.0 tick))
                              (to:d- y 0.5)))
         (dist         (to:dsqrt (to:d+ (to:dexpt translated-x 2.0)
                                        (to:dexpt translated-y 2.0)))))
    (to:dsin (to:d+ (to:d+ phase (to:d* -200.0 tick)) (to:d* frequency dist)))))

(defun normalize-coordinate (v max)
  (declare (fixnum v max))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d/ (to:d v) (to:d max)))

(defun plasma-value (x y frequency phase tick)
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d+ (circular-wave x y frequency phase tick)
         (horizontal-wave x frequency phase tick)
         (rotating-wave x y frequency phase tick)))

(defun draw-plasma (buffer width height tick)
  (loop for i from 0 below width do
    (loop for j from 0 below height do
      (let ((color (wave->color (plasma-value (normalize-coordinate i width)
                                              (normalize-coordinate j height)
                                              8.0
                                              0.0
                                              tick))))
        (sdlw:set-pixel@ buffer width i j color color color)))))
;;; end plasma ;;;

;; fire

(defun blur-kernel (buffer width height kernel-x kernel-y time)
  (declare (fixnum kernel-x kernel-y width height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((up    (to:frem (to:f+ kernel-y 1) height))
         (down  (max     (to:f- kernel-y 1) 0))
         (left  (if (= kernel-x 0)
                    (1- width)
                    (to:f- kernel-x 1)))
         (right (to:frem (to:f+ kernel-x 1) width))
         (sum-red (to:f+ (sdlw:extract-red-component (sdlw:pixel@ buffer width kernel-x up))
                         (sdlw:extract-red-component (sdlw:pixel@ buffer width kernel-x down))
                         (sdlw:extract-red-component (sdlw:pixel@ buffer width left kernel-y))
                         (sdlw:extract-red-component (sdlw:pixel@ buffer width right kernel-y))))
         (sum-green (to:f+ (sdlw:extract-green-component (sdlw:pixel@ buffer width kernel-x up))
                           (sdlw:extract-green-component (sdlw:pixel@ buffer width kernel-x down))
                           (sdlw:extract-green-component (sdlw:pixel@ buffer width left kernel-y))
                           (sdlw:extract-green-component (sdlw:pixel@ buffer width right kernel-y))))
         (sum-blue (to:f+ (sdlw:extract-blue-component (sdlw:pixel@ buffer width kernel-x up))
                          (sdlw:extract-blue-component (sdlw:pixel@ buffer width kernel-x down))
                          (sdlw:extract-blue-component (sdlw:pixel@ buffer width left kernel-y))
                          (sdlw:extract-blue-component (sdlw:pixel@ buffer width right kernel-y))))
         (r-average (ash sum-red -2))
         (g-average (ash sum-green -2))
         (b-average (ash sum-blue -2)))
    (if  (and (to:d> time .012)
              (to:f> kernel-y (truncate (* height 0.66)))
              (or (and (< r-average 80)
                       (= (random 60) 0))
                  (= (random 500) 0)))
        (sdlw:set-pixel@ buffer
                         width
                         (cond
                           ((= (random 2) 0)
                            (to:f- kernel-x 1))
                           ((= (random 2) 0)
                            (to:f+ kernel-x 1))
                           (t kernel-x))
                         (if (= (random 3) 0)
                             (1- kernel-y)
                             kernel-y)
                         20
                         20
                         0
                         255)
        (sdlw:set-pixel@ buffer
                         width
                         (cond
                           ((= (rem (random 2) 2) 0)
                            (to:f- kernel-x 1))
                           ((= (rem (random 2) 2) 0)
                            (to:f+ kernel-x 1))
                           (t kernel-x))
                         (if (= (rem (random 3) 2) 0)
                             (1- kernel-y)
                             kernel-y)
                         r-average
                         g-average
                         b-average
                         255))))

(defun blur (buffer width height time)
  (declare (fixnum width height))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for i from 1 below (1- width) do
    (loop for j from 1 below height do
      (blur-kernel buffer width height i j time))))

(defun reinforce-fire (buffer width height howmany)
  (declare (fixnum width height howmany))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop repeat howmany do
    (let* ((x         (random  width))
           (y         (1- (to:f- height (random 10))))
           (pixel     (sdlw:pixel@ buffer width x y))
           (new-pixel (sdlw:sum-pixels (sdlw:assemble-color 255 50 0 255)
                                       pixel)))
      (sdlw:set-pixel@ buffer
                       width
                       x
                       y
                       (sdlw:extract-red-component new-pixel)
                       (sdlw:extract-green-component new-pixel)
                       (sdlw:extract-blue-component new-pixel)))))

(defun draw-fire (buffer width height howmany-seed time)
  (reinforce-fire buffer width height howmany-seed)
  (blur buffer width height time))

;;;; end fire ;;;

;;;; blit ;;;;

(defun make-blitting-rectangle (width height)
  (declare (fixnum width height))
  (let* ((rectangle-width  (+ 20 (random (truncate (- (* width  1/4) 40)))))
         (rectangle-height (+ 20 (random (truncate (- (* height 1/4) 40)))))
         (rectangle        (sdlw:make-buffer rectangle-width rectangle-height))
         (destination-x    (+ (truncate (* 3/4
                                           (random width)))
                              20))
         (destination-y     (+ (truncate (* 3/4
                                            (random height)))
                               20)))
    (sdlw:clear-buffer rectangle
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
  (with-accessors ((buffer sdlw:buffer)
                   (width  sdlw:width)
                   (height sdlw:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (sdlw:sync *sdl-context*)
        (sdlw:push-for-rendering *sdl-context*
                                 (lambda (dt)
                                   (declare (fixnum dt))
                                   ;;;; (declare (optimize (speed 3) (debug 0)))
                                   (draw-plasma buffer width height tick)
                                   (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))))
      (format t "STOP!~%"))))

(defun clear-sdl-window (&key (force nil))
  (with-accessors ((buffer sdlw:buffer)
                   (width  sdlw:width)
                   (height sdlw:height)) *sdl-context*
    (sdlw:push-for-rendering *sdl-context*
                             (lambda (dt)
                               (declare (ignore dt))
                               (sdlw:clear-buffer buffer width height 0 0 0))
                             :force-push force)))

(defun draw-fire-thread ()
  (with-accessors ((buffer sdlw:buffer)
                   (width  sdlw:width)
                   (height sdlw:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      ;; I should  use :force-push t  in this function call  to ensure
      ;; the  event  of clearing  the  buffer  is not  discarded,  but
      ;; instead i prefer to leav the  key parameter as nil because it
      ;; could  give a  nice transition,  if  the queue  is filled  by
      ;; leftover of plasma rendering events
      (clear-sdl-window)
      (loop while (not (stop-drawing-thread-p *animation*)) do
        (sdlw:sync *sdl-context*)
        (sdlw:push-for-rendering *sdl-context*
                                 (lambda (dt)
                                   (declare (fixnum dt))
                                   (draw-fire buffer width height 500 tick)
                                   (setf tick (to:d+ tick (to:d* 1e-6 (to:d dt)))))))
      (format t "STOP FIRE!~%"))))

(defun draw-rectangles-thread ()
  (with-accessors ((buffer sdlw:buffer)
                   (width  sdlw:width)
                   (height sdlw:height)) *sdl-context*
    (let ((tick (to:d 0.0)))
      ;; I should  use :force-push t  in this function call  to ensure
      ;; the  event  of clearing  the  buffer  is not  discarded,  but
      ;; instead i prefer to leav the  key parameter as nil because it
      ;; could  give a  nice transition,  if  the queue  is filled  by
      ;; leftover of plasma rendering events
      (clear-sdl-window)
      (let ((rectangles (loop repeat 1000 collect
                                         (multiple-value-list (make-blitting-rectangle width height)))))
        (mapcar (lambda (rectangle)
                  (sdlw:push-for-rendering *sdl-context*
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
                                       (let ((sdlw:*blending-function* #'sdlw:blending-function-add))
                                         (sdlw:blit rectangle-buffer
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

(defun draw-bell-sprite (buffer width x y)
  (sdlw:push-for-rendering *sdl-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             (sdlw:blit (nodgui.pixmap:bits *bell-sprite*)
                                        (nodgui.pixmap:width *bell-sprite*)
                                        buffer
                                        width
                                        0
                                        0
                                        y
                                        x
                                        (nodgui.pixmap:height *bell-sprite*)
                                        (nodgui.pixmap:width *bell-sprite*)))
                           :force-push t))

(defun draw-lines (buffer width x y)
  (loop for degree from 0 below 360 by 2
        for color  from 0 by (truncate (* 255 (/ 2 360)))
        do
           (let ((radius 50.0))
             (sdlw:push-for-rendering *sdl-context*
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
                                          (sdlw:draw-line buffer width
                                                          x y
                                                          current-x current-y
                                                          255 0 current-color  255)))
                                      :force-push t))))

(defun stop-animation ()
  (when (and *animation*
             (bt:threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)
    (format t "anim ~a queue ~a~%" *animation* (sdlw::queue *sdl-context*))))

(defun demo-sdl ()
  (with-nodgui ()
    (let* ((warning-label (make-instance 'label
                                         :wraplength 800
                                         :text "WARNING: This animation may potentially trigger seizures for people with photosensitive epilepsy. Viewer discretion is advised."
                                         :font (font-create "serif" :size 20 :weight :bold)))
           (sdl-frame     (make-instance 'nodgui:classic-frame
                                         :width  800
                                         :height 600
                                         :background ""))
           (buttons-frame (make-instance 'nodgui:frame))
           (buttons-label (make-instance 'label
                                         :master buttons-frame
                                         :text "Select animation:"))
           (notice-label  (make-instance 'label
                                         :master buttons-frame
                                         :text "Please note that the button will responds with a delay because we need to wait the rendering queue to be empty"))
           (interaction-label (make-instance 'label
                                             :master nil
                                             :text "Click on the sdl window to draw a sprite, be aware that no bounds checking is done."))
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
                                        :master buttons-frame
                                        :text "quit"
                                        :command (lambda ()
                                                   (stop-animation)
                                                   (sdlw:quit-sdl *sdl-context*)
                                                   (exit-nodgui)))))
      (grid warning-label     0 0)
      (grid interaction-label 1 0)
      (grid sdl-frame         2 0)
      (grid buttons-frame     3 0 :sticky :sew)
      (grid buttons-label     0 0)
      (grid radio-plasma      0 1)
      (grid radio-fire        0 2)
      (grid radio-rectangles  0 3)
      (grid button-quit       0 4)
      (grid notice-label      0 5)
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)
      (bind sdl-frame
            #$<1>$
            (let ((what-to-draw 1))
              (lambda (event)
                (incf what-to-draw)
                (with-accessors ((buffer sdlw:buffer)
                                 (width  sdlw:width)
                                 (height sdlw:height)) *sdl-context*
                  (cond
                    ((= (rem what-to-draw 2) 0)
                     (draw-bell-sprite buffer width (event-x event) (event-y event)))
                    (t
                     (draw-lines  buffer width (event-x event) (event-y event))))))))
      (wait-complete-redraw)
      (setf *sdl-context* (make-instance 'sdlw:context
                                         :non-blocking-queue-maximum-size 16
                                         :classic-frame sdl-frame
                                         :buffer-width  800
                                         :buffer-height 600))
      (clear-sdl-window :force t))))
