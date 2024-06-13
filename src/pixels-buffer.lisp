;; This software is Copyright © cage

;; cage  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.pixels-canvas)

(a:define-constant +channels-number+ 4 :test #'=)

(defclass pixel-buffer-context (ctx:context)
  ((texture
    :initform nil
    :initarg :texture
    :accessor texture)
   (buffer
    :initform nil
    :initarg :buffer
    :accessor buffer)))

(defun make-texture (renderer width height)
  (sdl2:create-texture renderer :rgba8888 :streaming width height))

(u:definline set-pixel-color@ (buffer width x y color)
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((unsigned-byte 32) color))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (setf (aref buffer (to:f+ x (to:f* y width)))
        color))

(u:definline set-pixel@ (buffer width x y r g b &optional (a 255))
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((unsigned-byte 8) r g b a))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((color (pix:assemble-color r g b a)))
    (set-pixel-color@ buffer width x y color)))

(u:definline pixel@ (buffer width x y)
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 3) (safety 0)))
  (to:faref buffer (to:f+ x (the fixnum (to:f* y width)))))

(defmacro with-displace-pixel ((r g b a) pixel &body body)
  `(let ((,r (pix:extract-red-component   ,pixel))
         (,g (pix:extract-green-component ,pixel))
         (,b (pix:extract-blue-component  ,pixel))
         (,a (pix:extract-alpha-component ,pixel)))
     (declare ((unsigned-byte 8) ,r ,g ,b ,a))
     ,@body))

(u:definline color-channel-lerp (a b w)
  (declare ((unsigned-byte 8) a b w))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (ash (+ (* a w)
          (* b (- 256 w)))
       -8))

(defun color-lerp (a b w)
  (pix:assemble-color (color-channel-lerp (pix:extract-red-component a)
                                          (pix:extract-red-component b)
                                          w)
                      (color-channel-lerp (pix:extract-green-component a)
                                          (pix:extract-green-component b)
                                          w)
                      (color-channel-lerp (pix:extract-blue-component a)
                                          (pix:extract-blue-component b)
                                          w)
                      (color-channel-lerp (pix:extract-alpha-component a)
                                          (pix:extract-alpha-component b)
                                          w)))

(defun blending-function-combine (pixel-source pixel-destination)
  (declare (fixnum pixel-source pixel-destination))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-displace-pixel (r-source g-source b-source alpha-source)
                       pixel-source
    (with-displace-pixel (r-destination g-destination b-destination alpha-destination)
                         pixel-destination
      (declare (ignore alpha-destination))
      (pix:assemble-color (color-channel-lerp r-source r-destination alpha-source)
                          (color-channel-lerp g-source g-destination alpha-source)
                          (color-channel-lerp b-source b-destination alpha-source)
                          255))))

(defun blending-function-replace (pixel-source pixel-destination)
  (declare (fixnum pixel-source pixel-destination))
  (declare (ignore pixel-destination))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  pixel-source)

(defun saturate-byte (a)
  (declare (fixnum a))
  (min a #xfe))

(defun blending-function-add (pixel-source pixel-destination)
  (declare (fixnum pixel-source pixel-destination))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-displace-pixel (r-source g-source b-source alpha-source)
                       pixel-source
    (with-displace-pixel (r-destination g-destination b-destination alpha-destination)
                         pixel-destination
      (pix:assemble-color (saturate-byte (to:f+ r-source r-destination))
                          (saturate-byte (to:f+ g-source g-destination))
                          (saturate-byte (to:f+ b-source b-destination))
                          (saturate-byte (to:f+ alpha-source alpha-destination))))))

(defparameter *blending-function* #'blending-function-replace)

(defun blit-solid (buffer-source
                   buffer-source-width
                   buffer-destination
                   buffer-destination-width
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column)
  "Note: no bounds checking is done. This function supposed to be faster than `blit' when `*blending-function*' is bound to `#'blending-function-replace'."
  (declare ((simple-array (unsigned-byte 32)) buffer-source buffer-destination))
  (declare (fixnum buffer-source-width
                   buffer-destination-width
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((pixels-count (to:f- source-last-column source-column)))
    (if (and (= destination-column 0)
             (= source-last-column
                buffer-destination-width))
        (pix:copy-buffer-row buffer-source
                             buffer-destination
                             buffer-source-width
                             buffer-destination-width
                             source-column
                             source-row
                             destination-column
                             destination-row
                             pixels-count)
        (loop for from-row fixnum from source-row below source-last-row
          for to-row fixnum from destination-row do
            (pix:copy-buffer-row buffer-source
                                 buffer-destination
                                 buffer-source-width
                                 buffer-destination-width
                                 source-column
                                 from-row
                                 destination-column
                                 to-row
                                 pixels-count)))
    buffer-destination))

(defun blit-solid-ffi-pointer (buffer-source
                                     buffer-source-width
                                     buffer-destination
                                     buffer-destination-width
                                     source-row
                                     source-column
                                     destination-row
                                     destination-column
                                     source-last-row
                                     source-last-column)
  "Note: no bounds checking is done. This function supposed to be faster than `blit' when `*blending-function*' is bound to `#'blending-function-replace'."
  (declare ((simple-array (unsigned-byte 32)) buffer-destination))
  (declare (fixnum buffer-source-width
                   buffer-destination-width
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((pixels-count (to:f- source-last-column source-column)))
    (if (and (= destination-column 0)
             (= source-last-column
                buffer-destination-width))
        (pix::copy-buffer-row-ffi-pointer buffer-source
                                          buffer-destination
                                          buffer-source-width
                                          buffer-destination-width
                                          source-column
                                          source-row
                                          destination-column
                                          destination-row
                                          pixels-count)
        (loop for from-row fixnum from source-row below source-last-row
              for to-row fixnum from destination-row do
                (pix::copy-buffer-row-ffi-pointer buffer-source
                                                  buffer-destination
                                                  buffer-source-width
                                                  buffer-destination-width
                                                  source-column
                                                  from-row
                                                  destination-column
                                                  to-row
                                                  pixels-count)))
    buffer-destination))

(defun blit (buffer-source
             buffer-source-width
             buffer-destination
             buffer-destination-width
             source-row
             source-column
             destination-row
             destination-column
             source-last-row
             source-last-column)
  "Note: no bounds checking is done"
  (declare ((simple-array (unsigned-byte 32)) buffer-source buffer-destination))
  (declare (fixnum buffer-source-width
                   buffer-destination-width
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((copy-row (row-source row-destination)
           (declare (fixnum row-source row-destination))
           (declare (function *blending-function*))
           (declare (optimize (speed 3) (debug 0) (safety 0)))
           (loop for from-column fixnum from source-column below source-last-column
                 for to-column fixnum from 0 do
                   (let* ((color-destination (to:faref buffer-destination
                                               (to:f+ (to:f* row-destination
                                                        buffer-destination-width)
                                                 (to:f+ destination-column to-column))))
                          (color-source      (to:faref buffer-source
                                               (to:f+ (to:f* row-source buffer-source-width)
                                                 from-column)))
                          (color (funcall *blending-function* color-source color-destination)))
                     (declare (dynamic-extent color-destination color-source))
                     (setf (to:faref buffer-destination (to:f+ (to:f* row-destination
                                                                      buffer-destination-width)
                                                               (to:f+ destination-column to-column)))
                           color)))))
    (loop for from-row fixnum from source-row below source-last-row
          for to-row fixnum from destination-row do
            (copy-row from-row to-row))
    buffer-destination))

(defun make-rendering-thread (context)
  (nodgui.utils:make-thread
   (let ((sdl-context  context))
     (lambda ()
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       (let ((context  sdl-context))
         (with-accessors ((width           ctx:width)
                          (height          ctx:height)
                          (window          ctx:window)
                          (window-id       ctx::window-id)
                          (buffer          buffer)
                          (texture         texture)
                          (time-spent      ctx:time-spent)
                          (minimum-delta-t ctx:minimum-delta-t)) context
           (declare ((simple-array (unsigned-byte 32)) buffer))
           (declare (fixnum width height minimum-delta-t))
           (sdl2:with-init (:everything)
             (init-font-system)
             (setf window
                   (ctx::create-window-from-pointer (ctx::window-id->pointer window-id)))
             (sdl2:with-renderer (renderer window :flags '(:accelerated))
               (setf texture (make-texture renderer width height))
               (sdl2:with-event-loop (:method :poll)
                 (:idle
                  ()
                  (let* ((millis  (ctx:get-milliseconds))
                         (dt      (to:f- (the fixnum millis)
                                         (the fixnum time-spent))))
                    (declare (fixnum millis dt))
                    (when (not (ctx:modify-must-wait-p context))
                      (let ((modify-fn (ctx:pop-for-modify context)))
                        (declare (function modify-fn))
                        (funcall modify-fn dt)))
                    (if (not (or (ctx:rendering-must-wait-p context)
                                 (ctx:updating-must-wait-p context)))
                        (if (>= dt minimum-delta-t)
                            (let ((updating-fn  (ctx:pop-for-updating context))
                                  (rendering-fn (ctx:pop-for-rendering context)))
                              (declare (function updating-fn rendering-fn))
                              (setf time-spent millis)
                              (if (eq rendering-fn #'ctx:quit-sentinel)
                                  (funcall rendering-fn dt)
                                  (progn
                                    (loop while (> dt minimum-delta-t) do
                                      (funcall updating-fn dt)
                                      (decf dt minimum-delta-t))
                                    (funcall updating-fn dt)
                                    (funcall rendering-fn dt)
                                    (sdl2:update-texture texture
                                                         nil
                                                         (static-vectors:static-vector-pointer buffer)
                                                         (to:f* +channels-number+ width)) ; pitch
                                    (sdl2:render-copy renderer texture)
                                    (sdl2:render-present renderer))))
                            (sdl2:delay (to:f- minimum-delta-t dt)))
                        (sdl2:delay minimum-delta-t))))
                 (:quit ()
                        (terminate-font-system)
                        t))))))))))

(defun make-rendering-thread-blocking (context)
  (nodgui.utils:make-thread
   (let ((sdl-context  context))
     (lambda ()
       (let ((context  sdl-context))
         (with-accessors ((width           ctx:width)
                          (height          ctx:height)
                          (window          ctx:window)
                          (window-id       ctx::window-id)
                          (buffer          buffer)
                          (texture         texture)
                          (time-spent      ctx:time-spent)
                          (minimum-delta-t ctx:minimum-delta-t)) context
           (sdl2:with-init (:everything)
             (init-font-system)
             (setf window
                   (ctx::create-window-from-pointer (ctx::window-id->pointer window-id)))
             (sdl2:with-renderer (renderer window :flags '(:accelerated))
               (setf texture (make-texture renderer width height))
               (sdl2:with-event-loop (:method :poll)
                 (:idle
                  ()
                  (let* ((millis (ctx:get-milliseconds))
                         (dt     (- millis time-spent)))
                    (setf time-spent millis)
                    (let ((fn (ctx:pop-for-rendering context)))
                      (funcall fn dt)
                      (sdl2:update-texture texture
                                           nil
                                           (static-vectors:static-vector-pointer buffer)
                                           (to:f* +channels-number+ width)) ; pitch
                      (sdl2:render-copy renderer texture)
                      (sdl2:render-present renderer))))
                 (:quit ()
                        (terminate-font-system)
                        t))))))))))

(defmethod initialize-instance :after ((object pixel-buffer-context)
                                       &key
                                         (classic-frame nil)
                                         (buffer-width  nil)
                                         (buffer-height nil)
                                       &allow-other-keys)
  (when classic-frame
    (with-accessors ((width           ctx:width)
                     (height          ctx:height)
                     (buffer          buffer)
                     (thread          ctx:rendering-thread)
                     (event-loop-type ctx::event-loop-type)
                     (window          ctx:window)
                     (window-id       ctx::window-id)
                     (rendering-queue rendering-queue)) object
      (setf width     (or buffer-width
                          (nodgui:window-width  classic-frame))
            height    (or buffer-height
                          (nodgui:window-height classic-frame))
            buffer (pix:make-buffer width height))
      (tg:finalize object
                   (lambda () (pix:free-buffer-memory buffer)))
      (setf thread
            (if (ctx:events-polling-p object)
                (make-rendering-thread object)
                (make-rendering-thread-blocking object))))))

(defun sum-pixels (pixel-a pixel-b)
  (flet ((sum (a b)
           (min (to:f+ a b) #xff)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((red-a   (pix:extract-red-component pixel-a))
        (green-a (pix:extract-green-component pixel-a))
        (blue-a  (pix:extract-blue-component pixel-a))
        (alpha-a (pix:extract-blue-component pixel-a))
        (red-b   (pix:extract-red-component pixel-b))
        (green-b (pix:extract-green-component pixel-b))
        (blue-b  (pix:extract-blue-component pixel-b))
        (alpha-b (pix:extract-blue-component pixel-b)))
    (pix:assemble-color (sum red-a red-b)
                        (sum green-a green-b)
                        (sum blue-a blue-b)
                        (sum alpha-a alpha-b)))))

(defun clear-buffer (buffer width height r g b &optional (alpha 255))
  (declare (fixnum width height))
  (declare ((unsigned-byte 8) r g b alpha))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((color (pix:assemble-color r g b alpha)))
    (loop for i from 0 below (to:f* width height) do
      (setf (aref buffer i) color))))

(defun fill-rectangle (buffer buffer-width
                       top-left-x top-left-y
                       bottom-right-x bottom-right-y
                       r g b
                       &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width top-left-x top-left-y bottom-right-x bottom-right-y))
  (declare ((unsigned-byte 8) r g b a))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((width  (to:f- bottom-right-x top-left-x))
         (height (to:f- bottom-right-y top-left-y))
         (w/2    (ash width  -1))
         (h/2    (ash height -1))
         (color  (pix:assemble-color r g b a)))
    (loop for column from top-left-x below (to:f+ top-left-x w/2) do
      (loop for row from top-left-y below (to:f+ top-left-y h/2) do
        (set-pixel@ buffer buffer-width column             row r g b a)
        (set-pixel-color@ buffer buffer-width (to:f+ column w/2) row color)
        (set-pixel-color@ buffer buffer-width column             (to:f+ row h/2) color)
        (set-pixel-color@ buffer buffer-width column             (to:f+ row h/2) color)
        (set-pixel-color@ buffer buffer-width (to:f+ column w/2) (to:f+ row h/2) color)))
    buffer))

(defun fill-circle (buffer buffer-width x-center y-center radius r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((r-square  (to:f* radius radius))
        (color     (pix:assemble-color r g b a)))
    (declare (dynamic-extent r-square))
    (loop for x from 0 below radius do
      (let ((x-square (to:f* x x)))
        (declare (dynamic-extent x-square))
        (loop named inner for y from 0 below radius do
          (let ((condition (to:f<= (to:f+ (to:f* y y)
                                          x-square)
                                   r-square)))
            (if condition
                (progn
                  (set-pixel-color@ buffer buffer-width
                                    (to:f+ x-center x)
                                    (to:f+ y-center y)
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    (to:f+ x-center x)
                                    (to:f+ y-center (to:f- y))
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    (to:f+ x-center (to:f- x))
                                    (to:f+ y-center (to:f- y))
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    (to:f+ x-center (to:f- x))
                                    (to:f+ y-center        y)
                                    color))
                (return-from inner t))))))
    buffer))

(a:define-constant +cos-45-degree+ (to:d (cos (/ pi 4))) :test #'=)

(defun draw-circle (buffer buffer-width x-center y-center radius r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((x-end     (ash (to:f* radius 185364) -18))
        (color     (pix:assemble-color r g b a)))
    (declare (dynamic-extent x-end))
    (loop for x fixnum from 0 to x-end
          with y fixnum          = radius
          with threshold fixnum  = 0
          do
             (if (< threshold 0)
                 (incf threshold (to:f+ (to:f* 2 x)
                                        1))
                 (progn
                   (incf threshold (to:f* 2 (to:f+ (to:f- x y)
                                                   1)))
                   (decf y)))
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center x)
                               (to:f+ y-center y)
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center y)
                               (to:f+ y-center x)
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center (to:f- x))
                               (to:f+ y-center y)
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center y)
                               (to:f+ y-center (to:f- x))
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center (to:f- x))
                               (to:f+ y-center (to:f- y))
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center (to:f- y))
                               (to:f+ y-center (to:f- x))
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center        x)
                               (to:f+ y-center (to:f- y))
                               color)
             (set-pixel-color@ buffer buffer-width
                               (to:f+ x-center (to:f- y))
                               (to:f+ y-center        x)
                               color))))

(defun calc-octant (x y)
  (declare (fixnum x y))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((dx-positive (to:f> x 0))
         (dy-positive (to:f> y 0))
         (dx-negative (not dx-positive))
         (dy-negative (not dy-positive))
         (abs-dy>dx   (to:f> (abs y)
                             (abs x))))
    (declare (dynamic-extent dx-positive dx-negative dy-positive dy-negative abs-dy>dx))
    (cond
      ;; second octant
      ((and dx-positive
            dy-positive
            (to:f> y x))
       :2)
      ;; third octant
      ((and dx-negative
            dy-positive
            abs-dy>dx)
       :3)
      ;; fourth octant
      ((and dx-negative
            dy-positive
            (not abs-dy>dx))
       :4)
      ;; fifth octant
      ((and dx-negative
            dy-negative
            (not abs-dy>dx))
       :5)
      ;; sixth octant
      ((and dx-negative
            dy-negative
            abs-dy>dx)
       :6)
      ;; seventh octant
      ((and dx-positive
            dy-negative
            abs-dy>dx)
       :7)
      ;; eight octant
      ((and dx-positive
            dy-negative
            (not abs-dy>dx)
            (not (= y 0)))
       :8)
      ;; first octant
      (t :1))))

(defun to-first-octant (octant x y)
  (declare (symbol octant))
  (declare (fixnum x y))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case octant
    (:1
     (values x y))
    (:2
     (values y x))
    (:3
     (values y (to:f- x)))
    (:4
     (values (to:f- x) y))
    (:5
     (values (to:f- x) (to:f- y)))
    (:6
     (values (to:f- y) (to:f- x)))
    (:7
     (values (to:f- y) x))
    (:8
     (values x (to:f- y)))))

(defun from-first-octant (new-octant x y)
  (declare (symbol new-octant))
  (declare (fixnum x y))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case new-octant
    (:1
     (values x y))
    (:2
     (values y x))
    (:3
     (values (to:f- y) x))
    (:4
     (values (to:f- x) y))
    (:5
     (values (to:f- x) (to:f- y)))
    (:6
     (values (to:f- y) (to:f- x)))
    (:7
     (values y (to:f- x)))
    (:8
     (values x (to:f- y)))))

(defun draw-line (buffer buffer-width x0 y0 x1 y1 r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width x0 y0 x1 y1))
  (declare ((unsigned-byte 8) r g b a))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((octant (calc-octant (to:f- x1 x0)
                             (to:f- y1 y0))))
    (declare (dynamic-extent octant))
    (multiple-value-bind (first-octant-x first-octant-y)
        (to-first-octant octant
                         (to:f- x1 x0)
                         (to:f- y1 y0))
      (let* ((delta-x   (the fixnum first-octant-x))
             (delta-y   (the fixnum first-octant-y))
             (2dx       (ash delta-x 1))
             (2dy       (ash delta-y 1))
             (threshold (to:f- 2dy delta-x))
             (color     (pix:assemble-color r g b a)))
        (declare (dynamic-extent delta-x
                                 delta-y
                                 2dx
                                 2dy
                                 threshold))
        (loop with x fixnum = 0
              with y fixnum = 0
              while (to:f< x delta-x)
              do
                 (multiple-value-bind (actual-x actual-y)
                     (from-first-octant octant x y)
                   (declare (dynamic-extent actual-x actual-y))
                   (set-pixel-color@ buffer
                                     buffer-width
                                     (to:f+ (the fixnum actual-x) x0)
                                     (to:f+ (the fixnum actual-y) y0)
                                     color))
                 (when (to:f>= (the fixnum threshold) 0)
                   (incf (the fixnum threshold) (to:f- 2dx))
                   (incf y))
                 (incf (the fixnum threshold) 2dy)
                 (incf x)))))
    buffer)

(defun bilinear-interpolation (buffer buffer-width buffer-height x y)
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width buffer-height))
  (declare (to::desired-type x y))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; a          b
  ;; +----------+
  ;; |          |
  ;; |          |
  ;; +----------+
  ;; d          c
  (flet ((clamp-coordinate (coord max-value)
           (declare (to::desired-type max-value coord))
           (declare (optimize (speed 3) (debug 0) (safety 0)))
           (cond
             ((< coord 0.0)
              0.0)
             ((>= coord (1- max-value))
              (1- max-value))
             (t
              coord))))
      (let* ((floor-x   (floor (clamp-coordinate x (to:d buffer-width))))
             (floor-y   (floor (clamp-coordinate y (to:d buffer-height))))
             (ceiling-x (ceiling (clamp-coordinate x (to:d buffer-width))))
             (ceiling-y (ceiling (clamp-coordinate y (to:d buffer-height))))
             (dx        (truncate (to:d* 255.0f0 (to:d- x (to:d (the fixnum (floor x)))))))
             (dy        (truncate (to:d* 255.0f0 (to:d- y (to:d (the fixnum (floor y)))))))
             (a         (pixel@ buffer buffer-width floor-x floor-y))
             (b         (pixel@ buffer buffer-width ceiling-x floor-y))
             (c         (pixel@ buffer buffer-width ceiling-x ceiling-y))
             (d         (pixel@ buffer buffer-width floor-x ceiling-y))
             (inter-x1  (color-lerp c d dx))
             (inter-x2  (color-lerp b a dx))
             (inter-y   (color-lerp inter-x1 inter-x2 dy)))
        inter-y)))

(u:definline translate (coordinate delta)
  (declare (fixnum coordinate delta))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:f+ coordinate delta))

(u:definline float-translate (coordinate delta)
  (declare (to::desired-type coordinate delta))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d+ coordinate delta))

(u:definline rotate-sin-cos (x y sin-angle cosin-angle)
  (declare (fixnum x y))
  (declare (to::desired-type sin-angle cosin-angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (values (truncate (to:d- (to:d* (to:d x)
                                  cosin-angle)
                           (to:d* (to:d y)
                                  sin-angle)))
          (truncate (to:d+ (to:d* (to:d x)
                                  sin-angle)
                           (to:d* (to:d y)
                                  cosin-angle)))))

(defun rotate (x y angle)
  (declare (fixnum x y))
  (declare (to::desired-type angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((sin-angle   (to:dsin angle))
        (cosin-angle (to:dcos angle)))
    (rotate-sin-cos x y sin-angle cosin-angle)))

(u:definline float-rotate-sin-cos (x y sin-angle cosin-angle)
  (declare (to::desired-type x y sin-angle cosin-angle))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (values (to:d- (to:d* (to:d x)
                        cosin-angle)
                 (to:d* (to:d y)
                        sin-angle))
          (to:d+ (to:d* (to:d x)
                        sin-angle)
                 (to:d* (to:d y)
                        cosin-angle))))

(defun float-rotate (x y angle)
  (declare (to::desired-type angle))
  (declare (fixnum x y))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((sin-angle   (to:dsin angle))
        (cosin-angle (to:dcos angle)))
    (float-rotate-sin-cos (to:d x) (to:d y) sin-angle cosin-angle)))

(defun float-coordinate-scale (coordinate factor)
  (declare (to::desired-type coordinate factor))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:d* coordinate factor))

(defun coordinate-scale (coordinate factor)
  (declare (fixnum coordinate))
  (declare (to::desired-type factor))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (truncate (the to::desired-type (float-coordinate-scale (to:d coordinate) factor))))

(defun blit-transform (buffer-source
                       buffer-source-width
                       buffer-source-height
                       buffer-destination
                       buffer-destination-width
                       buffer-destination-height
                       source-row
                       source-column
                       destination-row
                       destination-column
                       source-last-row
                       source-last-column
                       &optional
                         (rotation 0f0)
                         (scaling-row 1.0f0)
                         (scaling-column 1.0f0)
                         (pivot-row 0)
                         (pivot-column 0)
                         (translate-x 0)
                         (translate-y 0))
  (declare ((simple-array (unsigned-byte 32)) buffer-source buffer-destination))
  (declare (nodgui.typed-operations::desired-type rotation scaling-row scaling-column))
  (declare (fixnum buffer-source-width
                   buffer-source-height
                   buffer-destination-width
                   buffer-destination-height
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column
                   pivot-row
                   pivot-column
                   translate-x
                   translate-y))
  (declare (function *blending-function*))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (and (/= rotation 0.0)
           (or (/= source-column 0)
               (/= source-row 0)
               (/= source-last-column buffer-source-width)
               (/= source-last-row buffer-source-height)))
      (let ((copy-width  (to:f- source-last-column source-column))
            (copy-height (to:f- source-last-row source-row)))
        (pix:with-buffer (copy copy-width copy-height)
          (blit buffer-source
                buffer-source-width
                copy
                copy-width
                source-row
                source-column
                0
                0
                source-last-row
                source-last-column)
          (blit-transform copy
                          copy-width
                          copy-height
                          buffer-destination
                          buffer-destination-width
                          buffer-destination-height
                          0
                          0
                          destination-row
                          destination-column
                          copy-height
                          copy-width
                          rotation
                          scaling-row
                          scaling-column
                          pivot-row
                          pivot-column
                          translate-x
                          translate-y)))
      (let* ((angle                  (to:degree->radians rotation))
             (cos-reverse-angle      (to:dcos (to:d- angle)))
             (sin-reverse-angle      (to:dsin (to:d- angle)))
             (reverse-scaling-column (to:d/ (to:d 1.0)
                                            scaling-column))
             (reverse-scaling-row    (to:d/ (to:d 1.0)
                                            scaling-row))
             (scaled-pivot-row               (float-coordinate-scale (to:d pivot-row) scaling-row))
             (scaled-pivot-column            (float-coordinate-scale (to:d pivot-column) scaling-column))
             (width-source-rectangle         (to:f- source-last-column source-column))
             (height-source-rectangle        (to:f- source-last-row source-row))
             (scaled-origin-column-offset    (the fixnum (coordinate-scale source-column scaling-column)))
             (scaled-origin-row-offset       (the fixnum (coordinate-scale source-row scaling-row)))
             (scaled-translate-x             (the fixnum (coordinate-scale translate-x scaling-column)))
             (scaled-translate-y             (the fixnum (coordinate-scale translate-y scaling-row))))
        (declare (dynamic-extent angle cos-reverse-angle sin-reverse-angle reverse-scaling-column
                                 reverse-scaling-row scaled-pivot-row scaled-pivot-column
                                 width-source-rectangle height-source-rectangle scaled-origin-column-offset
                                 scaled-origin-row-offset
                                 scaled-translate-x scaled-translate-y))
        (flet ((calc-aabb-vertex (x y)
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (let ((to-origin-x (coordinate-scale (translate (translate x (- pivot-column))
                                                                 (to:f- destination-column))
                                                      scaling-column))
                       (to-origin-y (coordinate-scale (translate (translate y (- pivot-row))
                                                                 (to:f- destination-row))
                                                      scaling-row)))
                   (multiple-value-bind (rotated-x rotated-y)
                       (rotate to-origin-x to-origin-y angle)
                     (values (translate rotated-x (to:f+ destination-column
                                                    (the fixnum (truncate (the to::desired-type
                                                                               scaled-pivot-column)))))
                             (translate rotated-y (to:f+ destination-row
                                                    (the fixnum (truncate (the to::desired-type
                                                                               scaled-pivot-row))))))))))
          (multiple-value-bind (aabb-1-column aabb-1-row)
              (calc-aabb-vertex destination-column destination-row)
            (multiple-value-bind (aabb-2-column aabb-2-row)
                (calc-aabb-vertex (to:f+ destination-column width-source-rectangle)
                                  destination-row)
              (multiple-value-bind (aabb-3-column aabb-3-row)
                  (calc-aabb-vertex (to:f+ destination-column width-source-rectangle)
                                    (to:f+ destination-row height-source-rectangle))
                (multiple-value-bind (aabb-4-column aabb-4-row)
                    (calc-aabb-vertex destination-column
                                      (to:f+ destination-row height-source-rectangle))
                  (let* ((aabb-min-column (min aabb-1-column
                                               aabb-2-column
                                               aabb-3-column
                                               aabb-4-column))
                         (aabb-max-column (max aabb-1-column
                                               aabb-2-column
                                               aabb-3-column
                                               aabb-4-column))
                         (aabb-min-row    (min aabb-1-row
                                               aabb-2-row
                                               aabb-3-row
                                               aabb-4-row))
                         (aabb-max-row    (max aabb-1-row
                                               aabb-2-row
                                               aabb-3-row
                                               aabb-4-row))
                         (untranslate-column (to:d+ (to:d- (to:d destination-column))
                                                    (to:d- scaled-pivot-column)))
                         (untranslate-row    (to:d+ (to:d- (to:d destination-row))
                                                    (to:d- scaled-pivot-row))))
                    (declare (dynamic-extent untranslate-row untranslate-column
                                             aabb-max-row aabb-max-column
                                             aabb-min-row aabb-min-column))
                    (loop for to-row fixnum from (to:f+ aabb-min-row scaled-origin-row-offset)
                            below (to:f+ aabb-max-row scaled-origin-row-offset) do
                              (loop for to-column fixnum from (to:f+ aabb-min-column scaled-origin-column-offset)
                                      below (to:f+ aabb-max-column scaled-origin-column-offset) do
                                        (let* ((actual-to-row    (to:f+ to-row scaled-translate-y))
                                               (actual-to-column (to:f+ to-column scaled-translate-x)))
                                          (declare (dynamic-extent actual-to-column actual-to-row))
                                          (when (and (to:f>= actual-to-row 0)
                                                     (to:f>= actual-to-column 0)
                                                     (to:f<  actual-to-row buffer-destination-height)
                                                     (to:f<  actual-to-column buffer-destination-width))
                                            (multiple-value-bind (reverse-rotated-destination-column
                                                                  reverse-rotated-destination-row)
                                                (float-rotate-sin-cos (to:d (float-translate (to:d to-column)
                                                                                             untranslate-column))

                                                                      (to:d (float-translate (to:d to-row)
                                                                                             untranslate-row))
                                                                      sin-reverse-angle
                                                                      cos-reverse-angle)
                                              (let* ((unscaled-source-column
                                                       (float-coordinate-scale (float-translate reverse-rotated-destination-column
                                                                                                (to:d scaled-pivot-column))
                                                                               reverse-scaling-column))
                                                     (unscaled-source-row
                                                       (float-coordinate-scale (float-translate reverse-rotated-destination-row
                                                                                                (to:d scaled-pivot-row))
                                                                               reverse-scaling-row)))
                                                (when (and (to:d>= unscaled-source-row 0f0)
                                                           (to:d>= unscaled-source-column 0f0)
                                                           (to:d< unscaled-source-row (to:d buffer-source-height))
                                                           (to:d< unscaled-source-column (to:d buffer-source-width)))
                                                  (let* ((color-source      (bilinear-interpolation buffer-source
                                                                                                    buffer-source-width
                                                                                                    buffer-source-height
                                                                                                    unscaled-source-column
                                                                                                    unscaled-source-row))
                                                         (color-destination (pixel@ buffer-destination
                                                                                    buffer-destination-width
                                                                                    actual-to-column
                                                                                    actual-to-row))
                                                         (color             (funcall *blending-function*
                                                                                     color-source
                                                                                     color-destination)))
                                                    (declare (dynamic-extent color-destination color-source))
                                                    (set-pixel-color@ buffer-destination
                                                                      buffer-destination-width
                                                                      actual-to-column
                                                                      actual-to-row
                                                                      color)))))))))))))))))
  buffer-destination)

(defun draw-text (buffer buffer-width text font x y r g b a)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((surface        (sdl2-ttf:render-utf8-blended font text r g b a))
         (pixels         (sdl2:surface-pixels surface))
         ;;(surface-width  (sdl2:surface-width surface))
         (surface-height (sdl2:surface-height surface))
         (surface-pitch  (sdl2:surface-pitch surface))
         (surface-pixels-width (to:f/ surface-pitch 4)))
    (declare (fixnum surface-height surface-pitch surface-pixels-width))
    (flet ((extract-channel (raw-pixel shift)
             (logand (ash raw-pixel shift)
                     #xff)))
      (pix:with-buffer (tmp-buffer (to:f* surface-height surface-pixels-width) surface-height)
        (loop for index from 0 below (to:f* surface-height surface-pixels-width)
              do
                 (let* ((raw-pixel    (cffi:mem-aref pixels :uint32 index))
                        (alpha        (extract-channel raw-pixel -24))
                        (source-color (pix:assemble-color r g b alpha)))
                   (declare ((simple-array (unsigned-byte 32)) tmp-buffer))
                   (setf (elt tmp-buffer index) source-color)))
        (let ((*blending-function* #'blending-function-combine))
          (blit tmp-buffer
                surface-pixels-width
                buffer
                buffer-width
                0
                0
                y
                x
                surface-height
                surface-pixels-width)
          (sdl2:free-surface surface))))))

(defun init-font-system ()
  (sdl2-ttf:init))

(defun terminate-font-system ()
  (when (> (sdl2-ttf:was-init) 0)
    (sdl2-ttf:quit)))

(defun open-font (font-path points-size)
  (sdl2-ttf:open-font font-path points-size))

(defun close-font (font-handle)
  (sdl2-ttf:close-font font-handle))
