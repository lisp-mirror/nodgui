(in-package :nodgui.pixels-canvas)

(a:define-constant +channels-number+ 4 :test #'=)

(defun fps->delta-t (fps)
  (truncate (/ 1000 fps)))

(defun get-milliseconds ()
  (sdl2:get-ticks))

(defclass context ()
  ((window-id
    :initform nil
    :initarg :window-id
    :accessor window-id)
   (window
    :initform nil
    :initarg :window
    :accessor window)
   (initialization-function
    :initform (lambda (window) window)
    :initarg :initialization-function
    :accessor initialization-function
    :type function)
   (queue
    :initform nil
    :initarg :queue
    :accessor queue)
   (rendering-thread
    :initform nil
    :initarg :rendering-thread
    :accessor rendering-thread)
   (event-loop-type
    :initform :polling
    :initarg :event-loop-type
    :accessor event-loop-type)
   (width
    :initform 0
    :initarg :width
    :accessor width)
   (height
    :initform 0
    :initarg :height
    :accessor height)
   (texture
    :initform nil
    :initarg :texture
    :accessor texture)
   (buffer
    :initform nil
    :initarg :buffer
    :accessor buffer)
   (minimum-delta-t
    :initform (fps->delta-t 60)
    :initarg :minimum-delta-t
    :accessor minimum-delta-t)
   (time-spent
    :initform (get-milliseconds)
    :initarg  :time-spent
    :accessor time-spent)))

(defun create-window-from-pointer (pointer-id)
  (sdl2::check-nullptr (sdl2::sdl-create-window-from pointer-id)))

(defun window-id->pointer (frame-id)
  (cffi:make-pointer frame-id))

(defun make-texture (renderer width height)
  (sdl2:create-texture renderer :rgba8888 :streaming width height))

(u:definline set-pixel-color@ (buffer width x y color)
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((unsigned-byte 32) color))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (setf (aref buffer (to:f+ x (to:f* y width)))
        color))

(defun set-pixel@ (buffer width x y r g b &optional (a 255))
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((unsigned-byte 8) r g b a))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((color (pix:assemble-color r g b a)))
    (set-pixel-color@ buffer width x y color)))

(defun pixel@ (buffer width x y)
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
       (let ((context  sdl-context))
         (with-accessors ((width           width)
                          (height          height)
                          (window          window)
                          (window-id       window-id)
                          (buffer          buffer)
                          (texture         texture)
                          (time-spent      time-spent)
                          (minimum-delta-t minimum-delta-t)) context
           (sdl2:with-init (:everything)
             (setf window (create-window-from-pointer (window-id->pointer window-id)))
             (sdl2:with-renderer (renderer window :flags '(:accelerated))
               (setf texture (make-texture renderer width height))
               (sdl2:with-event-loop (:method :poll)
                 (:idle
                  ()
                  (let* ((millis  (get-milliseconds))
                         (dt      (- millis time-spent)))
                    (when (>= dt minimum-delta-t)
                      (when (not (rendering-must-wait-p context))
                        (setf time-spent millis)
                        (let ((fn (pop-for-rendering context)))
                          (funcall fn dt)
                          (sdl2:update-texture texture
                                               nil
                                               (static-vectors:static-vector-pointer buffer)
                                               (to:f* +channels-number+ width)) ; pitch
                          (sdl2:render-copy renderer texture)
                          (sdl2:render-present renderer))))))
                 (:quit () t))))))))))

(defmethod initialize-instance :after ((object context)
                                       &key
                                         (classic-frame nil)
                                         (buffer-width  nil)
                                         (buffer-height nil)
                                         (non-blocking-queue-maximum-size 8192)
                                       &allow-other-keys)
  (when classic-frame
    (with-accessors ((width           width)
                     (height          height)
                     (buffer          buffer)
                     (thread          rendering-thread)
                     (event-loop-type event-loop-type)
                     (window          window)
                     (window-id       window-id)
                     (queue           queue)) object
      (setf window-id (nodgui:window-id classic-frame)
            queue     (if (events-polling-p object)
                          (q:make-queue :maximum-size non-blocking-queue-maximum-size)
                          (make-instance 'bq:synchronized-queue))
            width     (or buffer-width
                          (nodgui:window-width  classic-frame))
            height    (or buffer-height
                          (nodgui:window-height classic-frame))
            buffer    (pix:make-buffer width height))
      (tg:finalize object
                   (lambda () (pix:free-buffer-memory buffer)))
      (setf thread (make-rendering-thread object)))))

(defgeneric quit-sdl (object))

(defgeneric events-polling-p (object))

(defgeneric push-for-rendering (object function &key force-push))

(defgeneric pop-for-rendering (object))

(defgeneric rendering-must-wait-p (object))

(defgeneric sync (object))

(defmethod events-polling-p ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (eq (event-loop-type object) :polling))

(defmethod quit-sdl ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (push-for-rendering object
                      (lambda (dt)
                        (declare (ignore dt))
                        (sdl2:push-event :quit))
                      :force-push t)
  (bt:join-thread (rendering-thread object)))

(defmethod push-for-rendering ((object context) (function function) &key (force-push nil))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (if force-push
          (q:push-forced (queue object) function)
          (q:push (queue object) function))
      (bq:push-unblock (queue object) function)))

(defmethod pop-for-rendering ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (q:pop (queue object))
      (bq:pop-block (queue object))))

(defmethod rendering-must-wait-p ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (q:emptyp (queue object))
      (bq:emptyp (queue object))))

(defmethod sync ((object context))
  (sdl2:delay (minimum-delta-t object)))

(defun make-sdl-frame (width height &rest args)
  (apply #'make-instance
         (append (list 'nodgui:classic-frame
                       :width  width
                       :height height
                       :background "")
                 args)))

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
    (loop for x from 0 below radius do
      (let ((x-square (to:f* x x)))
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
        (loop with x fixnum = 0
              with y fixnum = 0
              while (to:f< x delta-x)
              do
                 (multiple-value-bind (actual-x actual-y)
                     (from-first-octant octant x y)
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
             (dx        (truncate (to:d* 255.0 (to:d- x (to:d (floor x))))))
             (dy        (truncate (to:d* 255.0 (to:d- y (to:d (floor y))))))
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
                       rotation
                       scaling-row
                       scaling-column
                       pivot-row
                       pivot-column)
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
                   pivot-column))
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
                          pivot-column)))
      (let* ((angle              (to:degree->radians rotation))
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
             (scaled-origin-row-offset       (the fixnum (coordinate-scale source-row scaling-row))))
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
                                               aabb-4-row)))
                    (loop for to-row fixnum from (to:f+ aabb-min-row scaled-origin-row-offset)
                            below (to:f+ aabb-max-row scaled-origin-row-offset) do
                              (loop for to-column fixnum from (to:f+ aabb-min-column scaled-origin-column-offset)
                                      below (to:f+ aabb-max-column scaled-origin-column-offset) do
                                        (when (and (to:f>= to-row 0)
                                                   (to:f>= to-column 0)
                                                   (to:f< to-row buffer-destination-height)
                                                   (to:f< to-column buffer-destination-width))
                                          (multiple-value-bind (reverse-rotated-destination-column
                                                                reverse-rotated-destination-row)
                                              (float-rotate-sin-cos (to:d (float-translate (float-translate (to:d to-column)
                                                                                                            (to:d- (to:d destination-column)))
                                                                                           (to:d- scaled-pivot-column)))
                                                                    (to:d (float-translate (float-translate (to:d to-row)
                                                                                                            (to:d- (to:d destination-row)))
                                                                                           (to:d- scaled-pivot-row)))
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
                                              (when (and (to:d>= unscaled-source-row 0.0)
                                                         (to:d>= unscaled-source-column 0.0)
                                                         (to:d< unscaled-source-row (to:d buffer-source-height))
                                                         (to:d< unscaled-source-column (to:d buffer-source-width)))
                                                (let* ((color-source      (bilinear-interpolation buffer-source
                                                                                                  buffer-source-width
                                                                                                  buffer-source-height
                                                                                                  unscaled-source-column
                                                                                                  unscaled-source-row))

                                                       (color-destination (pixel@ buffer-destination
                                                                                  buffer-destination-width
                                                                                  to-column
                                                                                  to-row))
                                                       (color             (funcall *blending-function*
                                                                                   color-source
                                                                                   color-destination)))
                                                  (set-pixel-color@ buffer-destination
                                                                    buffer-destination-width
                                                                    to-column
                                                                    to-row
                                                                    color))))))))))))))))
  buffer-destination)
