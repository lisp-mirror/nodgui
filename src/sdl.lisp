(in-package :nodgui.sdl-window)

(a:define-constant +channels-number+ 4 :test #'=)

(defun fps->delta-t (fps)
  (truncate (* 1000 (/ 1 fps))))

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

(defun extract-red-component (color)
  (declare (fixnum color))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -24) #xff))

(defun extract-blue-component (color)
  (declare (fixnum color))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -8) #xff))

(defun extract-green-component (color)
  (declare (fixnum color))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -16) #xff))

(defun extract-alpha-component (color)
  (declare (fixnum color))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand color #xff))

(defun assemble-color (r g b &optional (a 255))
  (declare ((unsigned-byte 8) r g b a))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logior (logand a #xff)
          (the fixnum (ash b 8))
          (the fixnum (ash g 16))
          (the fixnum (ash r 24))))

(defun set-pixel@ (buffer width x y r g b &optional (a 255))
  "Note: no bounds checking is done"
  (declare (fixnum x y width r g b a))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((value (assemble-color r g b a)))
    (setf (aref buffer (to:f+ x (to:f* y width)))
          value)))

(defun pixel@ (buffer width x y)
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 3) (safety 0)))
  (to:faref buffer (to:f+ x (the fixnum (to:f* y width)))))

(defmacro with-displace-pixel ((r g b a) pixel &body body)
  `(let ((,r (extract-red-component   ,pixel))
         (,g (extract-green-component ,pixel))
         (,b (extract-blue-component  ,pixel))
         (,a (extract-alpha-component ,pixel)))
     (declare ((unsigned-byte 8) ,r ,g ,b ,a))
     ,@body))

(defun color-channel-lerp (a b w)
  (declare ((unsigned-byte 8) a b w))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (ash (+ (* a w)
          (* b (- 256 w)))
       -8))

(defun blending-function-combine (pixel-source pixel-destination)
  (declare (fixnum pixel-source pixel-destination))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-displace-pixel (r-source g-source b-source alpha-source)
                       pixel-source
    (with-displace-pixel (r-destination g-destination b-destination alpha-destination)
                         pixel-destination
      (declare (ignore alpha-destination))
      (assemble-color (color-channel-lerp r-source r-destination alpha-source)
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
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-displace-pixel (r-source g-source b-source alpha-source)
                       pixel-source
    (with-displace-pixel (r-destination g-destination b-destination alpha-destination)
                         pixel-destination
      (assemble-color (saturate-byte (to:f+ r-source r-destination))
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
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((copy-row (row-source row-destination)
           (declare (fixnum row-source row-destination))
           ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
           (loop for column fixnum from source-column below source-last-column do
             (let* ((color-destination (to:faref buffer-destination
                                                 (to:f+ (to:f* row-destination
                                                               buffer-destination-width)
                                                        (to:f+ destination-column column))))
                    (color-source      (to:faref buffer-source
                                                 (to:f+ (to:f* row-source buffer-source-width)
                                                               column)))
                    (color (funcall *blending-function* color-source color-destination)))
               (setf (to:faref buffer-destination (to:f+ (to:f* row-destination
                                                                buffer-destination-width)
                                                         (to:f+ destination-column column)))
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
             (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
               (setf texture (make-texture renderer width height))
               (sdl2:with-event-loop (:method :poll)
                 (:idle
                  ()
                  (let* ((millis  (get-milliseconds))
                         (dt      (- millis time-spent)))
                    (when (>= dt minimum-delta-t)
                      (setf time-spent millis)
                      (when (not (rendering-must-wait-p context))
                        (let ((fn (pop-for-rendering context)))
                          (funcall fn dt)
                          (sdl2:update-texture texture
                                               nil
                                               (static-vectors:static-vector-pointer buffer)
                                               (to:f* +channels-number+ width)) ; pitch
                          (sdl2:render-copy renderer texture)
                          (sdl2:render-present renderer))))))
                 (:quit () t))))))))))

(defun buffer-sizes->static-vector-size (width height)
  (declare (fixnum width height))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:f* width height))

(defun make-buffer (width height)
  (make-buffer-elements (buffer-sizes->static-vector-size width height)))

(defun make-buffer-elements (element-count)
  (static-vectors:make-static-vector element-count
                                     :element-type '(unsigned-byte 32)
                                     :initial-element #x000000ff))

(defun free-buffer-memory (buffer)
  (static-vectors:free-static-vector buffer))

(defmacro with-buffer ((buffer width height) &body body)
  `(unwind-protect
        (let ((,buffer (nake-buffer ,width ,height)))
          ,@body)
     (free-buffer-memory ,buffer)))

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
            buffer    (make-buffer width height))
      (tg:finalize object
                   (lambda () (free-buffer-memory buffer)))
      (setf thread (make-rendering-thread object)))))

(defgeneric quit-sdl (object))

(defgeneric events-polling-p (object))

(defgeneric push-for-rendering (object function &key force-push))

(defgeneric pop-for-rendering (object))

(defgeneric rendering-must-wait-p (object))

(defgeneric sync (object))

(defmethod events-polling-p ((object context))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (eq (event-loop-type object) :polling))

(defmethod quit-sdl ((object context))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (push-for-rendering object
                      (lambda (dt)
                        (declare (ignore dt))
                        (sdl2:push-event :quit))
                      :force-push t)
  (bt:join-thread (rendering-thread object)))

(defmethod push-for-rendering ((object context) (function function) &key (force-push nil))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (if force-push
          (q:push-forced (queue object) function)
          (q:push (queue object) function))
      (bq:push-unblock (queue object) function)))

(defmethod pop-for-rendering ((object context))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (q:pop (queue object))
      (bq:pop-block (queue object))))

(defmethod rendering-must-wait-p ((object context))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (q:emptyp (queue object))
      (bq:emptyp (queue object))))

(defmethod sync ((object context))
  (sdl2:delay (minimum-delta-t object)))

(defun make-sdl-frame (width height)
  (make-instance 'nodgui:classic-frame
                 :width  width
                 :height height
                 :background ""))

(defun sum-pixels (pixel-a pixel-b)
  (flet ((sum (a b)
           (min (to:f+ a b) #xff)))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((red-a   (extract-red-component pixel-a))
        (green-a (extract-green-component pixel-a))
        (blue-a  (extract-blue-component pixel-a))
        (alpha-a (extract-blue-component pixel-a))
        (red-b   (extract-red-component pixel-b))
        (green-b (extract-green-component pixel-b))
        (blue-b  (extract-blue-component pixel-b))
        (alpha-b (extract-blue-component pixel-b)))
    (assemble-color (sum red-a red-b)
                    (sum green-a green-b)
                    (sum blue-a blue-b)
                    (sum alpha-a alpha-b)))))

(defun clear-buffer (buffer width height r g b &optional (alpha 255))
  (declare (fixnum width height r g b alpha))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((color (assemble-color r g b alpha)))
    (loop for i from 0 below (to:f* width height) do
      (setf (aref buffer i) color))))
