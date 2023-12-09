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
    :initform (fps->delta-t 30)
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
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -24) #xff))

(defun extract-blue-component (color)
  (declare (fixnum color))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -8) #xff))

(defun extract-green-component (color)
  (declare (fixnum color))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand (ash color -16) #xff))

(defun extract-alpha-component (color)
  (declare (fixnum color))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logand color #xff))

(defun assemble-color (r g b &optional (a 255))
  (declare (fixnum r g b a))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (logior (logand a #xff)
          (the fixnum (ash b 8))
          (the fixnum (ash g 16))
          (the fixnum (ash r 24))))

(defun set-pixel@ (buffer width x y r g b &optional (a 255))
  (declare (fixnum x y width r g b a))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((value (assemble-color r g b a)))
    (setf (aref buffer (to:f+ x (to:f* y width)))
          value)))

(defun pixel@ (buffer width x y)
  (declare (fixnum x y width))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (optimize (speed 3) (debug 3) (safety 0)))
  (to:faref buffer (to:f+ x (the fixnum (to:f* y width)))))

(defun blit (buffer-source
             buffer-source-width
             buffer-destination
             buffer-destination-width
             source-row source-column
             source-last-column
             destination-row)
  (declare ((simple-array (unsigned-byte 32)) buffer-source buffer-destination))
  (declare (fixnum buffer-source-width
                   buffer-destination-width
                   source-row source-column
                   source-last-column
                   destination-row))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (flet ((copy-row (source-row destination-row)
           (declare (fixnum source-row destination-row))
           (declare (optimize (speed 3) (debug 0) (safety 0)))
           (loop for column fixnum from source-column below source-last-column do
             (setf (to:faref buffer-source (to:f+ (to:f* destination-row buffer-source-width)
                                                  column))
                   (to:faref buffer-destination (to:f+ (to:f* source-row
                                                              buffer-destination-width)
                                                       column))))))
    (loop for from-row fixnum from source-row below destination-row
          for to-row fixnum from destination-row do
      (copy-row from-row destination-row))
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
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (to:f* width height))

(defun make-buffer (width height)
  (static-vectors:make-static-vector (buffer-sizes->static-vector-size width height)
                                     :element-type '(unsigned-byte 32)
                                     :initial-element #x000000ff))

(defmethod initialize-instance :after ((object context)
                                       &key
                                         (classic-frame nil)
                                         (buffer-width  nil)
                                         (buffer-height nil)
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
                          (q:make-queue)
                          (make-instance 'bq:synchronized-queue))
            width     (or buffer-width
                          (nodgui:window-width  classic-frame))
            height    (or buffer-height
                          (nodgui:window-height classic-frame))
            buffer    (make-buffer width height))
      (tg:finalize object
                   #'(lambda ()
                       (static-vectors:free-static-vector buffer)))
      (setf thread (make-rendering-thread object)))))

(defgeneric quit-sdl (object))

(defgeneric events-polling-p (object))

(defgeneric push-for-rendering (object function))

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
                        (sdl2:push-event :quit)))
  (bt:join-thread (rendering-thread object)))

(defmethod push-for-rendering ((object context) (function function))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (events-polling-p object)
      (q:push (queue object) function)
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

(defun make-sdl-frame (width height)
  (make-instance 'nodgui:classic-frame
                 :width  width
                 :height height
                 :background ""))

(defun sum-pixels (pixel-a pixel-b)
  (flet ((sum (a b)
           (min (to:f+ a b) #xff)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
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
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((color (assemble-color r g b alpha)))
    (loop for i from 0 below (to:f* width height) do
      (setf (aref buffer i) color))))
