(in-package :nodgui.rendering-buffer-context)

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
   (rendering-queue
    :initform nil
    :initarg :rendering-queue
    :accessor rendering-queue)
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
   (minimum-delta-t
    :initform (fps->delta-t 60)
    :initarg :minimum-delta-t
    :accessor minimum-delta-t)
   (time-spent
    :initform (get-milliseconds)
    :initarg  :time-spent
    :accessor time-spent)))

(defmethod initialize-instance :after ((object context)
                                       &key
                                         (classic-frame nil)
                                         (non-blocking-queue-maximum-size 8192)
                                       &allow-other-keys)
  (when classic-frame
    (with-accessors ((width           width)
                     (height          height)
                     (thread          rendering-thread)
                     (event-loop-type event-loop-type)
                     (window          window)
                     (window-id       window-id)
                     (rendering-queue rendering-queue)) object
      (setf window-id       (nodgui:window-id classic-frame)
            rendering-queue (if (events-polling-p object)
                                (q:make-queue :maximum-size non-blocking-queue-maximum-size)
                                (make-instance 'bq:synchronized-queue))
            width           (nodgui:window-width classic-frame)
            height          (nodgui:window-height classic-frame))
      (assert (or (eq event-loop-type :polling)
                  (eq event-loop-type :serving))
              (event-loop-type)
              "value of event-loop-type slot can be only :polling or serving, not ~a"))))

(defun create-window-from-pointer (pointer-id)
  (sdl2::check-nullptr (sdl2::sdl-create-window-from pointer-id)))

(defun window-id->pointer (frame-id)
  (cffi:make-pointer frame-id))

(defgeneric quit-sdl (object))

(defgeneric events-polling-p (object))

(defgeneric push-for-rendering (object function &key force-push))

(defgeneric pop-for-rendering (object))

(defgeneric rendering-must-wait-p (object))

(defgeneric sync (object))

(defmethod events-polling-p ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (eq (event-loop-type object) :polling))

(defun quit-sentinel (dt)
  (declare (ignore dt))
  (sdl2:push-event :quit))

(defmethod quit-sdl ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (push-for-rendering object
                      #'quit-sentinel
                      :force-push t)
  (bt:join-thread (rendering-thread object)))

(u:definline push-in-queue (context queue function force-push)
  (if (events-polling-p context)
      (if force-push
          (q:push-forced queue function)
          (q:push queue function))
      (bq:push-unblock queue function)))

(defmethod push-for-rendering ((object context) (function function) &key (force-push nil))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (push-in-queue object (rendering-queue object) function force-push))

(u:definline pop-from-queue (context queue)
  (if (events-polling-p context)
      (q:pop queue)
      (bq:pop-block queue)))

(defmethod pop-for-rendering ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (pop-from-queue object (rendering-queue object)))

(u:definline queue-empty-p (context queue)
  (if (events-polling-p context)
      (q:emptyp queue)
      (bq:emptyp queue)))

(defmethod rendering-must-wait-p ((object context))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (queue-empty-p object (rendering-queue object)))

(defmethod sync ((object context))
  (sdl2:delay (minimum-delta-t object)))

(defun make-sdl-frame (width height &rest args)
  (apply #'make-instance
         (append (list 'nodgui:classic-frame
                       :width  width
                       :height height
                       :background "")
                 args)))
