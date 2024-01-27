(in-package :nodgui.opengl-frame)

(a:define-constant +channels-number+ 4 :test #'=)

(defun initialize-opengl-rendering (window)
  (declare (ignore window))
  (gl:front-face :ccw)
  (gl:enable :depth-test :cull-face)
  (gl:depth-func :less)
  (gl:polygon-mode :front-and-back :fill)
  (gl:clear-color 0 0 0 1)
  (gl:clear-depth 1.0))

(defclass opengl-context (ctx:context)
  ((initialization-function
    :initform #'initialize-opengl-rendering
    :initarg :initialization-function
    :accessor initialization-function
    :type function)))

(defmethod initialize-instance :after ((object opengl-context)
                                       &key
                                       &allow-other-keys)
  (assert (eq (ctx::event-loop-type object) :polling)
          nil
          "only polling has been implemented so far"))

(defun make-rendering-thread (context)
  (nodgui.utils:make-thread
   (let ((sdl-context  context))
     (lambda ()
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       (let ((context  sdl-context))
         (with-accessors ((initialization-function ctx:initialization-function)
                          (width                   ctx:width)
                          (height                  ctx:height)
                          (window                  ctx:window)
                          (window-id               ctx::window-id)
                          (time-spent              ctx::time-spent)
                          (minimum-delta-t         ctx::minimum-delta-t)) context
           (declare (fixnum width height minimum-delta-t))
           (declare (function initialization-function))
           (sdl2:with-init (:everything)
             (setf window
                   (ctx::create-window-from-pointer (ctx::window-id->pointer window-id)))
             (sdl2:with-renderer (renderer window :flags '(:accelerated))
               (sdl2:with-gl-context (gl window)
                 (funcall initialization-function context)
                 (sdl2:with-event-loop (:method :poll)
                   (:idle
                    ()
                    (let* ((millis  (ctx::get-milliseconds))
                           (dt      (to:f- (the fixnum millis)
                                      (the fixnum time-spent))))
                      (if (not (ctx:rendering-must-wait-p context))
                          (if (>= dt minimum-delta-t)
                              (let ((fn (ctx:pop-for-rendering context)))
                                (declare (function fn))
                                (setf time-spent millis)
                                (if (eq fn #'ctx::quit-sentinel)
                                    (funcall fn dt)
                                    (progn
                                      (gl:clear :color-buffer :depth-buffer)
                                      (funcall fn dt)
                                      (gl:flush)
                                      (sdl2:gl-swap-window window))))
                              (sdl2:delay (to:f- minimum-delta-t dt)))
                          (sdl2:delay minimum-delta-t))))
                   (:quit () t)))))))))))
