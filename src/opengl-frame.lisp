;; This software is Copyright © cage

;; cage  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.opengl-frame)

(defun default-initialize-function (context)
  (declare (ignore context))
  (gl:front-face :cw)
  (gl:enable :depth-test :cull-face)
  (gl:depth-func :less)
  (gl:polygon-mode :front-and-back :fill)
  (gl:clear-color 0 0 0 1)
  (gl:clear-depth 1.0))

(defclass opengl-context (ctx:context) ())

(defmethod initialize-instance :after ((object opengl-context)
                                       &key
                                         (initialization-function #'default-initialize-function)
                                       &allow-other-keys)
  (assert (eq (ctx::event-loop-type object) :polling)
          nil
          "only polling has been implemented so far")
  (setf (ctx:initialization-function object) initialization-function)
  (setf (ctx:rendering-thread object) (make-rendering-thread object)))

(defun make-rendering-thread (context)
  (nodgui.utils:make-thread
   (let ((sdl-context  context))
     (lambda ()
       #.nodgui.config:default-optimization
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
                                      (gl:clear :color-buffer :depth-buffer)
                                      (funcall rendering-fn dt)
                                      (gl:flush)
                                      (sdl2:gl-swap-window window))))
                              (sdl2:delay (to:f- minimum-delta-t dt)))
                          (sdl2:delay minimum-delta-t))))
                   (:quit () t)))))))))))
