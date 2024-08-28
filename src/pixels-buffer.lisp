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
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
  (let ((color (pix:assemble-color r g b a)))
    (set-pixel-color@ buffer width x y color)))

(u:definline pixel@ (buffer width x y)
  "Note: no bounds checking is done"
  (declare (fixnum x y width))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
  pixel-source)

(defun make-blending-fn-replace-with-transparent-color (r-transparent g-transparent b-transparent)
  #.nodgui.config:default-optimization
  (declare ((unsigned-byte 8) r-transparent g-transparent b-transparent))
  (lambda (pixel-source pixel-destination)
    #.nodgui.config:default-optimization
    (declare (fixnum pixel-source pixel-destination))
    (if (not (and (= (pix:extract-red-component pixel-source)   r-transparent)
                  (= (pix:extract-green-component pixel-source) g-transparent)
                  (= (pix:extract-blue-component  pixel-source) b-transparent)))
        pixel-source
        pixel-destination)))

(defun saturate-byte (a)
  (declare (fixnum a))
  (min a #xfe))

(defun blending-function-add (pixel-source pixel-destination)
  (declare (fixnum pixel-source pixel-destination))
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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
             buffer-destination-height
             source-row
             source-column
             destination-row
             destination-column
             source-last-row
             source-last-column)
  (declare ((simple-array (unsigned-byte 32)) buffer-source buffer-destination))
  (declare (fixnum buffer-source-width
                   buffer-destination-width
                   buffer-destination-height
                   source-row
                   source-column
                   destination-row
                   destination-column
                   source-last-row
                   source-last-column))
  #.nodgui.config:default-optimization
  (let* ((source-blitted-w        (the fixnum (- source-last-column source-column)))
         (source-blitted-h        (the fixnum (- source-last-row source-row)))
         (destination-last-column (+ destination-column source-blitted-w))
         (destination-last-row    (+ destination-row source-blitted-h)))
    (when (not (or (< destination-last-row 0)
                   (< destination-last-column 0)
                   (> destination-row buffer-destination-height)
                   (> destination-column buffer-destination-width)))

      (let ((actual-source-row         (if (< destination-row 0)
                                           (- source-row destination-row)
                                           source-row))
            (actual-source-column      (if (< destination-column 0)
                                           (- source-column destination-column)
                                           source-column))
            (actual-source-last-row    (if (> destination-last-row
                                              buffer-destination-height)
                                           (- source-last-row
                                              (- destination-last-row
                                                 buffer-destination-height))
                                           source-last-row))
            (actual-source-last-column (if (> destination-last-column
                                              buffer-destination-width)
                                           (- source-last-column
                                              (- destination-last-column
                                                 buffer-destination-width))
                                           source-last-column))
            (actual-destination-row    (if (< destination-row 0)
                                           0
                                           destination-row))
            (actual-destination-column (if (< destination-column 0)
                                           0
                                           destination-column)))
        (declare (fixnum actual-source-row
                         actual-source-column
                         actual-source-last-row
                         actual-source-last-column
                         actual-destination-row
                         actual-destination-column))
        (flet ((copy-row (row-source row-destination)
                 (declare (fixnum row-source row-destination))
                 (declare (function *blending-function*))
                 #.nodgui.config:default-optimization
                 (loop for from-column fixnum
                       from actual-source-column
                         below actual-source-last-column
                       for to-column fixnum from 0 do
                         (let* ((color-destination (to:faref buffer-destination
                                                     (to:f+ (to:f* row-destination
                                                              buffer-destination-width)
                                                       (to:f+ actual-destination-column
                                                         to-column))))
                                (color-source      (to:faref buffer-source
                                                     (to:f+ (to:f* row-source
                                                              buffer-source-width)
                                                       from-column)))
                                (color (funcall *blending-function* color-source color-destination)))
                           (declare (dynamic-extent color-destination color-source))
                           (setf (to:faref buffer-destination (to:f+ (to:f* row-destination
                                                                       buffer-destination-width)
                                                                (to:f+ actual-destination-column
                                                                  to-column)))
                                 color)))))
          (loop for from-row fixnum from actual-source-row below actual-source-last-row
                for to-row fixnum from actual-destination-row do
                  (copy-row from-row to-row))
          buffer-destination)))))

(defun make-rendering-thread (context)
  (nodgui.utils:make-thread
   (let ((sdl-context  context))
     (lambda ()
       #.nodgui.config:default-optimization
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
    #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
  (let ((color (pix:assemble-color r g b alpha)))
    (loop for i from 0 below (to:f* width height) do
      (setf (aref buffer i) color))))

(defun fill-rectangle (buffer
                       buffer-width
                       buffer-height
                       top-left-x top-left-y
                       bottom-right-x bottom-right-y
                       r g b
                       &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width buffer-height
                   top-left-x top-left-y bottom-right-x bottom-right-y))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (when (and (> bottom-right-x 0)
             (> bottom-right-y 0)
             (< top-left-x buffer-width)
             (< top-left-y buffer-height))
    (let* ((clipped-top-left-x     (if (< top-left-x 0)
                                       0
                                       top-left-x))
           (clipped-top-left-y     (if (< top-left-y 0)
                                       0
                                       top-left-y))
           (clipped-bottom-right-x (if (> bottom-right-x buffer-width)
                                       buffer-width
                                       bottom-right-x))
           (clipped-bottom-right-y (if (> bottom-right-y buffer-height)
                                       buffer-height
                                       bottom-right-y))
           (width                  (to:f- clipped-bottom-right-x clipped-top-left-x))
           (height                 (to:f- clipped-bottom-right-y clipped-top-left-y))
           (w/2                    (ash width  -1))
           (h/2                    (ash height -1))
           (color                  (pix:assemble-color r g b a)))
      (declare (dynamic-extent clipped-top-left-x
                               clipped-top-left-y
                               clipped-bottom-right-x
                               clipped-bottom-right-y
                               width
                               height
                               w/2
                               h/2
                               color))
      (declare (fixnum clipped-top-left-x
                       clipped-top-left-y
                       clipped-bottom-right-x
                       clipped-bottom-right-y
                       width
                       height
                       w/2
                       h/2
                       color))
      (loop for column from clipped-top-left-x below (to:f+ clipped-top-left-x w/2) do
        (loop for row from clipped-top-left-y below (to:f+ clipped-top-left-y h/2) do
          (set-pixel@ buffer buffer-width column             row r g b a)
          (set-pixel-color@ buffer buffer-width (to:f+ column w/2) row color)
          (set-pixel-color@ buffer buffer-width column             (to:f+ row h/2) color)
          (set-pixel-color@ buffer buffer-width column             (to:f+ row h/2) color)
          (set-pixel-color@ buffer buffer-width (to:f+ column w/2) (to:f+ row h/2) color)))
      buffer)))

(defun pixel-inside-buffer-p (width height x y)
  (declare (fixnum width height x y))
  (and (>= x 0)
       (<  x width)
       (>= y 0)
       (<  y height)))

(defun fill-circle (buffer buffer-width buffer-height x-center y-center radius r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width buffer-height x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (let ((r-square  (to:f* radius radius))
        (color     (pix:assemble-color r g b a)))
    (declare (dynamic-extent r-square))
    (loop for x from 0 below radius do
      (let ((x-square (to:f* x x)))
        (declare (dynamic-extent x-square))
        (loop named inner for y fixnum from 0 below radius do
          (let ((condition (to:f<= (to:f+ (to:f* y y)
                                     x-square)
                             r-square)))
            (if condition
                (let ((x-top-left     (to:f+ x-center (to:f- x)))
                      (y-top-left     (to:f+ y-center        y))
                      (x-bottom-right (to:f+ x-center        x))
                      (y-bottom-right (to:f+ y-center (to:f- y))))
                  (declare (dynamic-extent x-top-left
                                           y-top-left
                                           x-bottom-right
                                           y-bottom-right))
                  (when (pixel-inside-buffer-p buffer-width buffer-height
                                               x-bottom-right y-top-left)
                    (set-pixel-color@ buffer buffer-width
                                      x-bottom-right
                                      y-top-left
                                      color))
                  (when (pixel-inside-buffer-p buffer-width buffer-height
                                               x-bottom-right y-bottom-right)
                    (set-pixel-color@ buffer buffer-width
                                      x-bottom-right
                                      y-bottom-right
                                      color))
                  (when (pixel-inside-buffer-p buffer-width buffer-height
                                               x-top-left y-bottom-right)
                    (set-pixel-color@ buffer buffer-width
                                      x-top-left
                                      y-bottom-right
                                      color))
                  (when (pixel-inside-buffer-p buffer-width buffer-height
                                               x-top-left y-top-left)
                    (set-pixel-color@ buffer buffer-width
                                      x-top-left
                                      y-top-left
                                      color)))
                (return-from inner t))))))
    buffer))

(defun fill-circle-insecure (buffer buffer-width x-center y-center radius r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (let ((r-square  (to:f* radius radius))
        (color     (pix:assemble-color r g b a)))
    (declare (dynamic-extent r-square))
    (loop for x from 0 below radius do
      (let ((x-square (to:f* x x)))
        (declare (dynamic-extent x-square))
        (loop named inner for y fixnum from 0 below radius do
          (let ((condition (to:f<= (to:f+ (to:f* y y)
                                     x-square)
                             r-square)))
            (if condition
                (let ((x-top-left     (to:f+ x-center (to:f- x)))
                      (y-top-left     (to:f+ y-center        y))
                      (x-bottom-right (to:f+ x-center        x))
                      (y-bottom-right (to:f+ y-center (to:f- y))))
                  (set-pixel-color@ buffer buffer-width
                                    x-bottom-right
                                    y-top-left
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    x-bottom-right
                                    y-bottom-right
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    x-top-left
                                    y-bottom-right
                                    color)
                  (set-pixel-color@ buffer buffer-width
                                    x-top-left
                                    y-top-left
                                    color))
                (return-from inner t))))))
    buffer))

(a:define-constant +cos-45-degree+ (to:d (cos (/ pi 4))) :test #'=)

(defun draw-circle (buffer buffer-width buffer-height x-center y-center radius r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width buffer-height x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (let ((x-end     (ash (to:f* radius 185364) -18)) ; ~ r * cos(45°)
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

             ;;       E      |     D                 |
             ;;          ----+----                   |
             ;;       --/    |    \--                |
             ;;      X       |       X-              |
             ;;     / -\     |     /- \  C           |
             ;; F  /    -\   |   /-    \             |
             ;;   /       -\ |0/-       \            |
             ;; --+----------+----------+-           |
             ;;   \      --/ | \-       /            |
             ;; G  \  --/    |   \-    / B           |
             ;;     -/       |     \- /              |
             ;;      \       |       X               |
             ;;       --\    |    /--                |
             ;;   H      ----+---- A                 | y axe
             ;;              |                       V

             (let* ((xa (to:f+ x-center x))
                    (ya (to:f+ y-center y))
                    (xb (to:f+ x-center y))
                    (yb (to:f+ y-center x))
                    (xh (to:f+ x-center (to:f- x)))
                    (yh ya)
                    (xc xb)
                    (yc (to:f+ y-center (to:f- x)))
                    (xe (to:f+ x-center (to:f- x)))
                    (ye (to:f+ y-center (to:f- y)))
                    (xf (to:f+ x-center (to:f- y)))
                    (yf yc)
                    (xd xa)
                    (yd ye)
                    (xg xf)
                    (yg yb))
               (declare (dynamic-extent xa ya xb yb xh yh xc yc xe ye xf yf xd yd xg yg))
               (when (pixel-inside-buffer-p buffer-width buffer-height xa ya)
                 (set-pixel-color@ buffer buffer-width xa ya color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xb yb)
                 (set-pixel-color@ buffer buffer-width xb yb color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xh yh)
                 (set-pixel-color@ buffer buffer-width xh yh color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xc yc)
                 (set-pixel-color@ buffer buffer-width xc yc color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xe ye)
                 (set-pixel-color@ buffer buffer-width xe ye color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xf yf)
                 (set-pixel-color@ buffer buffer-width xf yf color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xd yd)
                 (set-pixel-color@ buffer buffer-width xd yd color))
               (when (pixel-inside-buffer-p buffer-width buffer-height xg yg)
                 (set-pixel-color@ buffer buffer-width xg yg color))))))

(defun draw-circle-insecure (buffer buffer-width x-center y-center radius r g b &optional (a 255))
  "Note: no bounds checking is done. This function supposed to be faster than `draw-circle'."

  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width x-center y-center radius))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (let ((x-end     (ash (to:f* radius 185364) -18)) ; ~ r * cos(45°)
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

             ;;       E      |     D                 |
             ;;          ----+----                   |
             ;;       --/    |    \--                |
             ;;      X       |       X-              |
             ;;     / -\     |     /- \  C           |
             ;; F  /    -\   |   /-    \             |
             ;;   /       -\ |0/-       \            |
             ;; --+----------+----------+-           |
             ;;   \      --/ | \-       /            |
             ;; G  \  --/    |   \-    / B           |
             ;;     -/       |     \- /              |
             ;;      \       |       X               |
             ;;       --\    |    /--                |
             ;;   H      ----+---- A                 | y axe
             ;;              |                       V

             (let* ((xa (to:f+ x-center x))
                    (ya (to:f+ y-center y))
                    (xb (to:f+ x-center y))
                    (yb (to:f+ y-center x))
                    (xh (to:f+ x-center (to:f- x)))
                    (yh ya)
                    (xc xb)
                    (yc (to:f+ y-center (to:f- x)))
                    (xe (to:f+ x-center (to:f- x)))
                    (ye (to:f+ y-center (to:f- y)))
                    (xf (to:f+ x-center (to:f- y)))
                    (yf yc)
                    (xd xa)
                    (yd ye)
                    (xg xf)
                    (yg yb))
               (declare (dynamic-extent xa ya xb yb xh yh xc yc xe ye xf yf xd yd xg yg))
               (set-pixel-color@ buffer buffer-width xa ya color)
               (set-pixel-color@ buffer buffer-width xb yb color)
               (set-pixel-color@ buffer buffer-width xh yh color)
               (set-pixel-color@ buffer buffer-width xc yc color)
               (set-pixel-color@ buffer buffer-width xe ye color)
               (set-pixel-color@ buffer buffer-width xf yf color)
               (set-pixel-color@ buffer buffer-width xd yd color)
               (set-pixel-color@ buffer buffer-width xg yg color)))))

(defun calc-octant (x y)
  (declare (fixnum x y))
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
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

(defun line-parallel-to-y-p (x-intersection)
  #.nodgui.config:default-optimization
  (declare (to::desired-type x-intersection))
  (= x-intersection most-negative-single-float))

(defun line-parallel-to-x-p (slope)
  #.nodgui.config:default-optimization
  (declare (to::desired-type slope))
  (= slope most-positive-single-float))

(defun line-equation (x1 y1 x2 y2)
  #.nodgui.config:default-optimization
  (declare (fixnum x1 x2 y1 y2))
  (cond
    ((= x1 x2) ; parallel to y
     (values (to:d 0.0) most-negative-single-float))
    ((= y1 y2) ; parallel to x
     (values most-positive-single-float (to:d y1)))
    (t
     (let ((slope          (/ (to:d (- y2 y1))
                              (to:d (- x2 x1))))
           (x-intersection (/ (- (to:d (the fixnum (* y1 x2)))
                                 (to:d (the fixnum (* y2 x1))))
                              (to:d (- x2 x1)))))
       (values slope x-intersection)))))

(defun clockwise-orientation-p (x0 y0 x1 y1 x2 y2)
  #.nodgui.config:default-optimization
  (declare (fixnum x0 y0 x1 y1 x2 y2))
  (> (- (to:f* (- y1 y0)
          (- x2 x1))
        (to:f* (- y2 y1)
          (- x1 x0)))
     0))

(defun segments-intersect-p (x0 y0 x1 y1 x2 y2 x3 y3)
  (and (not (eq (clockwise-orientation-p x0 y0 x1 y1 x2 y2)
                (clockwise-orientation-p x0 y0 x1 y1 x3 y3)))
       (not (eq (clockwise-orientation-p x2 y2 x3 y3 x0 y0)
                (clockwise-orientation-p x2 y2 x3 y3 x1 y1)))))

(defun intersects-horizontal-axe-p (y-axe axe-width x0 y0 x1 y1)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum y-axe axe-width x0 y0 x1 y1))
  (or (and (= y0 y-axe)
           (<= 0 x0 axe-width))
      (and (= y1 y-axe)
           (<= 0 x1 axe-width))
      (segments-intersect-p 0 y-axe axe-width y-axe x0 y0 x1 y1)))

(defun intersects-x-axe-p (width x0 y0 x1 y1)
  (intersects-horizontal-axe-p 0 width x0 y0 x1 y1))

(defun intersects-x-max-p (height width x0 y0 x1 y1)
  (intersects-horizontal-axe-p height width x0 y0 x1 y1))

(defun intersects-vertical-axe-p (x-axe axe-height x0 y0 x1 y1)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum x-axe axe-height x0 y0 x1 y1))
  (or (and (= x0 x-axe)
           (<= 0 y0 axe-height))
      (and (= x1 x-axe)
           (<= 0 y1 axe-height))
      (segments-intersect-p x-axe 0 x-axe axe-height x0 y0 x1 y1)))

(defun intersects-y-axe-p (height x0 y0 x1 y1)
  (intersects-vertical-axe-p 0 height x0 y0 x1 y1))

(defun intersects-y-max-p (height width x0 y0 x1 y1)
  (intersects-vertical-axe-p width height x0 y0 x1 y1))

(defun draw-line (buffer buffer-width buffer-height x0 y0 x1 y1 r g b &optional (a 255))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width buffer-height x0 y0 x1 y1))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((width-limit      (1- buffer-width))
         (height-limit     (1- buffer-height))
         (intersects-x-axe (intersects-x-axe-p width-limit x0 y0 x1 y1))
         (intersects-x-max (intersects-x-max-p height-limit width-limit x0 y0 x1 y1))
         (intersects-y-axe (intersects-y-axe-p height-limit x0 y0 x1 y1))
         (intersects-y-max (intersects-y-max-p height-limit width-limit x0 y0 x1 y1))
         (actual-x0 -1)
         (actual-y0 -1)
         (actual-x1 -1)
         (actual-y1 -1))
    (declare (dynamic-extent width-limit
                             height-limit
                             intersects-x-axe
                             intersects-x-max
                             intersects-y-axe
                             intersects-y-max
                             actual-x0
                             actual-y0
                             actual-x1
                             actual-y1))
    (declare (fixnum actual-x0 actual-y0 actual-x1 actual-y1))
    (multiple-value-bind (slope intersection)
        (line-equation x0 y0 x1 y1)
      (let* ((line-parallel-to-x (line-parallel-to-x-p slope))
             (intersection-y-axe (if line-parallel-to-x
                                     (to:d y0)
                                     intersection)) ; 0, intersection
             (intersection-y-max (if line-parallel-to-x
                                     (to:d y0)
                                     (to:d+ (to:d* slope (to:d width-limit))
                                            intersection))) ; buffer-width, intersection
             (line-parallel-to-y (line-parallel-to-y-p intersection))
             (intersection-x-axe (cond
                                   (line-parallel-to-y
                                    (to:d x0)) ; x0, 0
                                   (line-parallel-to-x
                                    -1f0) ; no intersection
                                   (t
                                    (to:d/ (to:d- intersection)
                                           slope)))) ; intersection, 0
             (intersection-x-max (cond
                                   (line-parallel-to-y
                                    (to:d x0)) ; x0, buffer-height
                                   (line-parallel-to-x
                                    -1f0) ;  no intersection
                                   (t
                                    (to:d/ (to:d- (to:d height-limit)
                                                  intersection)
                                           slope))))) ; intersection, buffer-height
        (declare (dynamic-extent line-parallel-to-x
                                 intersection-y-axe
                                 intersection-y-max
                                 line-parallel-to-y
                                 intersection-x-axe
                                 intersection-x-max))
        (declare (to::desired-type intersection-y-axe
                                   intersection-y-max
                                   intersection-x-axe
                                   intersection-x-max))
        (when (or (pixel-inside-buffer-p buffer-width buffer-height x0 y0)
                  (pixel-inside-buffer-p buffer-width buffer-height x1 y1)
                  intersects-x-axe
                  intersects-x-max
                  intersects-y-axe
                  intersects-y-max)
          (cond
            ;; both ends inside the the buffer
            ((and (pixel-inside-buffer-p buffer-width buffer-height x0 y0)
                  (pixel-inside-buffer-p buffer-width buffer-height x1 y1))
             (setf actual-x0 x0
                   actual-y0 y0
                   actual-x1 x1
                   actual-y1 y1))
            ;; only one end inside the the buffer
            ((or (pixel-inside-buffer-p buffer-width buffer-height x0 y0)
                 (pixel-inside-buffer-p buffer-width buffer-height x1 y1))
             (if (pixel-inside-buffer-p buffer-width buffer-height x0 y0)
                 (setf actual-x0 x0
                       actual-y0 y0
                       actual-x1 x1
                       actual-y1 y1)
                 (setf actual-x0 x1
                       actual-y0 y1
                       actual-x1 x0
                       actual-y1 y0))
             (cond
               (intersects-y-max
                (if line-parallel-to-y
                    (setf actual-y1 (a:clamp actual-y1 0 height-limit)
                          actual-x1 width-limit)
                    (setf actual-y1 (truncate intersection-y-max)
                          actual-x1 width-limit)))
               (intersects-y-axe
                (if line-parallel-to-y
                    (setf actual-y1 (a:clamp actual-y1 0 height-limit)
                          actual-x1 0)
                    (setf actual-y1 (truncate intersection-y-axe)
                          actual-x1 0)))
               (intersects-x-max
                (if line-parallel-to-x
                    (setf actual-y1 height-limit
                          actual-x1 (a:clamp actual-x1 0 width-limit))
                    (setf actual-y1 height-limit
                          actual-x1 (truncate intersection-x-max))))
               (intersects-x-axe
                (if line-parallel-to-x
                    (setf actual-y1 0
                          actual-x1 (a:clamp actual-x1 0 width-limit))
                    (setf actual-y1 0
                          actual-x1 (truncate intersection-x-axe))))))
            (t ; both ends outside the the buffer
             (when intersects-y-max
               (if (< actual-x0 0)
                   (setf actual-y0 (truncate intersection-y-max)
                         actual-x0 width-limit)
                   (setf actual-y1 (truncate intersection-y-max)
                         actual-x1 width-limit)))
             (when intersects-y-axe
               (if (< actual-x0 0)
                   (setf actual-y0 (truncate intersection-y-axe)
                         actual-x0 0)
                   (setf actual-y1 (truncate intersection-y-axe)
                         actual-x1 0)))
             (when intersects-x-max
               (if (< actual-x0 0)
                   (setf actual-y0 height-limit
                         actual-x0 (truncate intersection-x-max))
                   (setf actual-y1 height-limit
                         actual-x1 (truncate intersection-x-max))))
             (when intersects-x-axe
               (if (< actual-x0 0)
                   (setf actual-y0 0
                         actual-x0 (truncate intersection-x-axe))
                   (setf actual-y1 0
                         actual-x1 (truncate intersection-x-axe))))))
          (draw-line-insecure buffer
                              buffer-width
                              actual-x0
                              actual-y0
                              actual-x1
                              actual-y1
                              r
                              g
                              b
                              a))))))

(defun combine-pixel-colors (buffer buffer-width color-source x y)
  #.nodgui.config:default-optimization
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare ((unsigned-byte 32) color-source))
  (declare (fixnum x y))
  (let* ((color-destination (pixel@ buffer
                                    buffer-width
                                    x
                                    y))
         (color             (funcall *blending-function*
                                     color-source
                                     color-destination)))
    (declare (dynamic-extent color-destination))
    (declare (function *blending-function*))
    (declare ((unsigned-byte 32) color color-destination))
    (set-pixel-color@ buffer
                      buffer-width
                      x
                      y
                      color)))

(defun draw-line-insecure (buffer buffer-width x0 y0 x1 y1 r g b &optional (a 255))
  "No bound checking, faster than `DRAW-LINE`."
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum buffer-width x0 y0 x1 y1))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (let ((octant (calc-octant (to:f- x1 x0)
                             (to:f- y1 y0))))
    (declare (dynamic-extent octant))
    (multiple-value-bind (first-octant-x first-octant-y)
        (to-first-octant octant
                         (to:f- x1 x0)
                         (to:f- y1 y0))
      (let* ((delta-x      (the fixnum first-octant-x))
             (delta-y      (the fixnum first-octant-y))
             (2dx          (ash delta-x 1))
             (2dy          (ash delta-y 1))
             (threshold    (to:f- 2dy delta-x))
             (color-source (pix:assemble-color r g b a)))
        (declare (dynamic-extent delta-x
                                 delta-y
                                 2dx
                                 2dy
                                 threshold))
        (loop with x fixnum = 0
              with y fixnum = 0
              while (to:f<= x delta-x)
              do
                 (multiple-value-bind (first-octant-x first-octant-y)
                     (from-first-octant octant x y)
                   (declare (dynamic-extent first-octant-x
                                            first-octant-y))
                   (let* ((actual-x          (to:f+ (the fixnum first-octant-x) x0))
                          (actual-y          (to:f+ (the fixnum first-octant-y) y0))
                          (color-destination (pixel@ buffer
                                                     buffer-width
                                                     actual-x
                                                     actual-y))
                          (color             (funcall *blending-function*
                                                      color-source
                                                      color-destination)))
                     (declare (dynamic-extent actual-x actual-y color-destination))
                     (declare (function *blending-function*))
                     (declare ((unsigned-byte 32) color color-destination))
                     (declare (fixnum actual-x actual-y))
                     (set-pixel-color@ buffer
                                       buffer-width
                                       actual-x
                                       actual-y
                                       color)))
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
  #.nodgui.config:default-optimization
  ;; a          b
  ;; +----------+
  ;; |          |
  ;; |          |
  ;; +----------+
  ;; d          c
  (let* ((floor-x   (floor x))
         (floor-y   (floor y))
         (ceiling-x (rem (ceiling x) buffer-width))
         (ceiling-y (rem (ceiling y) buffer-height))
         (dx        (truncate (to:d* 255.0f0 (to:d- x (to:d floor-x)))))
         (dy        (truncate (to:d* 255.0f0 (to:d- y (to:d floor-y)))))
         (a         (pixel@ buffer buffer-width floor-x floor-y))
         (b         (pixel@ buffer buffer-width ceiling-x floor-y))
         (c         (pixel@ buffer buffer-width ceiling-x ceiling-y))
         (d         (pixel@ buffer buffer-width floor-x ceiling-y))
         (inter-x1  (color-lerp c d dx))
         (inter-x2  (color-lerp b a dx))
         (inter-y   (color-lerp inter-x1 inter-x2 dy)))
    (declare (fixnum floor-x floor-y
                     ceiling-x ceiling-y
                     dx dy
                     a b c d
                     inter-x1 inter-x2
                     inter-y))
    (declare (dynamic-extent floor-x floor-y
                             ceiling-x ceiling-y
                             dx dy
                             a b c d
                             inter-x1 inter-x2
                             inter-y))
    inter-y))

(u:definline translate (coordinate delta)
  (declare (fixnum coordinate delta))
  #.nodgui.config:default-optimization
  (to:f+ coordinate delta))

(u:definline float-translate (coordinate delta)
  (declare (to::desired-type coordinate delta))
  #.nodgui.config:default-optimization
  (to:d+ coordinate delta))

(u:definline rotate-sin-cos (x y sin-angle cosin-angle)
  (declare (fixnum x y))
  (declare (to::desired-type sin-angle cosin-angle))
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
  (let ((sin-angle   (to:dsin angle))
        (cosin-angle (to:dcos angle)))
    (rotate-sin-cos x y sin-angle cosin-angle)))

(u:definline float-rotate-sin-cos (x y sin-angle cosin-angle)
  (declare (to::desired-type x y sin-angle cosin-angle))
  #.nodgui.config:default-optimization
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
  #.nodgui.config:default-optimization
  (let ((sin-angle   (to:dsin angle))
        (cosin-angle (to:dcos angle)))
    (float-rotate-sin-cos (to:d x) (to:d y) sin-angle cosin-angle)))

(defun float-coordinate-scale (coordinate factor)
  (declare (to::desired-type coordinate factor))
  #.nodgui.config:default-optimization
  (to:d* coordinate factor))

(defun coordinate-scale (coordinate factor)
  (declare (fixnum coordinate))
  (declare (to::desired-type factor))
  #.nodgui.config:default-optimization
  (truncate (the to::desired-type (float-coordinate-scale (to:d coordinate) factor))))

(defun texture-border-clamp (v)
  #.nodgui.config:default-optimization
  (declare (to::desired-type v))
  (cond
    ((< v 0f0)
     0f0)
    ((> v 1f0)
     1f0)
    (t
     v)))

(defun texture-border-wrap (v)
  #.nodgui.config:default-optimization
  (declare (to::desired-type v))
  (cond
    ((< v 0f0)
     (to:d- 1f0 (nth-value 1 (truncate v))))
    ((> v 1f0)
     (nth-value 1 (truncate v)))
    (t
     v)))

(defun texture-shader-wrap-replace (s-tex
                                    t-tex
                                    texture
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
  (declare ((simple-array (unsigned-byte 32)) buffer texture))
  (let* ((wrapped-s (texture-border-wrap s-tex))
         (wrapped-t (texture-border-wrap t-tex))
         (pixmap-x (truncate (to:d* wrapped-s
                                    (1- texture-width))))
         (pixmap-y (truncate (to:d* wrapped-t
                                    (1- texture-height))))
         (pixel    (pixel@ texture
                           (truncate texture-width)
                           pixmap-x
                           pixmap-y)))
    (declare (fixnum pixmap-x pixmap-y))
    (declare ((unsigned-byte 32) pixel))
    (declare (to::desired-type wrapped-s wrapped-t))
    (set-pixel-color@ buffer buffer-width x-buffer y-buffer pixel)))

(defun texture-shader-wrap-replace-bilinear (s-tex
                                             t-tex
                                             texture
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
  (declare ((simple-array (unsigned-byte 32)) buffer texture))
  (let* ((wrapped-s (texture-border-wrap s-tex))
         (wrapped-t (texture-border-wrap t-tex))
         (pixmap-x  (to:d* wrapped-s
                           (1- texture-width)))
         (pixmap-y  (to:d* wrapped-t
                           (1- texture-height)))
         (pixel     (bilinear-interpolation texture
                                            (truncate texture-width)
                                            (truncate texture-height)
                                            pixmap-x
                                            pixmap-y)))
    (declare ((unsigned-byte 32) pixel))
    (declare (to::desired-type wrapped-s wrapped-t pixmap-x pixmap-y))
    (set-pixel-color@ buffer buffer-width x-buffer y-buffer pixel)))

(defparameter *texture-shader* #'texture-shader-wrap-replace)

(defstruct polygon-intersection
  point
  start-vertex
  end-vertex
  start-texel
  end-texel)

(u:definline iaabb2-min-y (aabb)
  (elt aabb 1))

(u:definline iaabb2-max-y (aabb)
  (elt aabb 3))

(u:definline iaabb2-min-x (aabb)
  (elt aabb 0))

(u:definline iaabb2-max-x (aabb)
  (elt aabb 2))

(defsetf iaabb2-min-x (aabb) (new-val)
  `(setf (elt ,aabb 0) ,new-val))

(defsetf iaabb2-min-y (aabb) (new-val)
  `(setf (elt ,aabb 1) ,new-val))

(defsetf iaabb2-max-x (aabb) (new-val)
  `(setf (elt ,aabb 2) ,new-val))

(defsetf iaabb2-max-y (aabb) (new-val)
  `(setf (elt ,aabb 3) ,new-val))

(defun make-polygon-vertex-array (initial-contents)
  (let ((array (u:make-array-frame (length initial-contents)
                                   (vec2:vec2-insecure 0f0 0f0)
                                   'vec2:vec2
                                   t)))
    (loop for initial-value in initial-contents
          for i from 0 do
            (setf (aref array i) initial-value))
    array))

(defun make-polygon-texture-coordinates-array (initial-contents)
  (make-polygon-vertex-array initial-contents))

(defun make-iaabb2 (min-x min-y max-x max-y)
  (make-array 4
              :element-type 'fixnum
              :initial-contents (list min-x min-y max-x max-y)
              :adjustable nil))

(defun iaabb2= (a b)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) a b))
  (and (= (elt a 0) (elt b 0))
       (= (elt a 1) (elt b 1))
       (= (elt a 2) (elt b 2))
       (= (elt a 3) (elt b 3))))

(defun valid-iaabb2-p (aabb)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (and (>= (elt aabb 0) 0)
       (>= (elt aabb 1) 0)
       (>= (elt aabb 2) 0)
       (>= (elt aabb 3) 0)
       (> (elt aabb 2) (elt aabb 0))
       (> (elt aabb 3) (elt aabb 1))))

(defun copy-iaabb2 (original)
  (a:copy-array original
                :element-type 'fixnum
                :adjustable nil
                :fill-pointer nil))

(defun expand-iaabb2 (aabb coord)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (declare (vec2:uivec2 coord))
  (when (< (elt coord 0) (elt aabb 0))
    (setf (elt aabb 0) (elt coord 0)))
  (when (> (elt coord 0) (elt aabb 2))
    (setf (elt aabb 2) (elt coord 0)))
  (when (< (elt coord 1) (elt aabb 1))
    (setf (elt aabb 1) (elt coord 1)))
  (when (> (elt coord 1) (elt aabb 3))
    (setf (elt aabb 3) (elt coord 1)))
  aabb)

(defun union-iaabb2 (aabb aabb2)
  (expand-iaabb2 aabb (subseq aabb2 0 2))
  (expand-iaabb2 aabb (vec2:uivec2 (elt aabb2 2) (elt aabb2 1)))
  (expand-iaabb2 aabb (vec2:uivec2 (elt aabb2 2) (elt aabb2 3)))
  (expand-iaabb2 aabb (vec2:uivec2 (elt aabb2 0) (elt aabb2 3)))
  aabb)

(defun iaabb2->irect2 (coords)
  "(upper-left-x upper-left-y bottom-right-x bottom-right-y) to
   (upper-left-x upper-left-y  w h)"
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) coords))
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (x2 (elt coords 2))
        (y2 (elt coords 3)))
    (declare (fixnum x1 x2 y1 y2))
    (make-iaabb2 x1
                 y1
                 (the fixnum (- x2 x1))
                 (the fixnum (- y2 y1)))))

(defun irect2->iaabb2 (coords)
  "(upper-left-x upper-left-y  w h) to
   (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) coords))
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (w  (elt coords 2))
        (h  (elt coords 3)))
    (declare (fixnum x1 y1 w h))
    (make-iaabb2 x1
                 y1
                 (the fixnum (+ x1 w))
                 (the fixnum (+ y1 h)))))

(defun irect2->iaabb2* (coords)
  (irect2->iaabb2 coords))

(defun inside-iaabb2-p (aabb x y)
  "t if x y is inside this bounding box
   aabb is: (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (declare (fixnum x y))
  (and (>= x (elt aabb 0))
       (<= x (elt aabb 2))
       (>= y (elt aabb 1))
       (<= y (elt aabb 3))))

(defun iaabb2-intersect-p (aabb1 aabb2)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb1 aabb2))
  (if (or (>= (iaabb2-min-x aabb1) (iaabb2-max-x aabb2))
          (<= (iaabb2-max-x aabb1) (iaabb2-min-x aabb2))
          (>= (iaabb2-min-y aabb1) (iaabb2-max-y aabb2))
          (<= (iaabb2-max-y aabb1) (iaabb2-min-y aabb2)))
      nil
      t))

(defun iaabb2-inglobe-p (host guest)
  (and (inside-iaabb2-p host (iaabb2-min-x guest) (iaabb2-min-x guest))
       (inside-iaabb2-p host (iaabb2-max-x guest) (iaabb2-max-x guest))))

(defun iaabb2-null-p (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (and (= 0 (elt rect 2))
         (= 0 (elt rect 3)))))

(defun trasl-iaabb2 (aabb &optional (dx (- (elt aabb 0))) (dy (- (elt aabb 1))))
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (declare (fixnum dx dy))
  (make-iaabb2 (the fixnum (+ (elt aabb 0) dx))
               (the fixnum (+ (elt aabb 1) dy))
               (the fixnum (+ (elt aabb 2) dx))
               (the fixnum (+ (elt aabb 3) dy))))

(defun trasl-irect2 (rect &optional (dx (- (elt rect 0))) (dy (- (elt rect 1))))
  (make-iaabb2 (+ (elt rect 0) dx)
               (+ (elt rect 1) dy)
               (elt rect 2)
               (elt rect 3)))

(defun center-iaabb2 (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (vec2:uivec2 (+ (elt rect 0)
                    (truncate (/ (elt rect 2)
                                 2)))
                 (+ (elt rect 1)
                    (truncate (/ (elt rect 3)
                                 2))))))

(defun polygon-create-aabb (vertices)
  #.nodgui.config:default-optimization
  (declare ((simple-array vec2:uivec2) vertices))
  (let ((aabb (make-iaabb2 most-positive-fixnum
                           most-positive-fixnum
                           most-negative-fixnum
                           most-negative-fixnum)))
    (loop for vertex across vertices do
      (expand-iaabb2 aabb vertex))
    aabb))

(defun polygon-calculate-intersection (ray-y start-x start-y end-x end-y)
  #.nodgui.config:default-optimization
  (declare (fixnum ray-y start-x start-y end-x end-y))
  (multiple-value-bind (slope y-intersection)
      (line-equation start-x start-y end-x end-y)
    (declare (to::desired-type slope y-intersection))
    (if (or (line-parallel-to-x-p slope)
            (line-parallel-to-y-p y-intersection))
        end-x
        (truncate (/ (- (to:d ray-y)
                        y-intersection)
                     slope)))))

(defun add-intersection-p (ray-y start-y end-y next-y before-y)
  #.nodgui.config:default-optimization
  (declare (fixnum ray-y start-y end-y next-y before-y))
  (cond
    ((and (/= ray-y end-y)
          (/= ray-y start-y))
     t)
    ((= ray-y end-y)
     ;; if its y *is* equal to y of the ending segment add if the y of
     ;; the next  segment lay  both on different  half plane  wrt the
     ;; ray, add the intersection
     (and (not (= before-y start-y end-y))
          (or (and (> next-y ray-y)
                   (<=  start-y ray-y))
              (and (<= next-y ray-y)
                   (> start-y ray-y)))))
    (t
     nil)))

(u:definline exists-intersection-with-segment-p (ray-y start-y end-y)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum ray-y start-y end-y))
  (not (or (and (> ray-y start-y)
                (> ray-y end-y))
           (and (< ray-y start-y)
                (< ray-y end-y)))))

(defun polygon-collect-intersections (vertices ray-y)
  (declare ((simple-array vec2:uivec2) vertices))
  (declare (fixnum ray-y))
  #.nodgui.config:default-optimization
  (let ((intersections '())
        (vertices-length (length vertices)))
    (loop for offset from 0 by 1 below vertices-length do
      (let* ((start-segment  (aref vertices offset))
             (end-segment    (aref vertices (rem (1+ offset)
                                                 vertices-length)))
             (next-segment   (aref vertices (rem (+ 2 offset)
                                                 vertices-length)))
             (before-segment (aref vertices (rem (+ (1- offset)
                                                    vertices-length)
                                                 vertices-length)))
             (start-x        (vec2:uivec2-x start-segment))
             (start-y        (vec2:uivec2-y start-segment))
             (end-x          (vec2:uivec2-x end-segment))
             (end-y          (vec2:uivec2-y end-segment))
             (next-y         (vec2:uivec2-y next-segment))
             (before-y       (vec2:uivec2-y before-segment)))
        (declare (fixnum start-x start-y end-x end-y next-y before-y))
        (declare (dynamic-extent start-segment end-segment
                                 start-x start-y end-x end-y next-y before-y))
        ;; discard if ray does not intersects segment
        (when (exists-intersection-with-segment-p ray-y start-y end-y)
          (let ((intersection (polygon-calculate-intersection ray-y
                                                              start-x
                                                              start-y
                                                              end-x
                                                              end-y)))
            ;; add intersection
            (when (add-intersection-p ray-y start-y end-y next-y before-y)
              (push intersection intersections))))))
    (sort intersections #'<)))

(defun clip-y-polygon-aabb (aabb buffer-height)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (declare (fixnum buffer-height))
  (when (< (iaabb2-min-y aabb) 0)
    (setf (iaabb2-min-y aabb) 0))
  (when (> (iaabb2-max-y aabb) buffer-height)
    (setf (iaabb2-max-y aabb) buffer-height))
  aabb)

(defun aabb-outside-buffer-p (width height aabb)
  #.nodgui.config:default-optimization
  (declare ((simple-array fixnum) aabb))
  (not (or (pixel-inside-buffer-p width height
                                  (iaabb2-min-x aabb)
                                  (iaabb2-min-y aabb))
           (pixel-inside-buffer-p width height
                                  (iaabb2-max-x aabb)
                                  (iaabb2-min-y aabb))
           (pixel-inside-buffer-p  width height
                                   (iaabb2-max-x aabb)
                                   (iaabb2-max-y aabb))
           (pixel-inside-buffer-p width height
                                  (iaabb2-min-x aabb)
                                  (iaabb2-max-y aabb)))))

(defun float-vertices->fixnum-vertices (vertices)
  #.nodgui.config:default-optimization
  (declare ((simple-array vec2:vec2) vertices))
  (map 'vector
       (lambda (a) (vec2:uivec2 (round (the to::desired-type (vec2:vec2-x a)))
                                (round (the to::desired-type (vec2:vec2-y a)))))
       vertices))

(defun draw-polygon (buffer width height vertices color)
  "Note: vertices must be presented in counterclockwise order."
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum width))
  (declare ((simple-array vec2:vec2) vertices))
  (declare ((unsigned-byte 32) color))
  #.nodgui.config:default-optimization
  (let* ((actual-vertices (float-vertices->fixnum-vertices vertices))
         (aabb            (polygon-create-aabb actual-vertices)))
    (declare ((simple-array fixnum) aabb))
    (declare (dynamic-extent aabb))
    (when (not (aabb-outside-buffer-p width height aabb))
      (clip-y-polygon-aabb aabb height)
      (loop for y fixnum from (iaabb2-min-y aabb) below (iaabb2-max-y aabb) by 1 do
        (let ((intersections (polygon-collect-intersections actual-vertices y)))
          (declare (dynamic-extent intersections))
          (loop for (intersection-a intersection-b) on intersections by 'cddr
                ;; notes that, in principle the number of intersection
                ;; of  ray with  a  polygon is  always  even, so  this
                ;; checks shuld not exists. :( but i found some corner
                ;; case where the algorith i  worte fails and this patch
                ;; prevents    crash    or   artifacts.    The    same
                ;; considerations apply for texture mapped polygons.
                when intersection-b
                  do
                     (when (< (the fixnum intersection-a) 0)
                       (setf intersection-a 0))
                     (when (> (the fixnum intersection-b) width)
                       (setf intersection-b width))
                     (loop for pixel-x fixnum
                           from intersection-a below intersection-b
                           by 1
                           do
                              (combine-pixel-colors buffer width color pixel-x y))))))))

(defun polygon-collect-intersections-texture (vertices texels ray-y)
  (declare ((simple-array vec2:uivec2) vertices))
  (declare ((simple-array vec2:vec2) texels))
  (declare (fixnum ray-y))
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 3) (safety 3)))
  (let ((intersections '())
        (vertices-length (length vertices))
        (texels-length   (length texels)))
    (loop for offset from 0 by 1 below (length vertices) do
      (let* ((start-segment (aref vertices offset))
             (end-segment   (aref vertices (rem (1+ offset)
                                                vertices-length)))
             (next-segment  (aref vertices (rem (+ 2 offset)
                                                vertices-length)))
             (before-segment (aref vertices (rem (+ (1- offset)
                                                    vertices-length)
                                                 vertices-length)))
             (start-texel   (aref texels offset))
             (end-texel     (aref texels (rem (1+ offset)
                                              texels-length)))
             (start-x       (vec2:uivec2-x start-segment))
             (start-y       (vec2:uivec2-y start-segment))
             (end-x         (vec2:uivec2-x end-segment))
             (end-y         (vec2:uivec2-y end-segment))
             (next-y        (vec2:uivec2-y next-segment))
             (before-y      (vec2:uivec2-y before-segment)))
        (declare (fixnum start-x start-y end-x end-y next-y before-y))
        (declare (dynamic-extent start-segment end-segment
                                 start-x start-y
                                 end-x end-y
                                 next-y before-y))
        ;; discard if ray does not intersects segment
        (when (exists-intersection-with-segment-p ray-y start-y end-y)
          (let ((intersection (polygon-calculate-intersection ray-y
                                                              start-x
                                                              start-y
                                                              end-x
                                                              end-y)))
            ;; add intersection
            (when (add-intersection-p ray-y start-y end-y next-y before-y)
              (push (make-polygon-intersection :point        intersection
                                               :start-vertex start-segment
                                               :end-vertex   end-segment
                                               :start-texel  start-texel
                                               :end-texel    end-texel)
                    intersections))))))
    (sort intersections
          (lambda (a b)
            (<= (the fixnum (polygon-intersection-point a))
                (the fixnum (polygon-intersection-point b)))))))

(defun draw-texture-mapped-polygon (buffer width height vertices texels pixmap)
  "Note: vertices must be provided in counterclockwise order."
  #.nodgui.config:default-optimization
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum width))
  (declare ((simple-array vec2:vec2) vertices texels))
  (declare (function *texture-shader*))
  (let ((texture         (pix:bits pixmap))
        (pixmap-width    (to:d (the fixnum (pix:width pixmap))))
        (pixmap-height   (to:d (the fixnum (pix:height pixmap)))))
    (declare ((simple-array (unsigned-byte 32)) texture))
    (declare (to::desired-type pixmap-width pixmap-height))
    (declare (dynamic-extent texture pixmap-width pixmap-height))
    (draw-texture-mapped-polygon* buffer width height vertices texels
                                  texture
                                  pixmap-width
                                  pixmap-height)))

(a:define-constant +epsilon-delta+ 1e-12 :test #'=)

(defun draw-texture-mapped-polygon* (buffer width height vertices texels
                                     texture
                                     texture-width
                                     texture-height)
  "Note: vertices must be provided in counterclockwise order."
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare ((simple-array (unsigned-byte 32)) buffer))
  (declare (fixnum width))
  (declare ((simple-array vec2:vec2) vertices texels))
  (declare (function *texture-shader*))
  (declare ((simple-array (unsigned-byte 32)) texture))
  (let* ((actual-vertices (float-vertices->fixnum-vertices vertices))
         (aabb            (polygon-create-aabb actual-vertices)))
    (declare ((simple-array fixnum) aabb))
    (declare (dynamic-extent aabb))
    (clip-y-polygon-aabb aabb height)
    (loop for y fixnum
          from (iaabb2-min-y aabb) below (iaabb2-max-y aabb) by 1
          do
             (let ((intersections (polygon-collect-intersections-texture actual-vertices texels y)))
               (declare (dynamic-extent intersections))
               (loop for (intersection-a intersection-b) on intersections by 'cddr
                     when intersection-b do
                       (let* ((start-vertex-a   (polygon-intersection-start-vertex intersection-a))
                              (end-vertex-a     (polygon-intersection-end-vertex intersection-a))
                              (start-vertex-b   (polygon-intersection-start-vertex intersection-b))
                              (end-vertex-b     (polygon-intersection-end-vertex intersection-b))
                              (start-texel-a    (polygon-intersection-start-texel intersection-a))
                              (end-texel-a      (polygon-intersection-end-texel intersection-a))
                              (start-texel-b    (polygon-intersection-start-texel intersection-b))
                              (end-texel-b      (polygon-intersection-end-texel intersection-b))
                              (intersection-a-x (polygon-intersection-point intersection-a))
                              (intersection-b-x (polygon-intersection-point intersection-b))
                              (start-b-y        (vec2:uivec2-y start-vertex-b))
                              (end-b-y          (vec2:uivec2-y end-vertex-b))
                              (texel-weight-a   (/ (to:d (- (the fixnum (vec2:uivec2-y start-vertex-a))
                                                            y))
                                                   (to:d+ (to:d (- (the fixnum (vec2:uivec2-y start-vertex-a))
                                                                   (the fixnum (vec2:uivec2-y end-vertex-a))))
                                                          +epsilon-delta+)))
                              (texel-weight-b   (/ (to:d (- start-b-y y))
                                                   (to:d+ (to:d (- start-b-y
                                                                   end-b-y))
                                                          +epsilon-delta+)))
                              (texel-t-start-a  (vec2:vec2-y start-texel-a))
                              (texel-s-start-a  (vec2:vec2-x start-texel-a))
                              (texel-t-end-a    (vec2:vec2-y end-texel-a))
                              (texel-s-end-a    (vec2:vec2-x end-texel-a))
                              (texel-t-start-b  (vec2:vec2-y start-texel-b))
                              (texel-s-start-b  (vec2:vec2-x start-texel-b))
                              (texel-t-end-b    (vec2:vec2-y end-texel-b))
                              (texel-s-end-b    (vec2:vec2-x end-texel-b))
                              (texel1-t         (to:dlerp texel-weight-a texel-t-start-a texel-t-end-a))
                              (texel1-s         (to:dlerp texel-weight-a texel-s-start-a texel-s-end-a))
                              (texel2-t         (to:dlerp texel-weight-b texel-t-start-b texel-t-end-b))
                              (texel2-s         (if (= end-b-y start-b-y)
                                                    texel-s-end-b
                                                    (to:dlerp texel-weight-b texel-s-start-b texel-s-end-b))))
                         (declare (dynamic-extent start-vertex-a
                                                  end-vertex-a
                                                  start-vertex-b
                                                  end-vertex-b
                                                  start-texel-a
                                                  end-texel-a
                                                  start-texel-b
                                                  end-texel-b
                                                  intersection-a-x
                                                  intersection-b-x
                                                  start-b-y
                                                  end-b-y
                                                  texel-weight-a
                                                  texel-weight-b
                                                  texel-t-start-a
                                                  texel-s-start-a
                                                  texel-t-end-a
                                                  texel-s-end-a
                                                  texel-t-start-b
                                                  texel-s-start-b
                                                  texel-t-end-b
                                                  texel-s-end-b
                                                  texel1-t
                                                  texel1-s
                                                  texel2-t
                                                  texel2-s))
                         (declare (vec2:uivec2
                                   start-vertex-a
                                   end-vertex-a
                                   start-vertex-b
                                   end-vertex-b))
                         (declare (fixnum intersection-a-x
                                          intersection-b-x
                                          start-b-y
                                          end-b-y ))
                         (declare (to::desired-type texel-weight-a
                                                    texel-weight-b
                                                    texel-t-start-a
                                                    texel-s-start-a
                                                    texel-t-end-a
                                                    texel-s-end-a
                                                    texel-t-start-b
                                                    texel-s-start-b
                                                    texel-t-end-b
                                                    texel-s-end-b
                                                    texel1-t
                                                    texel1-s
                                                    texel2-t
                                                    texel2-s))
                         (let ((range (- intersection-b-x intersection-a-x)))
                           (declare (fixnum range))
                           (loop for x fixnum from (max 0 intersection-a-x)
                                   below (min intersection-b-x width)
                                 by 1
                                 with delta-t = (to:d- texel2-t texel1-t)
                                 with delta-s = (to:d- texel2-s texel1-s)
                                 with actual-range = (to:d+ (to:d range)
                                                            +epsilon-delta+)
                                 with t-increment = (to:d/ delta-t actual-range)
                                 with s-increment = (to:d/ delta-s actual-range)
                                 for interpolated-t = (to:d+ texel1-t
                                                             (to:d* t-increment
                                                                    (to:d- (min 0f0
                                                                                (to:d intersection-a-x)))))
                                   then (to:d+ interpolated-t
                                               t-increment)
                                 for interpolated-s = (to:d+ texel1-s
                                                             (to:d* s-increment
                                                                    (to:d- (min 0f0
                                                                                (to:d intersection-a-x)))))
                                   then (to:d+ interpolated-s
                                               s-increment)
                                 do
                                    (funcall *texture-shader*
                                             interpolated-s
                                             interpolated-t
                                             texture
                                             texture-width
                                             texture-height
                                             buffer
                                             width
                                             height
                                             x
                                             y)))))))))

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
                         (translate-x 0.0)
                         (translate-y 0.0))
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
  (declare (to::desired-type translate-x translate-y rotation scaling-row scaling-column))
  (declare (function *blending-function*))
  #.nodgui.config:default-optimization
  (let* ((tex-bottom-left-s         (to:d/ (to:d source-column)
                                           (to:d buffer-source-width)))
         (tex-bottom-left-t         (to:d/ (to:d source-row)
                                           (to:d buffer-source-height)))
         (tex-top-right-s           (to:d/ (to:d source-last-column)
                                           (to:d buffer-source-width)))
         (tex-top-right-t           (to:d/ (to:d source-last-row)
                                           (to:d buffer-source-height)))
         (texture-init-coordinates  (list (vec2:vec2-insecure tex-bottom-left-s tex-bottom-left-t)
                                          (vec2:vec2-insecure tex-top-right-s tex-bottom-left-t)
                                          (vec2:vec2-insecure tex-top-right-s tex-top-right-t)
                                          (vec2:vec2-insecure tex-bottom-left-s tex-top-right-t)))
         (texture-coordinates       (make-polygon-texture-coordinates-array texture-init-coordinates))
         (polygon-width             (to:d (to:f- source-last-column source-column)))
         (polygon-height            (to:d (to:f- source-last-row source-row)))
         (polygon-width/2           (to:d/ polygon-width 2f0))
         (polygon-height/2          (to:d/ polygon-height 2f0))
         (vertices-init-coordinates (list (vec2:vec2-insecure (to:d- polygon-width/2)
                                                              (to:d- polygon-height/2))
                                          (vec2:vec2-insecure polygon-width/2
                                                              (to:d- polygon-height/2))
                                          (vec2:vec2-insecure polygon-width/2
                                                              polygon-height/2)
                                          (vec2:vec2-insecure (to:d- polygon-width/2)
                                                              polygon-height/2)))
         (scaling                  (vec2:vec2-insecure scaling-column scaling-row))
         (pivot-x                  (to:d- (to:d pivot-column)
                                          polygon-width/2))
         (pivot-y                  (to:d- (to:d pivot-row)
                                          polygon-height/2))
         (transformed-vertices     (mapcar (lambda (a)
                                             (vec2:vec2-translate
                                              (vec2:vec2-translate
                                               (vec2:vec2-scaling
                                                (vec2:vec2-rotate (vec2:vec2-translate a
                                                                                       (to:d- pivot-x)
                                                                                       (to:d- pivot-y))
                                                                  (to:degree->radians rotation))
                                                scaling)
                                               (to:d* (to:d pivot-x)
                                                      (vec2:vec2-x scaling))
                                               (to:d* (to:d pivot-y)
                                                      (vec2:vec2-y scaling)))
                                              (to:d+ (to:d translate-x)
                                                     (to:d destination-column)
                                                     (to:d* polygon-width/2 scaling-column))
                                              (to:d+ (to:d translate-y)
                                                     (to:d destination-row)
                                                     (to:d* polygon-height/2 scaling-row))))
                                           vertices-init-coordinates))
         (vertices                 (make-polygon-vertex-array transformed-vertices)))
    (declare (dynamic-extent tex-bottom-left-s
                             tex-bottom-left-t
                             tex-top-right-s
                             tex-top-right-t
                             texture-init-coordinates
                             texture-coordinates
                             polygon-width
                             polygon-height
                             polygon-width/2
                             polygon-height/2
                             vertices-init-coordinates
                             scaling
                             pivot-x
                             pivot-y
                             transformed-vertices
                             vertices))
    (flet ((shader (s-tex
                    t-tex
                    texture
                    texture-width
                    texture-height
                    buffer
                    buffer-width
                    buffer-height
                    x-buffer
                    y-buffer)
             (declare (ignore buffer-height))
             (let* ((wrapped-s (texture-border-wrap s-tex))
                    (wrapped-t (texture-border-wrap t-tex))
                    (texture-x (ceiling (to:d* wrapped-s
                                               (1- texture-width))))
                    (texture-y (ceiling (to:d* wrapped-t
                                               (1- texture-height))))
                    (color-source       (pixel@ texture
                                                (truncate texture-width)
                                                texture-x texture-y))
                    (color-destination  (pixel@ buffer
                                                buffer-width
                                                x-buffer
                                                y-buffer))
                    (color (funcall *blending-function*
                                    color-source
                                    color-destination)))
               (declare (fixnum texture-x texture-y))
               (declare (to::desired-type texture-height texture-width))
               (declare ((simple-array (unsigned-byte 32)) buffer))
               (declare (to::desired-type wrapped-s wrapped-t))
               (set-pixel-color@ buffer buffer-width x-buffer y-buffer color))))
      (let ((*texture-shader* #'shader))
        (draw-texture-mapped-polygon* buffer-destination
                                      buffer-destination-width
                                      buffer-destination-height
                                      vertices
                                      texture-coordinates
                                      buffer-source
                                      (to:d buffer-source-width)
                                      (to:d buffer-source-height))))))

(defun create-new-buffer-with-text (text font r g b a)
  "This functionc creates a heap allocated buffer that need to be freed after use using `nodgui.pixmap:free-buffer-memory'."
  #.nodgui.config:default-optimization
  (let* ((surface        (sdl2-ttf:render-utf8-blended font text r g b a))
         (pixels         (sdl2:surface-pixels surface))
         ;;(surface-width  (sdl2:surface-width surface))
         (surface-height       (sdl2:surface-height surface))
         (surface-pitch        (sdl2:surface-pitch surface))
         (surface-pixels-width (to:f/ surface-pitch 4)))
    (declare (fixnum surface-height surface-pitch surface-pixels-width))
    (declare (dynamic-extent surface pixels surface-height surface-pitch))
    (flet ((extract-channel (raw-pixel shift)
             (declare (fixnum raw-pixel shift))
             #.nodgui.config:default-optimization
             (logand (ash raw-pixel shift)
                     #xff)))
      (let* ((buffer-width  (to:f* surface-height surface-pixels-width))
             (buffer-height surface-height)
             (buffer        (pix:make-buffer buffer-width buffer-height)))
        (declare (fixnum buffer-width buffer-height))
        (declare ((simple-array (unsigned-byte 32)) buffer))
        (loop for index fixnum from 0 below buffer-width do
          (let* ((raw-pixel    (cffi:mem-aref pixels :uint32 index))
                 (alpha        (extract-channel raw-pixel -24))
                 (source-color (pix:assemble-color r g b alpha)))
            (declare (dynamic-extent raw-pixel alpha))
            (setf (elt buffer index) source-color)))
        (sdl2:free-surface surface)
        (values buffer surface-pixels-width buffer-height)))))

(defun draw-text (buffer buffer-width buffer-height text font x y r g b a)
  #.nodgui.config:default-optimization
  (multiple-value-bind (buffer-text buffer-text-width buffer-text-height)
      (create-new-buffer-with-text text font r g b a)
    (let ((*blending-function* #'blending-function-combine))
      (blit buffer-text
            buffer-text-width
            buffer
            buffer-width
            buffer-height
            0
            0
            y
            x
            buffer-text-height
            buffer-text-width)
      (pix:free-buffer-memory buffer-text))))

(defun init-font-system ()
  (sdl2-ttf:init))

(defun terminate-font-system ()
  (when (> (sdl2-ttf:was-init) 0)
    (sdl2-ttf:quit)))

(defun open-font (font-path points-size)
  (sdl2-ttf:open-font font-path points-size))

(defun close-font (font-handle)
  (sdl2-ttf:close-font font-handle))

(defclass sprite-grid ()
  ((buffer
    :initform #()
    :initarg  :buffer
    :accessor buffer)
   (width
    :initform 0
    :initarg  :width
    :accessor width)
   (height
    :initform 0
    :initarg  :height
    :accessor height)
   (cell-width
    :initform 0
    :initarg  :cell-width
    :accessor cell-width)
   (cell-height
    :initform 0
    :initarg  :cell-height
    :accessor cell-height)))

(defmethod initialize-instance :after ((object sprite-grid) &key pixmap &allow-other-keys)
  (when pixmap
    (setf (buffer object) (pix:bits   pixmap)
          (width  object) (pix:width  pixmap)
          (height object) (pix:height pixmap))))

(defgeneric blit-cell (object
                       x-cell
                       y-cell
                       buffer-destination
                       buffer-destination-width
                       buffer-destination-height
                       row-destination
                       column-destination))

(defmethod blit-cell ((object sprite-grid)
                      (x-cell fixnum)
                      (y-cell fixnum)
                      (buffer-destination vector)
                      (buffer-destination-width fixnum)
                      (buffer-destination-height fixnum)
                      (row-destination fixnum)
                      (column-destination fixnum))
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (sprite-grid object))
  (declare ((simple-array (unsigned-byte 32)) buffer-destination))
  (declare (fixnum x-cell
                   y-cell
                   buffer-destination-width
                   buffer-destination-height))
  (with-accessors ((buffer      buffer)
                   (width       width)
                   (height      height)
                   (cell-width  cell-width)
                   (cell-height cell-height)) object
    (declare (fixnum width height cell-width cell-height))
    (declare ((simple-array (unsigned-byte 32)) buffer))
    (let* ((source-row         (* cell-height y-cell))
           (source-column      (* cell-width x-cell))
           (source-last-row    (+ source-row    cell-height))
           (source-last-column (+ source-column cell-width)))
      (declare (fixnum source-row
                       source-column
                       source-last-row
                       source-last-column))
      (blit buffer
            width
            buffer-destination
            buffer-destination-width
            buffer-destination-height
            source-row
            source-column
            row-destination
            column-destination
            source-last-row
            source-last-column))))
