;; This software is Copyright © cage

;; cage  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.demo)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defun random-gaussian-distribution (sigma)
  (labels ((random-in-range ()
             (+ -1.0 (random 2.0)))
           (displ (x1 x2)
             (+ (expt x1 2) (expt x2 2)))
           (pr (w)
             (sqrt (/ (* -2 (log w)) w))))
    (do* ((x1 (random-in-range) (random-in-range))
          (x2 (random-in-range) (random-in-range))
          (w1 (displ x1 x2) (displ x1 x2))
          (w  (pr w1) (pr w1)))
         ((< 0 w1 1.0)
          (list (* x1 w sigma)
                (* x2 w sigma))))))

(defun gaussian-probability (sigma mean)
  (+ (* (expt -1 (random 2))
        (first (random-gaussian-distribution sigma)))
     mean))

(a:define-constant +terrain-size+        64 :test #'=)

(a:define-constant +max-terrain-height+ 128 :test #'=)

(defun pixmap-max-value (pixmap)
  (let ((max-so-far -1))
    (loop for x from 0 below +terrain-size+
          do
             (loop for y from 0 below +terrain-size+
                   do
                      (when (> (elt (pixmap:pixel@ pixmap x y) 0)
                               max-so-far)
                        (setf max-so-far
                              (elt (pixmap:pixel@ pixmap x y) 0)))))
    max-so-far))

(defun make-terrain-pixmap (max-height
                            &optional (pixmap (pixmap::make-pixmap +terrain-size+
                                                                   +terrain-size+
                                                                   (nodgui.ubvec4:ubvec4 0
                                                                                         0
                                                                                         0
                                                                                         255))))
  (let* ((mean-x   (random +terrain-size+))
         (mean-y   (random +terrain-size+))
         (sigma-x  (1+ (random (/ (to:d +terrain-size+) (+ 4 (random 6))))))
         (sigma-y  (1+ (random (/ (to:d +terrain-size+) (+ 4 (random 6)))))))
    (loop repeat 600 do
       (let* ((x     (truncate (gaussian-probability sigma-x mean-x)))
              (y     (truncate (gaussian-probability sigma-y mean-y))))
         (when (and (>= x 0)
                    (< x +terrain-size+)
                    (>= y 0)
                    (< y +terrain-size+))
           (let ((pixel (pixmap:pixel@ pixmap x y)))
             (setf (pixmap:pixel@ pixmap x y)
                   (nodgui.ubvec4:ubvec4 (1+ (elt pixel 0))
                                         (1+ (elt pixel 1))
                                         (1+ (elt pixel 2))
                                         255))))))
    (let ((max (pixmap-max-value pixmap)))
      (if (>= max
              (* 1.1 max-height))
          (smooth-pixmap pixmap max-height)
          (make-terrain-pixmap max-height pixmap)))))

(defun smooth-pixmap (pixmap max-value)
  (flet ((smooth ()
           (with-accessors ((width  pixmap:width)
                            (height pixmap:height)) pixmap
             (loop for x from 0 below height do
               (loop for y from 0 below width do
                 (let* ((right   (rem (1+ x) width))
                        (left    (if (< (1- x) 0)
                                     (1- width)
                                     (1- x)))
                        (top     (rem (1+ y) height))
                        (bottom  (if (< (1- y) 0)
                                     (1- height)
                                     (1- y)))
                        (a       (pixmap:pixel@ pixmap left bottom))
                        (b       (pixmap:pixel@ pixmap left top))
                        (c       (pixmap:pixel@ pixmap right bottom))
                        (d       (pixmap:pixel@ pixmap right top))
                        (e       (pixmap:pixel@ pixmap x     bottom))
                        (f       (pixmap:pixel@ pixmap x      top))
                        (g       (pixmap:pixel@ pixmap right y))
                        (h       (pixmap:pixel@ pixmap left y))
                        (average (truncate (/ (+ (elt a 0)
                                                 (elt b 0)
                                                 (elt c 0)
                                                 (elt d 0)
                                                 (elt e 0)
                                                 (elt f 0)
                                                 (elt g 0)
                                                 (elt h 0))
                                              8))))
                   (setf (pixmap:pixel@ pixmap x y)
                         (nodgui.ubvec4:ubvec4 average average average 255))))))))
    (loop while (> (pixmap-max-value pixmap)
                   max-value)
          do
             (format t "max value ~a~%" (pixmap-max-value pixmap))
             (smooth))
    pixmap))

(defun make-flat-terrain-pixmap ()
  (pixmap::make-pixmap +terrain-size+
                       +terrain-size+
                       (nodgui.ubvec4:ubvec4 0
                                             0
                                             0
                                             255)))

(defun debug-terrain-pixmap (pixmap)
  (loop for x from 0 below (pixmap:width pixmap) do
    (loop for y from 0 below (pixmap:height pixmap) do
      (format *error-output* "~a ~a ~a~%" x y (pixmap:pixel@ pixmap x y)))))

(definline fast-glaref (v offset)
  (cffi:mem-aref (gl::gl-array-pointer v) :float offset))

(definline set-fast-glaref (v offset new-val)
  (setf (cffi:mem-aref (gl::gl-array-pointer v) :float offset)
        new-val)
  new-val)

(definline mock-null-pointer ()
  0)

(defsetf fast-glaref set-fast-glaref)

(defun debug-gl-array (array length)
  (format *error-output* "debug gl array~%")
  (loop for i from 0 below length do
    (format *error-output* "~a~%" (fast-glaref array i)))
  (format *error-output* "debug gl array end~%"))

(defclass transformable ()
  ((projection-matrix
    :initform (vector (matrix:identity-matrix))
    :reader projection-matrix
    :initarg :projection-matrix)
   (model-matrix
    :initform (vector (matrix:identity-matrix))
    :reader model-matrix
    :initarg :model-matrix)
   (view-matrix
    :initform (vector (matrix:identity-matrix))
    :reader view-matrix
    :initarg :view-matrix)))

(defmacro gen-accessors-matrix (name)
  (let ((fn-name (list (alexandria:format-symbol t "~:@(setf~)")
                       (alexandria:format-symbol t "~:@(~a~)" name))))
    `(progn
       (defgeneric ,fn-name (new-value object))
       (defmethod ,fn-name (new-value (object transformable))
         (with-slots (,name) object
           (setf (elt ,name 0) new-value))))))

(gen-accessors-matrix projection-matrix)

(gen-accessors-matrix model-matrix)

(gen-accessors-matrix view-matrix)

(defclass camera (transformable)
  ((target
    :initform (vec3:vec3 0f0 100f0 0f0)
    :accessor target
    :initarg :target
    :documentation "position the camera is pointing to.")
   (pos
    :initform (vec3:vec3 0f0 300f0 -100f0)
    :accessor pos
    :initarg :pos
    :documentation "position the camera.")
   (up
    :initform (vec3:vec3 0f0 1f0 0f0)
    :accessor up
    :initarg :up)))

(defgeneric look-at (object))

(defgeneric build-projection-matrix (object near far fov ratio))

(defgeneric rotate-around-y-axe (object angle))

(defmethod look-at ((object camera))
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (with-accessors ((up  up)
                   (pos pos)
                   (target target)
                   (fading-away-fn fading-away-fn)) object
    (declare (function fading-away-fn))
    (let ((standard-vw-matrix (matrix:look@ pos target up)))
      (declare (matrix:matrix standard-vw-matrix))
      (setf (view-matrix object) standard-vw-matrix))))

(defparameter *far*  1800.0)

(defparameter *near*    5.0)

(defparameter *fov*    50.0)

(defmethod build-projection-matrix ((object camera) near far fov ratio)
  (setf (projection-matrix object)
        (matrix:perspective fov ratio near far)))

(defmethod rotate-around-y-axe ((object camera) angle)
  (with-accessors ((pos pos)
                   (target target)) object
    (let* ((dir      (vec3:vec3- target pos))
           (rotation (matrix:rotate-around (vec3:vec3 0f0 1f0 0f0)
                                           (to:degree->radians angle))))
      (setf target
            (vec3:vec3+ pos
                        (vec3:transform-point dir rotation)))
      object)))

(defparameter *camera* (make-instance 'camera))

(defmacro with-camera-view-matrix ((matrix &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (view-matrix *camera*))
                        `(elt (the (simple-array simple-array (1))
                                   (view-matrix *camera*))
                              0))))
     ,@body))

(defmacro with-camera-projection-matrix ((matrix &key (wrapped nil)) &body body)
  `(let* ((,matrix ,(if wrapped
                        `(the (simple-array simple-array (1))
                              (projection-matrix *camera*))
                        `(elt (the (simple-array simple-array (1))
                                   (projection-matrix *camera*))
                              0))))
     ,@body))

(defun init-vertices-slot (&optional (length 0))
  (make-array-frame length vec3:+vec3-zero+ 'vec3:vec3 nil))

(defclass terrain-mesh (transformable)
  ((pixmap-heightmap
    :initform (make-terrain-pixmap +max-terrain-height+)
    :initarg  :pixmap-heightmap
    :accessor pixmap-heightmap)
   (renderer-data-vertices
    :initform nil
    :initarg :renderer-data-vertices
    :accessor renderer-data-vertices)
   (renderer-data-count-vertices
    :initform 0
    :initarg :renderer-data-count-vertices
    :accessor renderer-data-count-vertices
    :type (unsigned-byte 64))
   (vertices
    :initform (init-vertices-slot)
    :initarg :vertices
    :accessor vertices)
   (vertices-count
    :initform 0
    :initarg :vertices-count
    :accessor vertices-count)
   (modelview-matrix
    :initform (matrix:identity-matrix)
    :initarg :modelview-matrix
    :accessor modelview-matrix)
   (vao
    :initform '()
    :initarg :vao
    :accessor vao)
   (vbo
    :initform '()
    :initarg :vbo
    :accessor vbo)
   (shader-program
    :initform nil
    :initarg  :shader-program
    :accessor shader-program)
   (modelview-matrix-uniform-location
    :initform -1
    :accessor modelview-matrix-uniform-location)
   (projection-matrix-uniform-location
    :initform -1
    :accessor projection-matrix-uniform-location)))

(defun get-shader-files ()
  (mapcar (lambda (a) (uiop:native-namestring (asdf:component-pathname a)))
          (remove-if-not (lambda (x) (typep x 'asdf:static-file))
                         (asdf:component-children (asdf:find-system "nodgui")))))

(defun get-shader (re)
  (let ((shader-path (find-if (lambda (a) (cl-ppcre:scan re a))
                              (get-shader-files))))
    (a:read-file-into-string shader-path)))

(defun get-vertex-shader ()
  (get-shader "vert$"))

(defun get-fragment-shader ()
  (get-shader "frag$"))

(defparameter *vertex-shader* (get-vertex-shader))

(defparameter *fragment-shader* (get-fragment-shader))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun init-shaders ()
  (let ((v-shader (gl:create-shader :vertex-shader))
        (f-shader (gl:create-shader :fragment-shader)))
    (compile-and-check-shader v-shader *vertex-shader*)
    (compile-and-check-shader f-shader *fragment-shader*)
    (let ((program (gl:create-program)))
      (if (zerop program)
          (error "Error creating program")
          (progn
            (gl:attach-shader program v-shader)
            (gl:attach-shader program f-shader)
            (gl:link-program program)
            (format *error-output*
                    "shader compilation log:~%~a~%"
                    (gl:get-program-info-log program))
            (gl:use-program program)))
      program)))

(defun fill-terrain-vertices (mesh)
  (with-accessors ((vertices-count                     vertices-count)
                   (vertices                           vertices)
                   (shader-program                     shader-program)
                   (modelview-matrix-uniform-location  modelview-matrix-uniform-location)
                   (projection-matrix-uniform-location projection-matrix-uniform-location)
                   (pixmap-heightmap                      pixmap-heightmap)) mesh
    (with-accessors ((width  pixmap:width)
                     (height pixmap:height)) pixmap-heightmap
      (loop for x from 0 below (1- height) do
        (loop for z from 0 below (1- width) do
          ;;                                       Z
          ;;   <-----------------------------------+
          ;;    (v4)  v1               v6          |
          ;;             +------------+ ···        |
          ;;             | \-         |            |
          ;;             |   \-       |            |
          ;;             |     \-     |            |
          ;;             |       \-   |            |
          ;;             |         \- |            |
          ;;             +-----------\+ ···
          ;;          v2               v3 (v5)    V
          (flet ((get-height (pixmap x y)
                   (to:d (a:first-elt (pixmap:pixel@ pixmap x y)))))
            (let* ((x-float (to:d x))
                   (z-float (to:d z))
                   (v1 (vec3:vec3 x-float
                                  (get-height pixmap-heightmap x z)
                                  z-float))
                   (v2 (vec3:vec3 (1+ x-float)
                                  (get-height pixmap-heightmap (1+ x) z)
                                  z-float))
                   (v3 (vec3:vec3 (1+ x-float)
                                  (get-height pixmap-heightmap (1+ x) (1+ z))
                                  (1+ z-float)))
                   (v4 v1)
                   (v5 v3)
                   (v6 (vec3:vec3 x-float
                                  (get-height pixmap-heightmap x (1+ z))
                                  (1+ z-float))))
              (vector-push-extend v1 vertices)
              (vector-push-extend v2 vertices)
              (vector-push-extend v3 vertices)
              (vector-push-extend v4 vertices)
              (vector-push-extend v5 vertices)
              (vector-push-extend v6 vertices))))))))

(defmethod initialize-instance :after ((object terrain-mesh) &key &allow-other-keys)
  (with-accessors ((vertices-count                     vertices-count)
                   (vertices                           vertices)
                   (shader-program                     shader-program)
                   (modelview-matrix-uniform-location  modelview-matrix-uniform-location)
                   (projection-matrix-uniform-location projection-matrix-uniform-location)
                   (pixmap-heightmap                      pixmap-heightmap)) object
    (setf vertices-count
          (to:f* 3 ; each triangle contains three vertices
                 (square-pixmap-count-triangles (pixmap-heightmap object))))
    (setf shader-program (init-shaders))
    (setf modelview-matrix-uniform-location
          (gl:get-uniform-location shader-program "modelview_matrix"))
    (setf projection-matrix-uniform-location
          (gl:get-uniform-location shader-program "projection_matrix"))
    (fill-terrain-vertices object)
    (assert (= (length vertices) vertices-count))
    (format *error-output* "debug~%")
    (format *error-output* "terrain~%")
    (debug-terrain-pixmap pixmap-heightmap)
    (format *error-output* "vertices~%")
    (loop for i across vertices do
      (format *error-output* "~a~%" i))
    (format *error-output* "vertices end~%")
    object))

(defparameter *terrain* nil)

(defgeneric prepare-for-rendering (object))

(defgeneric render (object))

(defgeneric make-data-for-opengl (object))

(defgeneric remove-mesh-data (object))

(defmethod remove-mesh-data ((object terrain-mesh))
  (setf (vertices-count object) 0)
  object)

(defmethod %reset-data-renderer-count ((object terrain-mesh))
  (with-accessors ((renderer-data-count-vertices renderer-data-count-vertices))
      object
    (setf renderer-data-count-vertices 0)
    object))

(defun square-pixmap-count-triangles (pixmap)
  (with-accessors ((width pixmap:width)) pixmap
    (let ((one-less-width (1- width)))
      (to:f* 2 (to:f* one-less-width one-less-width)))))

(defmethod make-data-for-opengl ((object terrain-mesh))
  (with-accessors ((pixmap-heightmap             pixmap-heightmap)
                   (vertices-count               vertices-count)
                   (renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-vertices       renderer-data-vertices)
                   (vertices                     vertices)
                   (vbo                          vbo)
                   (vao                          vao)) object
    (%reset-data-renderer-count object)
    (labels ((get-vertices (results offset vertex)
               (declare (vec3:vec3 vertex))
               (declare (fixnum offset))
               (let ((scaled (vec3:vec3 (to:d* 10.0 (elt vertex 0))
                                        (elt vertex 1)
                                        (to:d* 10.0 (elt vertex 2)))))
                 (setf (fast-glaref results offset)       (elt scaled 0))
                 (setf (fast-glaref results (+ 1 offset)) (elt scaled 1))
                 (setf (fast-glaref results (+ 2 offset)) (elt scaled 2)))))
      (let ((gl-results-v (gl:alloc-gl-array :float (to:f* vertices-count 3))))
        (loop for vertex across vertices
              for gl-array-offset from 0 by 3
              do
                 (get-vertices gl-results-v gl-array-offset vertex))
        (setf renderer-data-vertices gl-results-v)
        (debug-gl-array renderer-data-vertices (to:f* vertices-count 3))
        ;; setup finalizer
        (let ((gl-arr-vert (slot-value object 'renderer-data-vertices))
              (vbos        (slot-value object 'vbo))
              (vaos        (slot-value object 'vao)))
          (tg:finalize object
                       #'(lambda ()
                           (free-memory* (list gl-arr-vert)
                                         vbos
                                         vaos))))))))

(defun free-memory (vertices vbo vao)
  (gl:free-gl-array vertices)
  (gl:delete-vertex-arrays vao)
  (gl:delete-buffers vbo))

(defun free-memory* (arrays vbos vaos)
  (loop for i in arrays do
       (when (and i
                  (not (gl::null-pointer-p (gl::gl-array-pointer i))))
         (gl:free-gl-array i)
         (setf (gl::gl-array-pointer i) (gl::null-pointer))))
  (when vbos
    (gl:delete-buffers vbos))
  (when vaos
    (gl:delete-vertex-arrays vaos)))

(defmethod mesh-destroy ((object terrain-mesh))
  (with-accessors ((renderer-data-vertices renderer-data-vertices)
                   (vbo vbo)
                   (vao vao)) object
    (setf vbo                              nil
          vao                              nil
          renderer-data-vertices           nil)))

(defun vbo-vertex-buffer-handle (vbo)
  (elt vbo 0))

(defun vao-vertex-buffer-handle (vao)
  (elt vao 0))

(a:define-constant +attribute-position-location+ 0 :test #'=)

(defmacro with-unbind-vao (&body body)
  `(unwind-protect
        (progn
          ,@body)
     (gl:bind-vertex-array 0)))

(defmethod prepare-for-rendering ((object terrain-mesh))
  (with-accessors ((vbo vbo)
                   (vao vao)
                   (renderer-data-vertices renderer-data-vertices)) object
    (mesh-destroy object)
    (setf vbo (gl:gen-buffers 1)
          vao (gl:gen-vertex-arrays 1))
    (make-data-for-opengl object)
    ;; vertices
    (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
    (gl:buffer-data :array-buffer :static-draw renderer-data-vertices)
    (with-unbind-vao
      (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
      ;; vertices
      (gl:bind-buffer :array-buffer (vbo-vertex-buffer-handle vbo))
      (gl:vertex-attrib-pointer +attribute-position-location+
                                3
                                :float
                                0
                                0
                                (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+))
    object))

(defmethod calculate ((object terrain-mesh) dt)
  (build-projection-matrix *camera*
                           *near*
                           *far*
                           *fov*
                           (to:d (/ +sdl-frame-width+
                                    +sdl-frame-height+)))
  (look-at *camera*))

(defun uniform-matrix (location dim matrices &optional (transpose t))
  (gl:uniform-matrix location dim matrices transpose))

(defmethod render ((object terrain-mesh))
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (with-accessors ((shader-program    shader-program)
                   (vertices-count    vertices-count)
                   (vbo               vbo)
                   (vao               vao)
                   (projection-matrix projection-matrix)
                   (model-matrix      model-matrix)
                   (view-matrix       view-matrix)
                   (compiled-shaders  compiled-shaders)) object
    (declare ((simple-array simple-array (1)) projection-matrix model-matrix view-matrix))
    (declare (list vao vbo))
    (declare (fixnum vertices-count))
    (with-camera-view-matrix (camera-vw-matrix)
      (with-camera-projection-matrix (camera-proj-matrix :wrapped t)
        (uniform-matrix (modelview-matrix-uniform-location object)
                        4
                        (vector (matrix:matrix* camera-vw-matrix
                                                (elt view-matrix 0)
                                                (elt model-matrix 0)))
                        nil)
        (uniform-matrix (projection-matrix-uniform-location object)
                        4
                        camera-proj-matrix
                        nil)
        (gl:bind-vertex-array (vao-vertex-buffer-handle vao))
        (gl:draw-arrays :triangles 0 (to:f* vertices-count 3))))))

(defun initialize-rendering-function (wish-stream)
  (lambda (context)
    (let ((*wish* wish-stream))
      (with-busy ((root-toplevel))
        (3d:default-initialize-function context)
        (setf *terrain* (make-instance 'terrain-mesh))
        (prepare-for-rendering *terrain*)))))

(defparameter *3d-context* nil)

(defun stop-3d-animation ()
  (when (and *animation*
             (bt:threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)))

(defun draw-terrain-thread ()
  (loop while (not (stop-drawing-thread-p *animation*)) do
    (ctx:sync *3d-context*)
    (ctx:push-for-updating *3d-context*
                           (lambda (dt)
                             (calculate *terrain* dt))
                           :force-push nil)
    (ctx:push-for-rendering *3d-context*
                            (lambda (dt)
                              (declare (ignore dt))
                              (render *terrain*))
                            :force-push nil))
  (format t "STOP terrain rendering!~%"))

(a:define-constant +velocity+ 10f0 :test #'=)

(defun demo-terrain ()
  (with-nodgui ()
    (let* ((instruction      (make-instance 'label
                                            :wraplength 800
                                            :text "use cursor keys to move around, 'e' and 'd' to change camera's height and 's' and 'd' keys to rotate the position where camera is currently pointing; rerun the demo to generate a new map."
                                            :font (font-create "serif"
                                                               :size 12
                                                               :weight :bold)))
           (sdl-frame        (ctx:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
           (button-quit      (make-instance 'button
                                            :master  nil
                                            :text    "quit"
                                            :command (lambda ()
                                                       (stop-3d-animation)
                                                       (ctx:quit-sdl *3d-context*)
                                                       (exit-nodgui)))))
      (grid instruction     0 0)
      (grid sdl-frame       1 0)
      (grid button-quit     2 0 :sticky :nw)
      (grid-columnconfigure (root-toplevel) :all :weight 1)
      (grid-rowconfigure    (root-toplevel) :all :weight 1)
      (setf *3d-context*
            (make-instance '3d:opengl-context
                           :event-loop-type         :polling
                           :initialization-function (initialize-rendering-function *wish*)
                           :classic-frame           sdl-frame
                           :buffer-width            +sdl-frame-width+
                           :buffer-height           +sdl-frame-height+))
      (force-focus sdl-frame)
      (flet ((calculate-moving (from to velocity)
               (let ((dir (vec3:vec3* (vec3:vec3-normalize (vec3:vec3- to from))
                                      velocity)))
                 (setf (elt dir 1) 0f0)
                 dir)))
        (bind sdl-frame
              #$<1>$
              (lambda (event)
                (declare (ignore event))
                (format *error-output* "clicked on 3D rendering window~%")))
        (bind sdl-frame
              #$<KeyPress-Up>$
              (lambda (event)
                (declare (ignore event))
                ;; when we are modifying a resources used both by TK and
                ;; SDL (*camera* in this case) we need to force the code
                ;; that update the  resources to be executed  by the SDL
                ;; (this constrains is especially true if we call openGL
                ;; library  functions) the  following macro  ensure only
                ;; the SDL  thread will execute  the code in  the body's
                ;; macro
                (ctx:in-renderer-thread (*3d-context* dt)
                  (declare (ignore dt))
                  (let ((dir (calculate-moving (pos *camera*)
                                               (target *camera*)
                                               +velocity+)))
                    (setf (pos *camera*)
                          (vec3:vec3+ (pos *camera*)
                                      dir))
                    (setf (target *camera*)
                          (vec3:vec3+ (target *camera*)
                                      dir))))))
        (bind sdl-frame
              #$<KeyPress-Down>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                  (declare (ignore dt))
                  (let ((dir (calculate-moving (target *camera*)
                                               (pos *camera*)
                                               +velocity+)))
                    (setf (pos *camera*)
                          (vec3:vec3+ (pos *camera*)
                                      dir))
                    (setf (target *camera*)
                          (vec3:vec3+ (target *camera*)
                                      dir))))))
        (bind sdl-frame
              #$<KeyPress-Left>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (setf (pos *camera*)
                        (vec3:vec3+ (pos *camera*)
                                    (vec3:vec3 +velocity+ 0f0 0f0)))
                  (setf (target *camera*)
                        (vec3:vec3+ (target *camera*)
                                    (vec3:vec3 +velocity+ 0f0 0f0))))))
        (bind sdl-frame
              #$<KeyPress-Right>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (setf (pos *camera*)
                        (vec3:vec3+ (pos *camera*)
                                    (vec3:vec3 (to:d- +velocity+) 0f0 0f0)))
                  (setf (target *camera*)
                        (vec3:vec3+ (target *camera*)
                                    (vec3:vec3 (to:d- +velocity+) 0f0 0f0))))))
        (bind sdl-frame
              #$<KeyPress-w>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (setf (pos *camera*)
                        (vec3:vec3+ (pos *camera*)
                                    (vec3:vec3 0.0 10.0 0.0)))
                  (setf (target *camera*)
                        (vec3:vec3+ (target *camera*)
                                    (vec3:vec3 0.0 10.0 0.0))))))
        (bind sdl-frame
              #$<KeyPress-s>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (setf (pos *camera*)
                        (vec3:vec3+ (pos *camera*)
                                    (vec3:vec3 0.0 -10.0 0.0)))
                  (setf (target *camera*)
                        (vec3:vec3+ (target *camera*)
                                    (vec3:vec3 0.0 -10.0 0.0))))))
        (bind sdl-frame
              #$<KeyPress-a>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (rotate-around-y-axe *camera* -1f0))))
        (bind sdl-frame
              #$<KeyPress-d>$
              (lambda (event)
                (declare (ignore event))
                (ctx:in-renderer-thread (*3d-context* dt)
                                        (declare (ignore dt))
                  (rotate-around-y-axe *camera* 1f0)))))
      (wait-complete-redraw)
      (setf *animation*
            (make-animation :thread
                            (make-thread #'draw-terrain-thread
                                         :name "terrain"))))))
