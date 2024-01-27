(in-package :nodgui.demo)

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

(a:define-constant +terrain-size+        32 :test #'=)

(a:define-constant +max-terrain-height+ 255 :test #'=)

(defun make-terrain-pixmap (max-height
                            &optional (pixmap (pixmap::make-pixmap +terrain-size+
                                                                   +terrain-size+
                                                                   (nodgui.ubvec4:ubvec4 0
                                                                                         0
                                                                                         0
                                                                                         255))))
  (let* ((mean-x   (random +terrain-size+))
         (mean-y   (random +terrain-size+))
         (sigma-x  (1+ (random (/ (to:d +terrain-size+) 3))))
         (sigma-y  (1+ (random (/ (to:d +terrain-size+) 3)))))
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
    (let ((max (let ((max-so-far -1))
                 (loop for x from 0 below +terrain-size+
                       do
                          (loop for y from 0 below +terrain-size+
                                do
                                   (when (> (elt (pixmap:pixel@ pixmap x y) 0)
                                            max-so-far)
                                     (setf max-so-far
                                           (elt (pixmap:pixel@ pixmap x y) 0)))))
                 max-so-far)))
      (if (>= max max-height)
          pixmap
          (make-terrain-pixmap max-height pixmap)))))

(definline fast-glaref (v offset)
  (cffi:mem-aref (gl::gl-array-pointer v) :float offset))

(definline set-fast-glaref (v offset new-val)
  (setf (cffi:mem-aref (gl::gl-array-pointer v) :float offset)
        new-val)
  new-val)

(definline mock-null-pointer ()
  0)

(defsetf fast-glaref set-fast-glaref)

(defgeneric seq->gl-array (seq))

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
    :initform (vec3:vec3 10.0 0.0 10.0)
    :accessor target
    :initarg :target
    :documentation "position the camera is pointing to.")
   (pos
    :initform vec3:+vec3-zero+
    :accessor pos
    :initarg :pos
    :documentation "position the camera.")
   (up
    :initform (vec3:vec3 0.0 1.0 0.0)
    :accessor up
    :initarg :up)))

(defmethod look-at ((object camera))
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (with-accessors ((up  up)
                   (pos pos)
                   (dir dir)
                   (target target)
                   (fading-away-fn fading-away-fn)) object
    (declare (function fading-away-fn))
    (let ((standard-vw-matrix (matrix:look@ pos target up)))
      (declare (matrix:matrix standard-vw-matrix))
      (setf (view-matrix object) standard-vw-matrix))))

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

(defclass terrain-mesh ()
  ((height-pixmap
    :initform (make-terrain-pixmap +max-terrain-height+)
    :initarg  :height-pixmap
    :accessor height-pixmap)
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

(defparameter *vertex-shader* "
#version 330 core

#version 330 core

layout x(location = 0)  in vec4 position;

uniform mat4 modelview_matrix;

uniform mat4 projection_matrix;

out float height;

mat3 normal_matrix = mat3(modelview_matrix);

void main () {
  height                 = position.y / 255.0;
  eye_position           = modelview_matrix * position;
  gl_Position            = proj_matrix * modelview_matrix * position;
}
")

(defparameter *fragment-shader* "
#version 330 core

in float height;

out vec4 color;

float lerp (in float from, in float to, in float pos) {
  float w = (pos - from) / (to - from);
  return w;
}

vec4 col_empty  = vec4(0.0);

vec4 terrain_color_by_height() {

  if (height <= 10){
    return vec4(255,0,0,255);
  }else if (height >  10 &&
	    height <= 30){
    return mix(vec4(255,0,0,255), vec4(0,255,0,255), height));
   } else {
    return mix(vec4(0,255,0,255), vec4(0,0,255,255), height));
  }

}

void main () {
  color = terrain_color_by_height();
}
")

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
            (format t "~A~%" (gl:get-program-info-log program))
            (gl:use-program program)))
      program)))

(defun fill-terrain-vertices (mesh)
  (with-accessors ((vertices-count                     vertices-count)
                   (vertices                           vertices)
                   (shader-program                     shader-program)
                   (modelview-matrix-uniform-location  modelview-matrix-uniform-location)
                   (projection-matrix-uniform-location projection-matrix-uniform-location)
                   (height-pixmap                      height-pixmap)) mesh
    (with-accessors ((width  pixmap:width)
                     (height pixmap:height)) height-pixmap
      (loop for y from 0 below (1- height) do
        (loop for x from 0 below (1- width) do
          ;; (v4)  v1 +------------+ v6
          ;;          | \-         |
          ;;          |   \-       |
          ;;          |     \-     |
          ;;          |       \-   |
          ;;          |         \- |
          ;;       v2 +-----------\+ v3 (v5)
          (flet ((get-height (pixmap x y)
                   (to:d (a:first-elt (pixmap:pixel@ pixmap x y)))))
            (let* ((x-float (to:d x))
                   (y-float (to:d y))
                   (v1 (vec3:vec3 x-float
                                  y-float
                                  (get-height height-pixmap x y)))
                   (v2 (vec3:vec3 x-float
                                  (1+ y-float)
                                  (get-height height-pixmap x (1+ y))))
                   (v3 (vec3:vec3 (1+ x-float)
                                  (1+ y-float)
                                  (get-height height-pixmap (1+ x) (1+ y))))
                   (v4 v1)
                   (v5 v3)
                   (v6 (vec3:vec3 (1+ x-float)
                                  y-float
                                  (get-height height-pixmap (1+ x) y))))
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
                   (height-pixmap                      height-pixmap)) object
    (setf vertices-count
          (to:f* 3             ; each triangle contains three vertices
            (square-pixmap-count-triangles (height-pixmap object))))
    (setf shader-program (init-shaders))
    (setf modelview-matrix-uniform-location
          (gl:get-uniform-location shader-program "modelview_matrix"))
    (setf projection-matrix-uniform-location
          (gl:get-uniform-location shader-program "projection_matrix"))
    (fill-terrain-vertices object)
    (assert (= (length vertices) vertices-count))
    object))

(defgeneric prepare-for-rendering-phong (object))

(defgeneric render-phong (object renderer))

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
  (with-accessors ((height-pixmap                height-pixmap)
                   (vertices-count               vertices-count)
                   (renderer-data-count-vertices renderer-data-count-vertices)
                   (renderer-data-vertices       renderer-data-vertices)
                   (vertices                     vertices)
                   (vbo                          vbo)
                   (vao                          vao)) object
    (%reset-data-renderer-count object)
    (labels ((get-vertices (results offset vertices-index)
               (let* ((v (elt vertices vertices-index)))
                 (declare (vec3:vec3 v))
                 (declare (fixnum offset vertices-index))
                 (setf (fast-glaref results offset)       (elt v 0))
                 (setf (fast-glaref results (+ 1 offset)) (elt v 1))
                 (setf (fast-glaref results (+ 2 offset)) (elt v 2)))))
      (let ((gl-results-v (gl:alloc-gl-array :float (to:f* vertices-count 3))))
        (loop for i in vertices
              for gl-array-offset from 0 by 3
              do
                 (get-vertices gl-results-v gl-array-offset i))
        (setf renderer-data-vertices gl-results-v)
        ;; setup finalizer
        (let ((gl-arr-vert     (slot-value object 'renderer-data-vertices))
              (vbos            (slot-value object 'vbo))
              (vaos            (slot-value object 'vao)))
          (tg:finalize object
                       #'(lambda ()
                           (free-memory* (list gl-arr-vert)
                                         vbos vaos))))))))

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
      (gl:vertex-attrib-pointer +attribute-position-location+ 3 :float 0 0 (mock-null-pointer))
      (gl:enable-vertex-attrib-array +attribute-position-location+))
    object))

(defmethod calculate ((object terrain-mesh) dt)
  (with-accessors ((vbo vbo) (render-aabb render-aabb)
                   (renderer-data-aabb-obj-space renderer-data-aabb-obj-space)) object
    (call-next-method)))

(defun uniform-matrix (location dim matrices &optional (transpose t))
  (gl:uniform-matrix location dim matrices transpose))

(defmethod render ((object terrain-mesh) renderer)
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

(defun demo-terrain ()
  (let ((sdl-context nil))
    (with-nodgui ()
        (let* ((sdl-frame         (px:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
               (button-quit      (make-instance 'button
                                                :master  nil
                                                :text    "quit"
                                                :command (lambda ()
                                                           (px:quit-sdl sdl-context)
                                                           (exit-nodgui)))))
          (grid sdl-frame          0 0)
          (grid button-quit        1 0 :sticky :nw)
          (grid-columnconfigure (root-toplevel) :all :weight 1)
          (grid-rowconfigure    (root-toplevel) :all :weight 1)
          (wait-complete-redraw)
          (setf sdl-context (make-instance '3d:opengl-context
                                           :event-loop-type :polling
                                           :classic-frame   sdl-frame
                                           :buffer-width    +sdl-frame-width+
                                           :buffer-height   +sdl-frame-height+))))))
