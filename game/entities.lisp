(in-package :entities)

(defclass occupy-space ()
  ((pos
    :initform (vec2:vec2)
    :initarg  :pos
    :reader   pos)
   (center-pos
    :initform (vec2:vec2)
    :initarg  :center-pos
    :accessor center-pos)
   (width
    :initarg :width
    :initform 0
    :accessor width
    :type fixnum)
   (height
    :initarg :height
    :initform 0
    :accessor height
    :type fixnum)))

(defclass physical-object (occupy-space)
  ((aabb
    :initform (aabb2:make-aabb2)
    :initarg :aabb
    :accessor aabb)
   (scaling-aabb
    :initform 1.0f0
    :initarg  :scaling-aabb
    :accessor scaling-aabb)
   (delta-pos
    :initform (vec2:vec2)
    :initarg  :delta-pos
    :accessor delta-pos)
   (force
    :initform (vec2:vec2)
    :initarg  :force
    :accessor force)
   (velocity
    :initform (vec2:vec2)
    :initarg  :velocity
    :accessor velocity)
   (acceleration
    :initform 0f0
    :initarg  :acceleration
    :accessor acceleration)
   (mass
    :initform .2f0
    :initarg  :mass
    :accessor mass)
   (width
    :initarg :width
    :initform 0
    :accessor width
    :type fixnum)
   (height
    :initarg :height
    :initform 0
    :accessor height
    :type fixnum)))

(defun shrink-recenter-aabb (w h center-pos scaling-factor)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum w h))
  (declare (to::desired-type scaling-factor))
  (declare (vec2:vec2 center-pos))
  (let* ((aabb-max-x  (to:d* scaling-factor (to:d w)))
         (aabb-max-y  (to:d* scaling-factor (to:d h)))
         (aabb        (aabb2:make-aabb2 0f0
                                        0f0
                                        aabb-max-x
                                        aabb-max-y))
         (aabb-center (aabb2:center-aabb2 aabb))
         (transl      (vec2:vec2- center-pos aabb-center)))
    (declare (dynamic-extent aabb-max-x
                             aabb-max-y
                             aabb
                             aabb-center
                             transl))
    (declare (to::desired-type aabb-max-x aabb-max-y))
    (declare (vec2:vec2 transl aabb-center))
    (declare ((simple-array to::desired-type) aabb))
    (aabb2:trasl-aabb2 aabb
                       (vec2:vec2-x transl)
                       (vec2:vec2-y transl))))

(defmethod (setf pos) (new-position (object physical-object))
  (with-accessors ((aabb         aabb)
                   (scaling-aabb scaling-aabb)
                   (width        width)
                   (height       height)
                   (center-pos center-pos)) object
    (setf (slot-value object 'pos) new-position)
    (setf center-pos (vec2:vec2+ new-position
                                 (vec2:vec2-insecure (to:d (/ width 2))
                                                     (to:d (/ height 2)))))
    (setf aabb (shrink-recenter-aabb (width object)
                                     (height object)
                                     center-pos
                                     scaling-aabb))
    object))

(defun aabb-outside-world-p (aabb world-w world-h)
  #.nodgui.config:default-optimization
  (declare ((simple-array to::desired-type) aabb))
  (declare (fixnum world-w world-h))
  (or (< (aabb2:aabb2-max-x aabb) 0f0)
      (> (aabb2:aabb2-min-x aabb) (to:d world-w))
      (< (aabb2:aabb2-max-y aabb) 0f0)
      (> (aabb2:aabb2-min-y aabb) (to:d world-h))))

(defun integrate (physical-object dt)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 0) (debug 3) (safety 3)))
  (declare (physical-object physical-object))
  (with-accessors ((pos          pos)
                   (delta-pos    delta-pos)
                   (acceleration acceleration)
                   (velocity     velocity)
                   (force        force)
                   (mass         mass)) physical-object
    (setf acceleration (vec2:vec2/ force mass))
    (setf velocity     (vec2:vec2+ velocity
                                   (vec2:vec2* acceleration dt)))
    (setf delta-pos    (vec2:vec2* velocity dt))
    (setf pos          (vec2:vec2+ pos delta-pos))
    physical-object))

(defmethod calculate ((object physical-object) world dt)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((aabb       aabb)
                   (pos        pos)
                   (center-pos center-pos)
                   (width      width)
                   (height     height)
                   (delta-pos  delta-pos)) object
    (declare ((simple-array to::desired-type) aabb))
    (declare (vec2:vec2 delta-pos pos center-pos))
    (declare (fixnum width height))
    (integrate object dt)
    (with-accessors ((world-width world:width)
                     (world-height world:height)) world
      (declare (fixnum world-width world-height))
      (setf aabb (aabb2:trasl-aabb2 aabb
                                    (vec2:vec2-x delta-pos)
                                    (vec2:vec2-y delta-pos)))
      ;; toroidal  space,  wrapping  the  coordinates if  the  aabb  is
      ;; completely outside the screen
      (cond
        ((< (aabb2:aabb2-max-x aabb) 0f0)
         (setf (pos object)
               (vec2:vec2-insecure (- (to:d world-width)
                                      (to:d (/ width 2f0)))
                                   (vec2:vec2-y pos))))
        ((> (aabb2:aabb2-min-x aabb) (to:d world-width))
         (setf (pos object)
               (vec2:vec2-insecure (to:d (- (rem (truncate (vec2:vec2-x center-pos))
                                                 world-width)
                                            (/ width 2f0)))
                                   (vec2:vec2-y pos))))
        ((< (aabb2:aabb2-max-y aabb) 0f0)
         (setf (pos object)
               (vec2:vec2-insecure (vec2:vec2-x pos)
                                   (to:d (- world-height
                                            (/ height 2f0))))))
        ((> (aabb2:aabb2-min-y aabb) (to:d world-height))
         (setf (pos object)
               (vec2:vec2-insecure (vec2:vec2-x pos)
                                   (to:d (- (rem (truncate (vec2:vec2-y center-pos))
                                                 world-height)
                                            (/ height 2f0))))))))))

(defclass with-hitpoints ()
  ((hitpoints
    :initform 1
    :initarg  :hitpoints
    :accessor hitpoints)))

(defclass entity (physical-object with-hitpoints)
  ((target-aabb
    :initform (aabb2:make-aabb2)
    :initarg :target-aabb
    :accessor target-aabb)
   (scaling-target-aabb
    :initform 1.0f0
    :initarg  :scaling-target-aabb
    :accessor scaling-target-aabb)))

(defgeneric alivep (object))

(defmethod alivep ((object entity))
  (> (hitpoints object) 0))

(defmethod (setf pos) :after (new-position (object entity))
  (with-accessors ((target-aabb         target-aabb)
                   (scaling-target-aabb scaling-target-aabb)
                   (width        width)
                   (height       height)
                   (center-pos center-pos)) object
    (setf target-aabb
          (shrink-recenter-aabb (width object)
                                (height object)
                                center-pos
                                scaling-target-aabb))
    object))

(defgeneric calculate (object world dt))

(defgeneric draw (object world destination destination-width destination-height))

(defgeneric hittedp (object entity))

(defgeneric destroyedp (object))

(defclass with-bitmap ()
  ((bitmap
    :initform #()
    :initarg  :bitmap
    :accessor bitmap)))

(defclass sprite (entity with-bitmap) ())

(defmethod calculate ((object sprite) world dt)
  (call-next-method))

(defun common-initialize-sprite (sprite pixmap)
  (with-accessors ((bitmap     bitmap)
                   (width      width)
                   (height     height)
                   (center-pos center-pos)) sprite
    (setf bitmap (pixmap:bits pixmap))
    (setf width  (pixmap:width pixmap))
    (setf height (pixmap:height pixmap))
    ;; keep last
    (setf (pos sprite) (slot-value sprite 'pos))
    sprite))

(defmethod initialize-instance :after ((object sprite) &key &allow-other-keys)
  (common-initialize-sprite object (bitmap object)))

(defun common-draw-sprite (sprite destination destination-width destination-height)
  (with-accessors ((bitmap bitmap)
                   (width  width)
                   (height height)
                   (object object)
                   (pos    pos)) sprite
    (px:blit bitmap
             width
             destination
             destination-width
             destination-height
             0
             0
             (truncate (vec2:vec2-y pos))
             (truncate (vec2:vec2-x pos))
             height
             width)))

(defmethod draw ((object sprite) world destination destination-width destination-height)
  (declare (ignore world))
  (common-draw-sprite object destination destination-width destination-height))

(defclass spawn-children ()
  ((next-generation-size
    :initform nil
    :initarg  :next-generation-size
    :accessor next-generation-size)))

(defclass sprite-asteroid (sprite spawn-children) ())

(defun draw-aabb (object destination destination-width destination-height)
  (with-accessors ((aabb aabb)) object
    (px:fill-rectangle destination
                       destination-width
                       destination-height
                       (round (aabb2:aabb2-min-x aabb))
                       (round (aabb2:aabb2-min-y aabb))
                       (round (aabb2:aabb2-max-x aabb))
                       (round (aabb2:aabb2-max-y aabb))
                       #xff
                       #x00
                       #xff)))

(defun draw-aabbs (object destination destination-width destination-height)
  (draw-aabb object destination destination-width destination-height)
  (with-accessors ((target-aabb target-aabb)) object
    (px:fill-rectangle destination
                       destination-width
                       destination-height
                       (round (aabb2:aabb2-min-x target-aabb))
                       (round (aabb2:aabb2-min-y target-aabb))
                       (round (aabb2:aabb2-max-x target-aabb))
                       (round (aabb2:aabb2-max-y target-aabb))
                       #x00
                       #x00
                       #xff)))

#+debug-game
(defmethod draw :after ((object sprite-asteroid) world destination destination-width destination-height)
  (draw-aabbs object destination destination-width destination-height))

(defclass polygon ()
  ((vertices
    :initform (px:make-polygon-vertex-array (list (vec2:vec2)
                                                  (vec2:vec2)
                                                  (vec2:vec2)
                                                  (vec2:vec2)))
    :initarg  :vertices
    :accessor vertices)))

(defclass with-texture ()
  ((tex-coordinates
    :initform (px:make-polygon-texture-coordinates-array (list (vec2:vec2)
                                                               (vec2:vec2)
                                                               (vec2:vec2)
                                                               (vec2:vec2)))
    :initarg  :tex-coordinates
    :accessor tex-coordinates)))

(defclass background (polygon with-texture with-bitmap) ())

(defmethod draw ((object background) world destination destination-width destination-height)
  (declare (ignore world))
  (px:draw-texture-mapped-polygon destination
                                  destination-width
                                  destination-height
                                  (vertices        object)
                                  (tex-coordinates object)
                                  (bitmap          object)))

(defclass asteroid (polygon entity spawn-children)
  ((fill-color
    :initform (pixmap:assemble-color 255 50 20 10)
    :initarg  :fill-color
    :accessor fill-color)
   (current-vertices
    :initform (px:make-polygon-vertex-array (list (vec2:vec2)
                                                  (vec2:vec2)
                                                  (vec2:vec2)
                                                  (vec2:vec2)))
    :initarg  :current-vertices
    :accessor current-vertices)))

(defun expand-aabb (aabb vertices)
  (map nil
       (lambda (a) (aabb2:nexpand-aabb2 aabb a))
       vertices)
  aabb)

(defmethod initialize-instance :after ((object asteroid) &key &allow-other-keys)
  #.nodgui.config:default-optimization
  (with-accessors ((width    width)
                   (height   height)
                   (vertices vertices)
                   (aabb     aabb)) object
    (setf aabb         (expand-aabb (aabb2:make-aabb2) vertices))
    (setf width        (truncate (the to::desired-type (aabb2:aabb2-width aabb))))
    (setf height       (truncate (the to::desired-type (aabb2:aabb2-height aabb))))
    ;; keep last
    (setf (pos object) (slot-value object 'pos))
    object))

(defmethod calculate :after ((object asteroid) world dt)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((pos              pos)
                   (vertices         vertices)
                   (current-vertices current-vertices)) object
    (declare ((simple-array vec2:vec2) vertices))
    (flet ((bubbling (vector-component phase)
             (declare (to::desired-type vector-component phase))
             (to:d* vector-component
                    (to:d+ 1f0
                           (to:d* 0.05f0
                                  (to:dsin (to:d* 2500f0
                                                    (to:d+ phase
                                                       (the to::desired-type
                                                            (world:ticks world))))))))))
      (setf current-vertices
            (map 'vector
                 (lambda (v)
                   (vec2:vec2+ pos
                               (vec2:vec2-insecure (bubbling (vec2:vec2-x v) 0f0)
                                                   (bubbling (vec2:vec2-y v) 1000f0))))
                 vertices)))))

(defmethod draw ((object asteroid) world destination destination-width destination-height)
  (with-accessors ((current-vertices current-vertices)
                   (fill-color       fill-color)) object
    (let ((px:*blending-function* #'px:blending-function-add))
      (px:draw-polygon destination
                       destination-width
                       destination-height
                       current-vertices
                       fill-color))
    #+debug-game
    (draw-aabbs object destination destination-width destination-height)))

(defun approx-gaussian-random ()
  (/ (- 6
        (loop repeat 12 sum (random 1.0)))
     6))

(defun make-asteroid-vertices (average-radius vertices-number)
  (let* (;;#+sbcl           (*random-state* (sb-kernel::seed-random-state 147437))
         (vertices        (loop for i from 0 below (* 2 pi) by (/ pi vertices-number)
                              for radius = average-radius
                                then (+ average-radius
                                        (* 3/4
                                           average-radius
                                           (approx-gaussian-random)))
                              collect
                              (vec2:vec2 (* radius (cos i))
                                         (* radius (sin i)))))
         (aabb            (expand-aabb (aabb2:make-aabb2)
                                       vertices))
         (offset          (vec2:vec2 (to:d- (aabb2:aabb2-min-x aabb))
                                     (to:d- (aabb2:aabb2-min-y aabb))))
         (actual-vertices (loop for vertex in vertices
                                collect
                                (vec2:vec2+ vertex offset))))
    (px:make-polygon-vertex-array actual-vertices)))

(a:define-constant +start-player-direction+ (vec2:vec2 0 -1) :test #'vec2:vec2=)

(defclass with-direction ()
  ((direction
    :initform +start-player-direction+
    :initarg  :direction
    :accessor direction)))

(defclass with-rotation ()
  ((rotation
    :initform 0f0
    :initarg  :rotation
    :accessor rotation)))

(defclass thrush-exhaust (occupy-space with-bitmap with-rotation) ())

(defmethod (setf pos) (new-position (object thrush-exhaust))
  (setf (slot-value object 'pos) new-position))

(defmethod initialize-instance :after ((object thrush-exhaust) &key &allow-other-keys)
  (common-initialize-sprite object (bitmap object)))

(defun common-draw-rotated-sprite (sprite destination destination-width destination-height)
  (with-accessors ((bitmap   bitmap)
                   (width    width)
                   (height   height)
                   (pos      pos)
                   (rotation rotation)) sprite
    (px:blit-transform bitmap
                       width
                       height
                       destination
                       destination-width
                       destination-height
                       0
                       0
                       (truncate (vec2:vec2-y pos))
                       (truncate (vec2:vec2-x pos))
                       height
                       width
                       (to:radians->degree rotation)
                       1.0
                       1.0
                       (truncate (ash height -1))
                       (truncate (ash width -1)))))

(defun blending-function-thrust-clsr (world)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((flame-intensity (+ 0.5f0 (* 0.1f0
                                   (to:dsin (* 4000f0
                                               (the to::desired-type (world:ticks world))))))))
    (declare (dynamic-extent flame-intensity))
    (declare (to::desired-type flame-intensity))
    (lambda (pixel-source pixel-destination)
      (declare (fixnum pixel-source pixel-destination))
      ;;#.nodgui.config:default-optimization
      (declare (optimize (speed 3) (debug 0) (safety 0)))
      (px:with-displace-pixel (r-source g-source b-source x)
                              pixel-source
        (declare (ignore x))
        (px:with-displace-pixel (r-destination g-destination b-destination alpha-destination)
                                pixel-destination
          (flet ((combine (source destination)
                   (px::saturate-byte (to:f+ (truncate (* source flame-intensity))
                                             destination))))
            (pixmap:assemble-color (combine r-source r-destination)
                                   (combine g-source g-destination)
                                   (combine b-source b-destination)
                                   alpha-destination)))))))

(defmethod draw ((object thrush-exhaust) world destination destination-width destination-height)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((bitmap   bitmap)
                   (width    width)
                   (height   height)
                   (pos      pos)
                   (rotation rotation)) object
    (declare (vec2:vec2 pos))
    (declare (fixnum width))
    (let ((px:*blending-function* (blending-function-thrust-clsr world))
          (scale (if (<= 0 (random 100) 97)
                     1.0f0
                     0.8f0)))
      (declare (dynamic-extent scale))
      (declare (to::desired-type scale))
      (px:blit-transform bitmap
                         width
                         height
                         destination
                         destination-width
                         destination-height
                         0
                         0
                         (truncate (the to::desired-type (vec2:vec2-y pos)))
                         (truncate (the to::desired-type (vec2:vec2-x pos)))
                         height
                         width
                         (to:radians->degree rotation)
                         scale
                         scale
                         0
                         (truncate (ash width -1))))))

(defclass with-life ()
  ((life
    :initform 10000
    :initarg  :life
    :accessor life)))

(defmethod alivep ((object with-life))
  (> (life object) 0))

(defmethod calculate :after ((object with-life) world dt)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum dt))
  (with-accessors ((life life)) object
    (declare (fixnum life))
    (when (> life 0)
      (decf life dt))))

(defclass shield (thrush-exhaust with-life) ())

(defmethod calculate  ((object shield) world dt)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum dt))
  (with-accessors ((rotation rotation)) object
    (declare (to::desired-type rotation))
    (incf rotation (the to::desired-type (world:ticks world)))))

(defmethod draw ((object shield) world destination destination-width destination-height)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((bitmap   bitmap)
                   (width    width)
                   (height   height)
                   (pos      pos)
                   (rotation rotation)) object
    (declare (vec2:vec2 pos))
    (declare (fixnum width height))
    (let ((px:*blending-function* #'px:blending-function-combine))
      (px:blit-transform bitmap
                         width
                         height
                         destination
                         destination-width
                         destination-height
                         0
                         0
                         (truncate (the to::desired-type (vec2:vec2-y pos)))
                         (truncate (the to::desired-type (vec2:vec2-x pos)))
                         height
                         width
                         (to:radians->degree rotation)
                         (to:d+ 1.1f0
                                (* 0.1f0
                                   (to:dsin (* 4000f0 (the to::desired-type (world:ticks world))))))
                         (to:d+ 1.1f0
                                (* 0.1f0
                                   (to:dcos (* 4000f0 (the to::desired-type (world:ticks world))))))
                         (truncate (ash height -1))
                         (truncate (ash width  -1))))))

(defclass player (sprite with-direction with-rotation)
  ((current-thrust
    :initform 0f0
    :initarg  :current-thrust
    :accessor current-thrust)
   (sprite-thrust
    :initform nil
    :initarg  :sprite-thrust
    :accessor sprite-thrust)
   (sprite-shield
    :initform nil
    :initarg  :sprite-shield
    :accessor sprite-shield)
   (draw-thrust
    :initform nil
    :initarg  :draw-thrust
    :reader   draw-thrust-p
    :writer   (setf draw-thrust))))

(defmethod calculate :after ((object player) world dt)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((direction      direction)
                   (rotation       rotation)
                   (center-pos     center-pos)
                   (thrush-exhaust sprite-thrust)
                   (shield         sprite-shield)
                   (width          width)
                   (height         height)) object
    (declare (fixnum height))
    ;; thrust
    (with-accessors ((thrust-rotation   rotation)
                     (thrust-pos        pos)
                     (thrush-width      width)
                     (thrust-center-pos center-pos)) thrush-exhaust
      (declare (fixnum thrush-width))
      (declare (vec2:vec2 thrust-pos))
      (setf thrust-pos
            (vec2:vec2+ center-pos
                        (vec2:vec2* (vec2:vec2-negate direction)
                                    (/ (to:d height)
                                       2f0))))
      (decf (the to::desired-type (vec2:vec2-x thrust-pos))
            (/ (to:d thrush-width)
               2f0))
      (setf thrust-rotation rotation))
    ;; shield
    (calculate shield world dt)
    (with-accessors ((shield-pos        pos)
                     (shield-width      width)
                     (shield-height     width)
                     (shield-center-pos center-pos)) shield
      (declare (vec2:vec2 shield-pos shield-center-pos))
      (setf shield-pos
            (vec2:vec2- center-pos
                        (vec2:vec2 (ash (the fixnum shield-width) -1)
                                   (ash (the fixnum shield-height) -1)))))))

(defun apply-rotation (player rotation-entity)
  #.nodgui.config:default-optimization
  (declare (player player))
  (declare (to::desired-type rotation-entity))
  (with-accessors ((direction direction)
                   (rotation  rotation)) player
    (declare (vec2:vec2 direction))
    (declare (to::desired-type rotation))
    (incf rotation rotation-entity)
    (setf direction
          (vec2:vec2-normalize (vec2:vec2-rotate +start-player-direction+
                                                 rotation)))
    (apply-thrust player)
    player))

(defun apply-thrust (player &optional (force-entity (current-thrust player)))
  #.nodgui.config:default-optimization
  (declare (player player))
  (declare (to::desired-type force-entity))
  (with-accessors ((direction      direction)
                   (force          force)
                   (draw-thrust    draw-thrust)
                   (current-thrust current-thrust)) player
    (declare (vec2:vec2 direction force))
    (setf current-thrust force-entity)
    (setf force (vec2:vec2* direction current-thrust))
    (if (= force-entity 0f0)
        (setf draw-thrust nil)
        (setf draw-thrust t))
    player))

(defmethod draw ((object player) world destination destination-width destination-height)
  (with-accessors ((bitmap        bitmap)
                   (width         width)
                   (height        height)
                   (pos           pos)
                   (rotation      rotation)
                   (sprite-shield sprite-shield)) object
    (common-draw-rotated-sprite object destination destination-width destination-height)
    ;; thrust
    (when (draw-thrust-p object)
      (draw (sprite-thrust object) world destination destination-width destination-height))
    ;; shield
    (when (alivep sprite-shield)
      (draw sprite-shield world destination destination-width destination-height))
    #+debug-game
    (progn
      (draw-aabbs object destination destination-width destination-height)
      (let ((end-line (vec2:vec2+ (center-pos object)
                                  (vec2:vec2* (direction object)
                                              10f0))))
        (px:draw-line destination
                      destination-width
                      destination-height
                      (truncate (vec2:vec2-x (center-pos object)))
                      (truncate (vec2:vec2-y (center-pos object)))
                      (truncate (vec2:vec2-x end-line))
                      (truncate (vec2:vec2-y end-line))
                      0
                      255
                      0)))))

(defclass bullet (with-life with-bitmap with-hitpoints physical-object) ())

(defmethod initialize-instance :after ((object bullet) &key &allow-other-keys)
  (common-initialize-sprite object (bitmap object)))

(defmethod draw ((object bullet) world destination destination-width destination-height)
  (declare (ignore world))
  (common-draw-sprite object destination destination-width destination-height)
  #+debug-game
  (draw-aabb object destination destination-width destination-height))

(defmethod alivep ((object bullet))
  (and (> (hitpoints object) 0)
       (call-next-method)))

;;;; De Boor
;;;; "A Pratical Guide to Splines" 2001,  p. 90

(defun db-knot@ (knots i)
  (elt knots (- i 1)))

(defun db-w (x i k knots)
  (let ((t-i     (db-knot@ knots i))
        (t-i+k-1 (db-knot@ knots (+ i k -1))))
    (if (= t-i+k-1 t-i)
        0.0
        (/ (- x t-i)
           (- t-i+k-1 t-i)))))

(defun db-bpol (x i k knots)
  (let ((t-i   (db-knot@ knots i))
        (t-i+1 (db-knot@ knots (+ i 1))))
    (if (= k 1)
        (if (and (>= x t-i)
                 (<  x t-i+1))
            1.0
            0.0)
        (let ((w1 (db-w x    i    k knots))
              (w2 (db-w x (+ i 1) k knots)))
          (+ (* w1 (db-bpol x    i    (- k 1) knots))
             (* (- 1 w2) (db-bpol x (+ i 1) (- k 1) knots)))))))

(defun db-build-knots (control-points k)
  (loop for i from 0 below (+ (length control-points) k) collect i))

(defun db-limits (control-points degree)
  (values degree
          (length control-points)))

(defun db-interpolation (control-points &key (degree 3) (pad-control-points t))
  (let* ((k          (1+ degree))
         (actual-cps (if pad-control-points
                         (append (loop repeat degree collect
                                      (alexandria:first-elt control-points))
                                 control-points
                                 (loop repeat degree collect
                                      (alexandria:last-elt control-points)))
                         control-points))
         (length-cp (length actual-cps))
         (knots     (db-build-knots actual-cps k)))
    (multiple-value-bind (from to)
        (db-limits actual-cps degree)
      (values (lambda (s)
                (let ((sum-elem (loop for j from 1 to length-cp
                                      collect
                                      (vec2:vec2* (elt actual-cps (1- j))
                                                  (db-bpol s j k knots)))))
                  (reduce #'vec2:vec2+ sum-elem :initial-value vec2:+vec2-zero+)))
              from to))))

(defun make-ufo-trajectory (world ufo-w ufo-h number-of-control-points &optional (speed 0.01))
  (let* ((width          (world:width  world))
         (height         (world:height world))
         (control-points  (loop repeat number-of-control-points
                                collect
                                (vec2:vec2 (random width)
                                           (random height)))))
    (flet ((end-point ()
             (elt (list (vec2:vec2 (- ufo-w) (- ufo-h))
                        (vec2:vec2 (+ width ufo-w) (- ufo-h))
                        (vec2:vec2 (+ width ufo-w) (+ height ufo-h))
                        (vec2:vec2 (- ufo-w) (+ height ufo-h)))
                  (random 4))))

      (push (end-point) control-points)
      (setf control-points (reverse control-points))
      (push (end-point) control-points)
      (setf control-points (nreverse control-points))
      (multiple-value-bind (interpolator start end)
          (db-interpolation control-points)
        (lambda ()
          (if (< start end)
              (prog1
                  (funcall interpolator start)
                (incf start speed))
              (vec2:vec2 (- ufo-w) (- ufo-h))))))))

(defclass ufo (sprite with-direction with-rotation)
  ((trajectory
    :initform (lambda () (vec2:vec2 1e10
                                    1e10))
    :initarg  :trajectory
    :accessor trajectory)))

(defmethod calculate ((object ufo) world dt)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (ignore dt))
  (with-accessors ((rotation rotation)
                   (pos      pos)
                   (trajectory trajectory)) object
    (declare (to::desired-type rotation))
    (declare (vec2:vec2 pos))
    (declare (function trajectory))
    (incf rotation (the to::desired-type (world:ticks world)))
    (setf pos (funcall trajectory))))

(defmethod draw ((object ufo) world destination destination-width destination-height)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((bitmap   bitmap)
                   (width    width)
                   (height   height)
                   (pos      pos)
                   (rotation rotation)) object
    (declare (vec2:vec2 pos))
    (declare (fixnum width height))
    (common-draw-rotated-sprite object destination destination-width destination-height)
    #+debug-game
    (draw-aabbs object destination destination-width destination-height)))

(defclass after-trigger ()
  ((event-after
    :initform (constantly t)
    :initarg  :event-after
    :accessor event-after
    :type function)))

(defmethod calculate :after ((object after-trigger) world dt)
  (declare (ignore world dt))
  (with-accessors ((event-after event-after)) object
    (when (and event-after
               (not (alivep object)))
    (prog1
        (funcall event-after)
      (setf event-after nil)))))

(defclass animated-sprite (occupy-space with-life)
  ((atlas
    :initform nil
    :initarg  :atlas
    :accessor atlas)
   (frame-index
    :initform 0
    :initarg  :frame-index
    :accessor frame-index)
   (frame-number
    :initform 0
    :initarg  :frame-number
    :accessor frame-number)))

(defmethod width ((object animated-sprite))
  (px:cell-width (atlas object)))

(defmethod height ((object animated-sprite))
  (px:cell-height (atlas object)))

(defmethod alivep ((object animated-sprite))
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (< (the fixnum (frame-index object))
     (the fixnum (frame-number object))))

(defclass explosion (animated-sprite after-trigger)
  ((explosion-delay
    :initform 0
    :initarg  :explosion-delay
    :accessor explosion-delay)
   (frame-speed
    :initform 0.25f0
    :initarg  :frame-speed
    :accessor frame-speed)
   (ticks
    :initform 0f0
    :initarg  :ticks
    :accessor ticks)))

(defmethod initialize-instance :after ((object explosion) &key &allow-other-keys)
  (with-accessors ((width        width)
                   (height       height)
                   (pos        pos)
                   (center-pos center-pos)) object
    (setf (slot-value object 'pos)
          (vec2:vec2+ pos
                      (vec2:vec2-negate (vec2:vec2-insecure (to:d (/ width 2))
                                                            (to:d (/ height 2))))))
    object))

(defmethod calculate ((object explosion) world dt)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum dt))
  (declare (ignore world))
  (with-accessors ((frame-index     frame-index)
                   (frame-speed     frame-speed)
                   (explosion-delay explosion-delay)
                   (ticks           ticks)) object
    (declare (to::desired-type frame-speed ticks))
    (declare (fixnum explosion-delay))
    (if (< explosion-delay 0)
        (progn
          (incf (the to::desired-type ticks)
                (the to::desired-type frame-speed))
          (setf frame-index
                (truncate ticks)))
        (decf explosion-delay dt))))

(defmethod draw ((object explosion) world destination destination-width destination-height)
  (declare (ignore world))
  (with-accessors ((atlas           atlas)
                   (ticks           ticks)
                   (explosion-delay explosion-delay)
                   (frame-index     frame-index)
                   (frame-number    frame-number)
                   (pos             pos)) object
    (when (and (alivep object)
               (< explosion-delay 0))
      (px:blit-cell (atlas object)
                    frame-index
                    0
                    destination
                    destination-width
                    destination-height
                    (truncate (vec2:vec2-y pos))
                    (truncate (vec2:vec2-x pos))))))

(defclass particle (occupy-space)
  ((particle-children
    :initform '()
    :initarg  :particle-children
    :accessor particle-children)))

(defmethod calculate ((object particle) world dt)
  ;;#.nodgui.config:default-optimization
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((particle-children particle-children)) object
    (loop for child in particle-children do
      (calculate child world dt))))

(defmethod draw ((object particle) world destination destination-width destination-height)
  (with-accessors ((particle-children particle-children)) object
    (loop for child in particle-children do
      (draw child world destination destination-width destination-height))))

(defmethod alivep ((object particle))
  (some #'alivep (particle-children object)))

(defclass spark (with-direction physical-object)
  ((transparency
    :initform 255
    :initarg  :transparency
    :accessor transparency)
   (color
    :initform (pixmap:assemble-color #xff #xff #x00)
    :initarg  :color
    :accessor color)
   (extension
    :initform 5f0
    :initarg  :extension
    :accessor extension)))

(defmethod calculate ((object spark) world dt)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((transparency transparency)) object
    (declare (fixnum transparency))
    (integrate object dt)
    (setf transparency (max (- transparency 40) 0))))

(defmethod alivep ((object spark))
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (> (the fixnum (transparency object))
     0))

(defmethod draw ((object spark) world destination destination-width destination-height)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((color        color)
                   (transparency transparency)
                   (extension    extension)
                   (direction    direction)
                   (pos          pos)) object
    (let ((end-spark (vec2:vec2+ pos
                                   (vec2:vec2* direction extension))))
      (px::with-displace-pixel (r g b a) color
        (declare (ignore a))
        (let ((px:*blending-function* #'px:blending-function-combine))
          (px:draw-line destination
                        destination-width
                        destination-height
                        (truncate (vec2:vec2-x pos))
                        (truncate (vec2:vec2-y pos))
                        (truncate (vec2:vec2-x end-spark))
                        (truncate (vec2:vec2-y end-spark))
                        r
                        g
                        b
                        transparency))))))

(defclass sparks (particle) ())

(defmethod initialize-instance :after ((object sparks)
                                       &key
                                         (number 50)
                                         (master-direction (vec2:vec2 0 1))
                                       &allow-other-keys)
  (loop repeat number do
    (let* ((color   (pixmap:assemble-color #xff (random #xff) #x00))
           (angle   (* (to:degree->radians 300f0)
                       (approx-gaussian-random)))
           (dir     (vec2:vec2-rotate master-direction angle))
           (size    (+ 1f0 (random 10f0)))
           (spark (make-instance 'spark
                                   :velocity  (vec2:vec2* dir 0.4f0)
                                   :extension size
                                   :pos       (pos object)
                                   :color     color
                                   :direction dir)))
      (declare (dynamic-extent angle))
      (push spark (particle-children object)))))
