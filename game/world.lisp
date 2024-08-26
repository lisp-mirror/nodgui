(in-package :world)

(defparameter *sprite-bitmaps-dir* (uiop:native-namestring (asdf:system-relative-pathname :nodgui "game/data"
                                                                                          :type :directory)))

(defparameter *asteroid-bitmap-big*    (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/meteor1.png")))

(defparameter *asteroid-bitmap-medium* (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/meteor2.png")))

(defparameter *asteroid-bitmap-small*  (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/meteor3.png")))

(defparameter *asteroid-bitmap-tiny*   (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/meteor4.png")))

(defparameter *bitmap-player*          (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/player.png")))

(defparameter *bitmap-ufo*             (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/ufo.png")))

(defparameter *bitmap-background*      (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/bkg.png")))

(defparameter *bitmap-thrust*          (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/thrust.png")))

(defparameter *bitmap-shield*          (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/shield.png")))

(defparameter *bitmap-bullet*          (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/bullet.png")))

(defparameter *bitmap-explosion-big*    (pixmap:slurp-pixmap 'pixmap:png
                                                            (u:strcat *sprite-bitmaps-dir*
                                                                      "/explosions/explosion-96x96-12.png")))

(defparameter *bitmap-explosion-medium*  (pixmap:slurp-pixmap 'pixmap:png
                                                             (u:strcat *sprite-bitmaps-dir*
                                                                       "/explosions/explosion-32x32-6.png")))

(defparameter *bitmap-explosion-small*  (pixmap:slurp-pixmap 'pixmap:png
                                                             (u:strcat *sprite-bitmaps-dir*
                                                                       "/explosions/explosion-15x15-6.png")))

(defparameter *atlas-explosion-big*     (make-instance 'px:sprite-grid
                                                       :pixmap     *bitmap-explosion-big*
                                                       :cell-width  96
                                                       :cell-height 96))

(defparameter *atlas-explosion-medium*   (make-instance 'px:sprite-grid
                                                       :pixmap     *bitmap-explosion-medium*
                                                       :cell-width  32
                                                       :cell-height 32))

(defparameter *atlas-explosion-small*   (make-instance 'px:sprite-grid
                                                       :pixmap     *bitmap-explosion-small*
                                                       :cell-width  15
                                                       :cell-height 15))

(a:define-constant +world-width+ 640 :test #'=)

(a:define-constant +world-height+ 480 :test #'=)

(a:define-constant +easy-level+   :easy   :test #'eq)

(a:define-constant +medium-level+ :medium :test #'eq)

(a:define-constant +hard-level+   :hard   :test #'eq)

(defclass world ()
  ((points
    :initform 0
    :initarg :points
    :accessor points)
   (player-lives
    :initform 3
    :initarg  :player-lives
    :accessor player-lives)
   (player-shields
    :initform 3
    :initarg  :player-shields
    :accessor player-shields)
   (player-teleports
    :initform 3
    :initarg  :player-teleports
    :accessor player-teleports)
   (asteroids
    :initform '()
    :initarg  :asteroids
    :accessor asteroids)
   (effects
    :initform '()
    :initarg  :effects
    :accessor effects)
   (ufo
    :initform nil
    :initarg  :ufo
    :accessor ufo)
   (bullets
    :initform '()
    :initarg  :bullets
    :accessor bullets)
   (player
    :initform nil
    :initarg  :player
    :accessor player)
   (background
    :initform (make-instance 'e:background
                             :bitmap *bitmap-background*)
    :initarg  :background
    :accessor background)
   (width
    :initform +world-width+
    :initarg  :width
    :accessor width)
   (height
    :initform +world-height+
    :initarg  :height
    :accessor height)
   (difficult-level
    :initform +easy-level+
    :initarg  :difficult-level
    :accessor difficult-level)
   (ticks
    :initform 0f0
    :initarg  :ticks
    :accessor ticks)
   (game-finished
    :initform nil
    :initarg  :game-finished
    :accessor game-finished)))

(defmethod initialize-instance :after ((object world) &key &allow-other-keys)
  (with-accessors ((width      width)
                   (height     height)
                   (background background)) object
  (let ((max-s-coord (to:d (/ width  (pixmap:width  (e:bitmap background)))))
        (max-t-coord (to:d (/ height (pixmap:height (e:bitmap background))))))
    (setf (vec2:vec2-x (elt (e:tex-coordinates background) 1)) max-s-coord)
    (setf (vec2:vec2-x (elt (e:tex-coordinates background) 2)) max-s-coord)
    (setf (vec2:vec2-y (elt (e:tex-coordinates background) 2)) max-t-coord)
    (setf (vec2:vec2-y (elt (e:tex-coordinates background) 3)) max-t-coord)
    (setf (vec2:vec2-x (elt (e:vertices background) 1)) (to:d width))
    (setf (vec2:vec2-x (elt (e:vertices background) 2)) (to:d width))
    (setf (vec2:vec2-y (elt (e:vertices background) 2)) (to:d height))
    (setf (vec2:vec2-y (elt (e:vertices background) 3)) (to:d height))
    object)))

(defun random-position (world sprite)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (world world))
  (declare ((or e:sprite e:asteroid) sprite))
  (let ((w (width  sprite))
        (h (height sprite)))
    (declare (dynamic-extent w h))
    (declare (fixnum w h))
    (with-accessors ((width  width)
                     (height height)) world
      (declare (fixnum width height))
      (vec2:vec2-insecure (to:d (random (- width w)))
                          (to:d (random (- height h)))))))

(defun random-speed (difficult-level)
  (let* ((starting-speed (ecase difficult-level
                           (:easy
                            0.03f0)
                           (:medium
                            0.05f0)
                           (:hard
                            0.07f0)))
         (res (vec2:vec2-insecure starting-speed 0f0)))
  (vec2:vec2-rotate res (random (to:degree->radians 360.0)))))

(a:define-constant +scale-aabb-asteroid-easy+ .6f0 :test #'=)

(a:define-constant +scale-aabb-asteroid-medium+ .5f0 :test #'=)

(a:define-constant +scale-aabb-asteroid-hard+ .4f0 :test #'=)

(defun scaling-factor-target-aabb (difficult-level)
  (ecase difficult-level
    (:easy   +scale-aabb-asteroid-easy+)
    (:medium +scale-aabb-asteroid-medium+)
    (:hard   +scale-aabb-asteroid-hard+)))

(defun hitpoints-asteroids (difficult-level)
  (ecase difficult-level
    (:easy   5)
    (:medium 10)
    (:hard   15)))

(defun shield-life (difficult-level)
  (ecase difficult-level
    (:easy   10000)
    (:medium 5000)
    (:hard   5000)))

(a:define-constant +scaling-aabb-asteroid-sprite+  .65f0                :test #'=)

(a:define-constant +scaling-aabb-asteroid+         .80f0                :test #'=)

(a:define-constant +scaling-aabb-player+           .70f0                :test #'=)

(a:define-constant +asteroid-vertices-number+     7                     :test #'=)

(a:define-constant +player-mass+                   .2f0                 :test #'=)

(a:define-constant +asteroid-tiny-mass+           (* +player-mass+ 3/4) :test #'=)

(a:define-constant +asteroid-small-mass+          +player-mass+         :test #'=)

(a:define-constant +asteroid-medium-mass+         (* +player-mass+ 3)   :test #'=)

(a:define-constant +asteroid-big-mass+            (* +player-mass+ 100) :test #'=)

(defun add-sprite-asteroid (world pixmap next-generation-size mass &optional position)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 0) (debug 3) (safety 3)))
  (with-accessors ((difficult-level difficult-level)) world
    (let ((asteroid (make-instance 'e:sprite-asteroid
                                   :mass         mass
                                   :bitmap       pixmap
                                   :scaling-aabb +scaling-aabb-asteroid-sprite+
                                   :scaling-target-aabb
                                   (scaling-factor-target-aabb difficult-level)
                                   :next-generation-size next-generation-size)))
      (setf (e:pos asteroid)      (or position
                                      (random-position world asteroid)))
      (setf (e:velocity asteroid) (random-speed difficult-level))
      (push asteroid (asteroids world)))))

(defun add-sprite-asteroid-big (world)
  (add-sprite-asteroid world
                       *asteroid-bitmap-big*
                       :medium
                       +asteroid-big-mass+))

(defun add-sprite-asteroid-medium (world &optional position)
  (add-sprite-asteroid world
                       *asteroid-bitmap-medium*
                       :small
                       +asteroid-medium-mass+
                       position))

(defun add-sprite-asteroid-small (world &optional position)
  (add-sprite-asteroid world
                       *asteroid-bitmap-small*
                       :tiny
                       +asteroid-small-mass+
                       position))

(defun add-sprite-asteroid-tiny (world &optional position)
  (add-sprite-asteroid world
                       *asteroid-bitmap-tiny*
                       :stop
                       +asteroid-tiny-mass+
                       position))

(defun add-asteroid (world radius next-generation-size mass &optional (position nil))
  (with-accessors ((difficult-level difficult-level)) world
    (let ((asteroid (make-instance 'e:asteroid
                                   :mass mass
                                   :next-generation-size next-generation-size
                                   :hitpoints    (hitpoints-asteroids difficult-level)
                                   :scaling-aabb +scaling-aabb-asteroid+
                                   :scaling-target-aabb
                                   (scaling-factor-target-aabb difficult-level)
                                   :vertices
                                   (e:make-asteroid-vertices radius
                                                             +asteroid-vertices-number+))))
      (setf (e:pos asteroid)      (or position
                                      (random-position world asteroid)))
      (setf (e:velocity asteroid) (random-speed difficult-level))
      (push asteroid (asteroids world)))))

(a:define-constant +radius-asteroid-big+    45f0 :test #'=)

(a:define-constant +radius-asteroid-medium+ 25f0 :test #'=)

(a:define-constant +radius-asteroid-small+  15f0 :test #'=)

(a:define-constant +radius-asteroid-tiny+   10f0 :test #'=)

(defun add-asteroid-big (world)
  (add-asteroid world
                +radius-asteroid-big+
                :medium
                +asteroid-big-mass+))

(defun add-asteroid-medium (world &optional position)
  (add-asteroid world
                +radius-asteroid-medium+
                :small
                +asteroid-medium-mass+
                position))

(defun add-asteroid-small (world &optional position)
  (add-asteroid world
                +radius-asteroid-small+
                :tiny
                +asteroid-small-mass+
                position))

(defun add-asteroid-tiny (world &optional position)
  (add-asteroid world
                +radius-asteroid-tiny+
                :stop
                +asteroid-tiny-mass+
                position))

(defun spawn-number (difficult-level)
  (ecase difficult-level
    (:easy   2)
    (:medium 3)
    (:hard   3)))

(defun spawn-asteroid (world asteroid)
  (let ((spawn-number (spawn-number (difficult-level world))))
    (typecase asteroid
      (e:sprite-asteroid
       (case (e:next-generation-size asteroid)
         (:medium
          (add-explosion-big world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-sprite-asteroid-medium world (e:center-pos asteroid))))
         (:small
          (add-explosion-medium world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-sprite-asteroid-small world (e:center-pos asteroid))))
         (:tiny
          (add-explosion-small world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-sprite-asteroid-tiny world (e:center-pos asteroid))))
         (:stop
          (add-explosion-tiny world (e:center-pos asteroid)))))
      (e:asteroid
       (case (e:next-generation-size asteroid)
         (:medium
          (add-explosion-big world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-asteroid-medium world (e:center-pos asteroid))))
         (:small
          (add-explosion-medium world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-asteroid-small world (e:center-pos asteroid))))
         (:tiny
          (add-explosion-small world (e:center-pos asteroid))
          (loop repeat spawn-number do
            (add-asteroid-tiny world (e:center-pos asteroid))))
         (:stop
          (add-explosion-tiny world (e:center-pos asteroid))))))))

(defun player-starting-lives (difficult-level)
  (ecase difficult-level
    (:easy   4)
    (:medium 3)
    (:hard   3)))

(defun player-starting-teleport (difficult-level)
  (ecase difficult-level
    (:easy   5)
    (:medium 4)
    (:hard   3)))

(defun player-starting-shield (difficult-level)
  (ecase difficult-level
    (:easy   4)
    (:medium 3)
    (:hard   3)))

(defun teleport-safe-distance (difficult-level)
  (ecase difficult-level
    (:easy   2f0)
    (:medium 1.5f0)
    (:hard   1f0)))

(defun teleport-player (world)
  (let* ((player (player world))
         (position (random-position world player)))
    ;; (setf pos) will fits the aabb too
    (setf (e:pos player) position)
    (let ((player-aabb (e:aabb player))
          (scale-aabb  (teleport-safe-distance (difficult-level world))))
      (flet ((test-intersect (a)
               (aabb2:aabb2-intersect-p (aabb2:scale-aabb2 (e:aabb a)
                                                           scale-aabb
                                                           scale-aabb)
                                        player-aabb)))
        (if (not (or (some #'test-intersect (asteroids world))
                     (some #'test-intersect (bullets world))
                     (test-intersect (ufo world))))
            position
            (teleport-player world))))))

(defun add-player (world)
  (with-accessors ((difficult-level difficult-level)) world
    (let* ((thrust  (make-instance 'e:thrush-exhaust
                                   :bitmap *bitmap-thrust*))
           (shield  (make-instance 'e:shield
                                   :life   0
                                   :bitmap *bitmap-shield*))
           (player  (make-instance 'e:player
                                   :hitpoints           (player-starting-lives difficult-level)
                                   :sprite-shield       shield
                                   :sprite-thrust       thrust
                                   :bitmap              *bitmap-player*
                                   :scaling-aabb        +scaling-aabb-player+
                                   :scaling-target-aabb +scaling-aabb-player+)))
      (setf (player world) player)
      (teleport-player world))))

(defgeneric fire-bullet (world object))

(a:define-constant +bullet-speed+ 2e-1 :test #'=)

(defun bullet-life (difficult-level)
  (ecase difficult-level
    (:easy   2000)
    (:medium 2000)
    (:hard   1000)))

(defmethod fire-bullet ((world world) (object e:player))
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((difficult-level difficult-level)) world
    (let* ((bullet-velocity        (vec2:vec2* (e:direction object)
                                               +bullet-speed+))
           (bullet                 (make-instance 'e:bullet
                                                  :velocity bullet-velocity
                                                  :bitmap   *bitmap-bullet*
                                                  :life     (bullet-life difficult-level)))
           (player-half-height     (to:d (ash (the fixnum
                                                   (e:height object))
                                              -1)))
           (bullet-half-width      (to:d (ash (the fixnum
                                                   (e:width bullet))
                                              -1)))
           (bullet-height          (to:d (the fixnum (e:height bullet))))
           (bullet-launch-position
             (vec2:vec2+ (e:center-pos object)
                         (vec2:vec2- (vec2:vec2* (e:direction object)
                                                 (to:d+ 0.6f0
                                                        bullet-height
                                                        player-half-height))
                                     (vec2:vec2-insecure bullet-half-width
                                                         0f0)))))
      (setf (e:pos bullet) bullet-launch-position)
      (push bullet (bullets world)))))

(defun add-shield (world)
  (setf (e:life (e:sprite-shield (player world)))
        (shield-life (difficult-level world))))

(defun ufo-control-points (difficult-level)
  (ecase difficult-level
    (:easy   5)
    (:medium 10)
    (:hard   15)))

(defun ufo-speed (difficult-level)
  (ecase difficult-level
    (:easy   0.01)
    (:medium 0.02)
    (:hard   0.025)))

(defun add-ufo (world)
  (let ((ufo (make-instance 'e:ufo
                            :bitmap *bitmap-ufo*)))
    (setf (ufo world) ufo)))

(defun revive-ufo (world)
  (with-accessors ((ufo ufo)) world
    (setf (e:hitpoints ufo) 1)
    (setf (e:trajectory ufo)
          (e:make-ufo-trajectory world
                                 (e:width ufo)
                                 (e:height ufo)
                                 (ufo-control-points (difficult-level world))))))
(let ((time (random 1000)))
  (declare (fixnum time))
  (declare (dynamic-extent time))
  (defun ufo-revive-p (world)
    #.nodgui.config:default-optimization
    ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
    (declare (world world))
    (incf time)
    (ecase (difficult-level world)
      (:easy   nil) ; no UFO on easy level
      (:medium (= (rem time 3500)
                  0))
      (:hard   (= (rem time 3000)
                  0)))))

(defun initialize-game (world)
  (setf (game-finished world) nil)
  (setf (asteroids world) '())
  (setf (bullets world) '())
  (let ((difficult-level (difficult-level world)))
    (ecase difficult-level
      (:easy
       (add-sprite-asteroid-medium world)
       (loop repeat 2 do
         (add-sprite-asteroid-small world))
       (add-sprite-asteroid-tiny world))
      (:medium
       (add-sprite-asteroid-big world)
       (loop repeat 2 do
         (add-sprite-asteroid-medium world))
       (add-sprite-asteroid-small world))
      (:hard
       (add-asteroid-big world)
       (loop repeat 2 do
         (add-sprite-asteroid-big world))
       (add-sprite-asteroid-medium world)))
    (add-ufo world)
    (add-player world)
    (setf (points world) 0)
    (setf (e:hitpoints (player world))
          (player-starting-lives difficult-level))
    (setf (player-shields world)
          (player-starting-shield difficult-level))
    (setf (player-teleports world)
          (player-starting-teleport difficult-level))
    (print-points world)
    (print-hitpoints (player world))
    (print-shields world)
    (print-teleports world)))

(a:define-constant +game-over-messages+ '("Mission failed!~%Game over."
                                          "The forces of evil won again!~%Game over.")
  :test #'equalp)

(defun game-over-messages ()
  (format nil
          (elt +game-over-messages+
               (random (length +game-over-messages+)))))

(defun victory-message (world)
  (ecase (difficult-level world)
    (:easy
     (format nil "You have won!~%Try a more difficult level!"))
    (:medium
     (format nil "You have won!~%Try a more difficult level!"))
    (:hard
     (format nil "Congratulations! You have won!"))))

(defun print-hitpoints (player)
  (let ((*wish* (main::gui-server)))
    (setf (text (main::nodgui-player-lives))
          (format nil  "~a" (e:hitpoints player)))))

(defun print-points (world)
  (let ((*wish* (main::gui-server)))
    (setf (text (main::nodgui-points))
          (format nil  "~a" (points world)))))

(defun print-teleports (world)
  (let ((*wish* (main::gui-server)))
    (setf (text (main::nodgui-player-teleports))
          (format nil  "~a" (player-teleports world)))))

(defun print-shields (world)
  (let ((*wish* (main::gui-server)))
    (setf (text (main::nodgui-player-shields))
          (format nil  "~a" (player-shields world)))))

(defun shield-active-p (player)
  (e:alivep (e:sprite-shield player)))

(defun player-tip (player)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vec2:vec2+ (e:center-pos player)
              (vec2:vec2-insecure (to:d (ash (the fixnum (e:width player))
                                             -1))
                                  0f0)))

(defun player-damage (world player)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (when (not (shield-active-p player))
    (decf (the fixnum (e:hitpoints player))))
  (let ((player (world:player world)))
    (print-hitpoints player)
    (when (not (e:alivep player))
      (let ((explosion-min-width-pos  (to:d (* -2 (the fixnum (width player)))))
            (explosion-min-height-pos (to:d (* -2 (the fixnum (height player)))))
            (explosion-max-width-pos  (to:d (*  4 (the fixnum (width player)))))
            (explosion-max-height-pos (to:d (*  4 (the fixnum (height player))))))
        (loop for delay from 10 to 100 by 10 do
          (let ((explosion
                  (add-explosion-medium world
                                        (vec2:vec2+
                                         (e:center-pos player)
                                         (vec2:vec2-insecure (+ explosion-min-width-pos
                                                                (random explosion-max-width-pos))
                                                             (+ explosion-min-height-pos
                                                                (random explosion-max-height-pos)))))))
            (setf (e:explosion-delay explosion) delay)))
        (let ((explosion (add-explosion-big world (e:center-pos player))))
          (setf (e:event-after explosion)
                (lambda ()
                  (let ((*wish* (main::gui-server)))
                    (message-box (game-over-messages)
                                 "Failure"
                                 +message-box-type-ok+
                                 +message-box-icon-error+)))))))))

(defun add-explosion (world atlas frame-number position)
  (let ((fx (make-instance 'e:explosion
                           :atlas        atlas
                           :frame-number frame-number
                           :pos          position)))
    (push fx (effects world))
    fx))

(defun add-explosion-big (world position)
  (add-explosion world
                 *atlas-explosion-big*
                 12
                 position))

(defun add-explosion-medium (world position)
  (add-explosion world
                 *atlas-explosion-medium*
                 6
                 position))

(defun add-explosion-small (world position)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for delay  from 0 to 20 by 10
        for offset = (vec2:vec2 (* 5 (cos (random (1+ delay))))
                                (* 5 (sin (random (1+ delay)))))
        then (vec2:vec2 (* 5 (cos (random delay)))
                                (* 5 (sin (random delay))))
        do
    (let ((fx (add-explosion world
                             *atlas-explosion-small*
                             6
                             (vec2:vec2+ position
                                         offset))))
      (setf (e:explosion-delay fx) delay))))

(defun add-explosion-tiny (world position)
  (add-explosion world
                 *atlas-explosion-small*
                 6
                 position))

(defun add-sparks (world pos direction number)
  (let ((fx (make-instance 'e:sparks
                           :number           number
                           :pos              pos
                           :master-direction direction)))
    (push fx (effects world))
    fx))

(defun check-hitted-entities (world)
  (flet ((add-bullet-sparks (bullet number)
           (add-sparks world
                         (e:pos bullet)
                         (vec2:vec2-negate (vec2:vec2-normalize (e:velocity bullet)))
                         number))
         (asteroid->sparks-number (asteroid)
           (case (e:next-generation-size asteroid)
             (:medium 50)
             (:small  20)
             (:tiny   10)
             (:stop    5))))
    (with-accessors ((bullets   bullets)
                     (asteroids asteroids)
                     (ufo       ufo)
                     (player    player)) world
      (loop for asteroid in asteroids do
        (loop for bullet in bullets
              when (and (e:alivep bullet)
                        (aabb2:aabb2-intersect-p (e:aabb bullet)
                                                 (e:target-aabb asteroid)))
                do
                   (add-bullet-sparks bullet
                                      (asteroid->sparks-number asteroid))
                   (incf (world:points world) 100)
                   (print-points world)
                   (decf (e:hitpoints bullet))
                   (decf (e:hitpoints asteroid))))
      (loop for bullet in bullets
            when (e:alivep bullet)
            do
               (when (and (e:alivep ufo)
                          (aabb2:aabb2-intersect-p (e:aabb bullet)
                                                   (e:target-aabb ufo)))
                 (add-explosion-medium world (e:center-pos ufo))
                 (add-bullet-sparks bullet 30)
                 (decf (e:hitpoints bullet))
                 (decf (e:hitpoints ufo)))
               (when (aabb2:aabb2-intersect-p (e:aabb bullet)
                                              (e:target-aabb player))
                 (add-bullet-sparks bullet 30)
                 (decf (e:hitpoints bullet))
                 (player-damage world player))))))

(defun speeds-after-collisions (v1 v2 m1 m2)
  #.nodgui.config:default-optimization
  ;;(declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (vec2:vec2 v1 v2))
  (declare (to::desired-type m1 m2))
  (let ((mass-sum (+ m1 m2)))
    (declare (dynamic-extent mass-sum))
    (values (vec2:vec2/ (vec2:vec2+ (vec2:vec2* v1 (- m1 m2))
                                    (vec2:vec2* v2
                                                (* 2f0 m2)))
                        mass-sum)
            (vec2:vec2/ (vec2:vec2+ (vec2:vec2* v2 (- m2 m1))
                                    (vec2:vec2* v1
                                                (* 2f0 m1)))
                        mass-sum))))

(defun check-damaged-player (world)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-accessors ((asteroids asteroids)
                   (ufo       ufo)
                   (player    player)) world
    (loop for asteroid in asteroids
          when (aabb2:aabb2-intersect-p (e:aabb player)
                                        (e:aabb asteroid))
            do
               (player-damage world player)
               (multiple-value-bind (v-asteroid v-player)
                   (speeds-after-collisions (e:velocity asteroid)
                                            (e:velocity player)
                                            (e:mass asteroid)
                                            (e:mass player))
                 (setf (e:velocity player) v-player)
                 (setf (e:velocity asteroid) v-asteroid)
                 (return-from check-damaged-player t)))
    (when (aabb2:aabb2-intersect-p (e:aabb player)
                                   (e:aabb ufo))
      (player-damage world player))))

(defmethod e:calculate ((object world) world dt)
  (declare (ignore world))
  (with-accessors ((asteroids     asteroids)
                   (bullets       bullets)
                   (effects       effects)
                   (player        player)
                   (game-finished game-finished)
                   (ufo           ufo)) object
    (when (ufo-revive-p object)
      (revive-ufo object))
    (check-hitted-entities object)
    (when (e:alivep player)
      (check-damaged-player object))
    (loop for asteroid in asteroids
          when (not (e:alivep asteroid))
            do
               (spawn-asteroid object asteroid))
    (setf asteroids (remove-if-not #'e:alivep asteroids))
    (setf bullets   (remove-if-not #'e:alivep bullets))
    (setf effects   (remove-if-not #'e:alivep effects))
     (loop for entity in asteroids do
      (e:calculate entity object dt))
    (loop for bullet in bullets do
      (e:calculate bullet object dt))
    (e:calculate ufo object dt)
    (e:calculate player object dt)
    (loop for effect in effects do
      (e:calculate effect object dt))
    (when (and (not game-finished)
               (null asteroids))
      (setf game-finished t)
      (let ((*wish* (main::gui-server)))
        (message-box (victory-message object)
                     "Success"
                     +message-box-type-ok+
                     +message-box-icon-error+)))))

(defmethod e:draw ((object world) world destination destination-width destination-height)
  (declare (ignore world))
  (with-accessors ((asteroids  asteroids)
                   (player     player)
                   (bullets   bullets)
                   (effects   effects)
                   (background background)
                   (ufo        ufo)) object
    (e:draw (background object) object destination destination-width destination-height)
    (when (e:alivep player)
      (e:draw player object destination destination-width destination-height))
    (loop for entity in asteroids do
      (e:draw entity object destination destination-width destination-height))
    (loop for bullet in bullets do
      (e:draw bullet object destination destination-width destination-height))
    (when (e:alivep ufo)
      (e:draw ufo object destination destination-width destination-height))
    (loop for effect in effects do
      (e:draw effect object destination destination-width destination-height))))
