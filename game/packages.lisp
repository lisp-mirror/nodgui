(defpackage :aabb2
  (:use
   :cl
   :nodgui)
  (:local-nicknames (:a      :alexandria)
                    (:q      :nodgui.non-blocking-queue)
                    (:to     :nodgui.typed-operations)
                    (:ctx    :nodgui.rendering-buffer-context)
                    (:px     :nodgui.pixels-canvas)
                    (:pixmap :nodgui.pixmap)
                    (:matrix :nodgui.matrix)
                    (:vec3   :nodgui.vec3)
                    (:vec2   :nodgui.vec2)
                    (:u      :nodgui.utils))
  (:export
   :make-aabb2
   :aabb2~
   :aabb2-min-x
   :aabb2-max-x
   :aabb2-min-y
   :aabb2-max-y
   :valid-aabb2-p
   :expand-aabb2
   :nexpand-aabb2
   :expand-corners-aabb2
   :nexpand-aabb2
   :union-aabb2
   :aabb2->rect2
   :aabb2-width
   :aabb2-height
   :aabb2-area
   :rect2->aabb2
   :inside-aabb2-p
   :aabb2-intersect-p
   :aabb2-inglobe-p
   :approx-aabb2-intersect-p
   :aabb2-null-p
   :trasl-aabb2
   :trasl-rect2
   :rotate-aabb2*
   :center-aabb2
   :rotate-aabb2
   :scale-aabb2))

(defpackage :entities
  (:use
   :cl
   :nodgui)
  (:local-nicknames (:a      :alexandria)
                    (:q      :nodgui.non-blocking-queue)
                    (:to     :nodgui.typed-operations)
                    (:ctx    :nodgui.rendering-buffer-context)
                    (:px     :nodgui.pixels-canvas)
                    (:pixmap :nodgui.pixmap)
                    (:matrix :nodgui.matrix)
                    (:vec3   :nodgui.vec3)
                    (:vec2   :nodgui.vec2)
                    (:u      :nodgui.utils))
  (:export
   :alivep
   :entity
   :aabb
   :scaling-aabb
   :pos
   :center-pos
   :force
   :velocity
   :acceleration
   :mass
   :hitpoints
   :target-aabb
   :scaling-target-aabb
   :destroyedp
   :bitmap
   :sprite
   :next-generation-size
   :sprite-asteroid
   :width
   :height
   :calculate
   :draw
   :polygon
   :vertices
   :bitmap
   :tex-coordinates
   :background
   :asteroid
   :make-asteroid-vertices
   :direction
   :rotation
   :thrush-exhaust
   :life
   :shield
   :player
   :sprite-shield
   :apply-rotation
   :apply-thrust
   :bullet
   :ufo
   :trajectory
   :make-ufo-trajectory
   :event-after
   :animated-sprite
   :atlas
   :frame-index
   :frame-number
   :explosion
   :explosion-delay
   :frame-speed
   :ticks
   :sparks))

(defpackage :world
  (:use
   :cl
   :nodgui)
  (:local-nicknames (:a      :alexandria)
                    (:q      :nodgui.non-blocking-queue)
                    (:to     :nodgui.typed-operations)
                    (:ctx    :nodgui.rendering-buffer-context)
                    (:px     :nodgui.pixels-canvas)
                    (:pixmap :nodgui.pixmap)
                    (:matrix :nodgui.matrix)
                    (:vec3   :nodgui.vec3)
                    (:vec2   :nodgui.vec2)
                    (:u      :nodgui.utils)
                    (:e      :entities))
  (:export
   :+world-width+
   :+world-height+
   :difficult-level
   :world
   :width
   :height
   :asteroids
   :player
   :ticks
   :initialize-game
   :fire-bullet
   :add-shield
   :points
   :player-lives
   :player-shields
   :player-teleports
   :shield-active-p
   :teleport-player))

(defpackage :main
  (:use
   :cl
   :nodgui)
  (:local-nicknames (:a      :alexandria)
                    (:q      :nodgui.non-blocking-queue)
                    (:to     :nodgui.typed-operations)
                    (:ctx    :nodgui.rendering-buffer-context)
                    (:px     :nodgui.pixels-canvas)
                    (:pixmap :nodgui.pixmap)
                    (:matrix :nodgui.matrix)
                    (:vec3   :nodgui.vec3)
                    (:vec2   :nodgui.vec2)
                    (:u      :nodgui.utils)
                    (:e      :entities))
  (:export))
