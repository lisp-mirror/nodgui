(in-package :main)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defstruct animation
  (thread)
  (lock (u:make-lock))
  (stop-p nil))

(defgeneric stop-drawing-thread (object))

(defgeneric stop-drawing-thread-p (object))

(defgeneric wait-thread (object))

(defmethod stop-drawing-thread ((object animation))
  (u:with-lock-held ((animation-lock object))
    (setf (animation-stop-p object) t)))

(defmethod stop-drawing-thread-p ((object animation))
  (u:with-lock-held ((animation-lock object))
    (animation-stop-p object)))

(defmethod wait-thread ((object animation))
  (u:join-thread (animation-thread object)))

(defparameter *animation*  nil)

(defparameter *game-world* (make-instance 'world:world))

(defparameter *pixel-buffer-context* nil)

(defun game-thread ()
  (with-accessors ((buffer        px:buffer)
                   (buffer-width  ctx:width)
                   (buffer-height ctx:height)) *pixel-buffer-context*
    (loop while (not (stop-drawing-thread-p *animation*)) do
      (ctx:sync *pixel-buffer-context*)
      (ctx:push-for-updating *pixel-buffer-context*
                             (lambda (dt)
                               (declare (fixnum dt))
                               ;; (declare (optimize (speed 3) (debug 0)))
                               (incf (world:ticks *game-world*)
                                     (to:d* 1e-6 (to:d dt)))
                               (e:calculate *game-world* nil dt))
                             :force-push nil)
      (ctx:push-for-rendering *pixel-buffer-context*
                              (lambda (dt)
                                (declare (ignore dt))
                                ;; (declare (optimize (speed 3) (debug 0)))
                                (e:draw *game-world* nil buffer buffer-width buffer-height))
                              :force-push nil))
      (format t "STOP game!~%")))

(defun stop-animation ()
  (when (and *animation*
             (u:threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)
    (format t
            "anim ~a queue ~a~%"
            *animation*
            (ctx::rendering-queue *pixel-buffer-context*))))

(defun quit-game ()
  (stop-animation)
  (ctx:quit-sdl *pixel-buffer-context*)
  (exit-nodgui))

(defun new-game (&optional (difficult-level :easy))
  (stop-animation)
  (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                          (declare (ignore dt))
    (setf (world:difficult-level *game-world*) difficult-level)
    (world:initialize-game *game-world*)
    (setf *animation* (make-animation))
    (setf (animation-thread *animation*)
          (u:make-thread #'game-thread :name "game"))))


(defun initialize-menu (toplevel)
  (let* ((bar       (make-menubar toplevel))
         (game      (make-menu bar  "Game"))
         (help      (make-menu bar  "Help"))
         (difficult (make-menu game "Difficult level")))
    (make-menubutton game "Quit" #'quit-game)
    (make-menubutton difficult "easy" (lambda () (new-game :easy)))
    (make-menubutton difficult "medium" (lambda () (new-game :medium)))
    (make-menubutton difficult "hard" (lambda () (new-game :hard)))
    (nodgui:make-menubutton help
                            "About"
                            (lambda ()
                              (let ((master (nodgui:root-toplevel)))
                                (nodgui:with-toplevel (toplevel :master master
                                                                :title "About")
                                  (nodgui:transient toplevel master)
                                  (let ((text-widget (make-instance 'nodgui:scrolled-text
                                                                    :master toplevel
                                                                    :cursor nodgui:+standard-cursor+
                                                                    :use-horizontal-scrolling-p nil
                                                                    :read-only t)))
                                    (setf (nodgui:text text-widget)
                                          (format nil "© cage Released under GPLv3+~%for information: https://www.autistici.org/interzona/nodgui.html"))
                                    (pack text-widget))))))))

(defun clear-sdl-window (&key (context *pixel-buffer-context*) (force nil))
  (with-accessors ((buffer px:buffer)
                   (width  ctx:width)
                   (height ctx:height)) context
    (ctx:push-for-updating *pixel-buffer-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             (declare (optimize (speed 3) (debug 0)))
                             t)
                           :force-push force)
    (ctx:push-for-rendering context
                            (lambda (dt)
                              (declare (ignore dt))
                              (px:clear-buffer buffer width height 100 100 100))
                            :force-push force)))

(a:define-constant +r-transparent-color+ #xff :test #'=)

(a:define-constant +g-transparent-color+ #x00 :test #'=)

(a:define-constant +b-transparent-color+ #xff :test #'=)

(defun globally-set-blending-function ()
  (setf px:*blending-function*
        (px:make-blending-fn-replace-with-transparent-color +r-transparent-color+
                                                            +g-transparent-color+
                                                            +b-transparent-color+)))
(defmacro wait-do (delay fn &rest args)
  `(nodgui.utils:make-thread
    (lambda () (sleep ,delay)
      ,(if args
           `(apply ,fn ,@args)
           `(funcall ,fn)))))

(a:define-constant +rotation-clockwise-entity+ (to:degree->radians -10f0) :test #'=)

(defun set-ui-keybindings ()
  (bind (root-toplevel)
        #$<Control-q>$
        (lambda (event)
          (declare (ignore event))
          (quit-game)))
  (bind (root-toplevel)
        #$<Control-n>$
        (lambda (event)
          (declare (ignore event))
          (new-game (world:difficult-level *game-world*)))))

(defun player-fire ()
  (world:fire-bullet *game-world* (world:player *game-world*)))

(defun player-shield ()
  (when (and (> (world:player-shields *game-world*)
                0)
             (not (world:shield-active-p (world:player *game-world*))))
    (decf (world:player-shields *game-world*))
    (world:add-shield *game-world*)
    (world::print-shields *game-world*)))

(defun player-teleport ()
  (when (> (world:player-teleports *game-world*)
           0)
    (decf (world:player-teleports *game-world*))
    (world::print-teleports *game-world*)
    (world:teleport-player *game-world*)))

(a:define-constant +fire-frequency+ 600 :test #'=)

(defun set-game-keybindings (sdl-frame)
  (bind sdl-frame
        #$<KeyPress-Left>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                  (declare (ignore dt))
            (e:apply-rotation (world:player *game-world*)
                              +rotation-clockwise-entity+))))
  (bind sdl-frame
        #$<KeyPress-Right>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                  (declare (ignore dt))
            (e:apply-rotation (world:player *game-world*)
                              (to:d- +rotation-clockwise-entity+)))))
  (bind sdl-frame
        #$<KeyPress-Up>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                  (declare (ignore dt))
            (e:apply-thrust (world:player *game-world*) 1e-5))))
  (bind sdl-frame
        #$<KeyPress-Down>$
        (lambda (event)
          (declare (ignore event))
          (e:apply-thrust (world:player *game-world*) 0f0)))
  (bind sdl-frame
        #$<KeyPress-d>$
        (let ((*lambda-frequency* +fire-frequency+))
          (lambda-fixed-frequency (event)
            (declare (ignore event))
            (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                    (declare (ignore dt))
              (player-fire)))))
  (bind sdl-frame
        #$<KeyPress-s>$
        (let ((*lambda-frequency* +fire-frequency+))
          (lambda-fixed-frequency (event)
            (declare (ignore event))
            (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                    (declare (ignore dt))
              (player-shield)))))
  (bind sdl-frame
        #$<KeyPress-a>$
        (let ((*lambda-frequency* +fire-frequency+))
          (lambda-fixed-frequency (event)
            (declare (ignore event))
            (ctx:in-renderer-thread (*pixel-buffer-context* dt)
                                    (declare (ignore dt))
              (player-teleport))))))

(defparameter *nodgui-points*       nil)

(defparameter *nodgui-player-lives* nil)

(defparameter *nodgui-player-shields* nil)

(defparameter *nodgui-player-teleports* nil)

(defparameter *gui-server*          nil)

(defun gui-server ()
  *gui-server*)

(defun nodgui-points ()
  *nodgui-points*)

(defun nodgui-player-lives ()
  *nodgui-player-lives*)

(defun nodgui-player-shields ()
  *nodgui-player-shields*)

(defun nodgui-player-teleports ()
  *nodgui-player-teleports*)

(a:define-constant +font-h1+ "sans 20 bold" :test #'string=)

(a:define-constant +font-h2+ "sans 15 bold" :test #'string=)

(defun main ()
  (globally-set-blending-function)
  (with-nodgui ()
    (setf *gui-server* *wish*)
    (let* ((sdl-frame    (ctx:make-sdl-frame (* 2 world:+world-width+)
                                             (* 2 world:+world-height+)))
           (frame        (make-instance 'frame))
           (points-label (make-instance 'label
                                        :master frame
                                        :font +font-h1+
                                        :text "Points"))
           (points       (make-instance 'label
                                        :font +font-h2+
                                        :master frame
                                        :text (format nil
                                                      "~a"
                                                      (world:points *game-world*))))
           (lives-label  (make-instance 'label
                                        :master frame
                                        :font +font-h1+
                                        :text "Lives"))
           (lives        (make-instance 'label
                                        :master frame
                                        :font +font-h2+
                                        :text (format nil
                                                      "~a"
                                                      (world:player-lives *game-world*))))
           (shields-label  (make-instance 'label
                                        :master frame
                                        :font +font-h1+
                                        :text "Shields"))
           (shields        (make-instance 'label
                                        :master frame
                                        :font +font-h2+
                                        :text (format nil
                                                      "~a"
                                                      (world:player-shields *game-world*))))
           (teleports-label  (make-instance 'label
                                        :master frame
                                        :font +font-h1+
                                        :text "Teleports"))
           (teleports        (make-instance 'label
                                        :master frame
                                        :font +font-h2+
                                        :text (format nil
                                                      "~a"
                                                      (world:player-teleports *game-world*)))))
      (setf *nodgui-points* points)
      (setf *nodgui-player-lives* lives)
      (setf *nodgui-player-shields* shields)
      (setf *nodgui-player-teleports* teleports)
      (initialize-menu (nodgui:root-toplevel))
      (grid sdl-frame                0 0)
      (grid frame                    0 1 :sticky :news)
      (grid points-label             1 0 :sticky :n)
      (grid points                   2 0 :sticky :n)
      (grid lives-label              3 0 :sticky :n)
      (grid lives                    4 0 :sticky :n)
      (grid shields-label            5 0 :sticky :n)
      (grid shields                  6 0 :sticky :n)
      (grid teleports-label          7 0 :sticky :n)
      (grid teleports                8 0 :sticky :n)
      (wait-complete-redraw)
      (setf *pixel-buffer-context*
            (make-instance 'px:pixel-buffer-context
                           :minimum-delta-t (ctx:fps->delta-t 30)
                           :non-blocking-queue-maximum-size 16
                           :classic-frame sdl-frame
                           :buffer-width  world:+world-width+
                           :buffer-height world:+world-height+))
      (force-focus sdl-frame)
      (clear-sdl-window :force t)
      (world:initialize-game *game-world*)
      (set-game-keybindings sdl-frame)
      (set-ui-keybindings)
      (setf *animation* (make-animation))
      (setf (animation-thread *animation*)
            (u:make-thread #'game-thread :name "game")))))
