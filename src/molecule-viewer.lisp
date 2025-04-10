;; derived from:
;; Doug Hoyte, March 2007
;; Assignment 2: Molecule Viewer
;; COSC414 with Dr. Alan Paeth, UBC Okanagan

;; This program is distributed under the terms and
;; conditions of the BSD license:
;; http://www.opensource.org/licenses/bsd-license.php

;; Usage:
;; $ sbcl --load molview.lisp

;; Commands:
;; x/X - Rotate molecule around X axis
;; y/Y - Rotate molecule around Y axis
;; z/Z - Rotate molecule around Z axis
;; t/T - Rotate light source around theta
;; p/P - Rotate light source around phi
;; r/R - Increase/decrease red light component
;; g/G - Increase/decrease green light component
;; b/B - Increase/decrease blue light component
;; s/S - Increase/decrease number of sphere slices
;; w   - Toggle "random walk" frame-rate testing mode
;; m   - Toggle solid sphere vs wire sphere
;; l   - Toggle showing light source

(asdf:make :nodgui)

(asdf:make :cl-glut)

(defpackage #:molecule-viewer
  (:use :cl
   :nodgui)
  (:local-nicknames (:a       :alexandria)
                    (:bq      :syncronized-queue)
                    (:q       :nodgui.non-blocking-queue)
                    (:to      :nodgui.typed-operations)
                    (:u       :nodgui.utils)
                    (:pix     :nodgui.pixmap)
                    (:ctx     :nodgui.rendering-buffer-context)
                    (:pixbuff :nodgui.pixels-canvas)
                    (:3d      :nodgui.opengl-frame))
  (:export #:main))

(in-package #:molecule-viewer)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

;; Hard-coded molecules

(alexandria:define-constant +water+
  '((O  0.000 0.000 0.000)
    (H -0.900 0.000 0.000)
    (H  0.000 1.000 0.000))
  :test 'equal)

(alexandria:define-constant +ethanol+
  '((C -0.426  -0.115  -0.147)
    (O -0.599   1.244  -0.481)
    (H -0.750  -0.738  -0.981)
    (H -1.022  -0.351   0.735)
    (H -1.642   1.434  -0.689)
    (C  1.047  -0.383   0.147)
    (H  1.370   0.240   0.981)
    (H  1.642  -0.147  -0.735)
    (H  1.180  -1.434   0.405))
  :test 'equal)

;; Variables/Constants

(alexandria:define-constant +spin-speed+ 5 :test #'=)

(defvar *slices* 40)    ; number of hori/verti slices on spheres

(defvar *model-type* 'solid)

(defvar *curr-mol*)

(defvar *view-rotx* 20)

(defvar *view-roty* 30)

(defvar *view-rotz* 0)

(defvar *show-light-source* nil)

(defvar *light-theta* 0)

(defvar *light-phi* 0)

(defvar *light-dist* 5)

;; 5 degrees in radians
(alexandria:define-constant +light-spin-speed+ .0872664625 :test #'=)

(defvar *light-r* .8)

(defvar *light-g* .8)

(defvar *light-b* .8)

(a:define-constant +light-colour-vel+ .1 :test #'=)

(defvar *walk-mode* nil)

(defstruct animation
  (thread)
  (lock (u:make-lock "Animation"))
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

(defparameter *animation* nil)

;; Our molecule viewer class

(defun draw-light-source ()
  (gl:with-pushed-matrix
    (gl:material :front :ambient (vector *light-r* *light-g* *light-b* 1))
    (gl:translate (* *light-dist* (cos *light-theta*) (sin *light-phi*))
                  (* *light-dist* (sin *light-theta*) (sin *light-phi*))
                  (* *light-dist* (cos *light-phi*)))
    (glut:solid-sphere 0.1 10 10)))

(defun draw-atom (element x y z)
  (gl:with-pushed-matrix
    (gl:rotate *view-rotx* 1 0 0)
    (gl:rotate *view-roty* 0 1 0)
    (gl:rotate *view-rotz* 0 0 1)
    (gl:material :front :ambient-and-diffuse
      (case element
        ((H) #(0.8 0.8 0.8 1))
        ((O) #(0.8 0.1 0.0 1))
        ((C) #(0.2 0.2 0.2 1))))
    (gl:translate x y z)
    (funcall
     (if (eq *model-type* 'wire)
         #'glut:wire-sphere
         #'glut:solid-sphere)
      (case element
        ((H) 0.7)
        ((O) 1.0)
        ((C) 1.2))
      *slices*
      *slices*)))

(defmethod display ()
  (gl:enable :cull-face :lighting :light0 :depth-test)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:light :light0 :position (vector (* *light-dist* (cos *light-theta*) (sin *light-phi*))
                                      (* *light-dist* (sin *light-theta*) (sin *light-phi*))
                                      (* *light-dist* (cos *light-phi*))
                                      0))
  (gl:light :light0 :diffuse (vector *light-r* *light-g* *light-b* 1))
  (when *show-light-source*
    (draw-light-source))
  (dolist (a *curr-mol*)
    (apply #'draw-atom a)))

(defmacro lim-between (sym bot top)
  `(setf ,sym (a:clamp ,sym ,bot ,top)))

(defvar *origclick* nil)

(defvar *origrot* nil)

(defun random-interval (bot top)
  (+ (* (- top bot)
        (/ (random 100000)
           100000.0))
     bot))

(defvar *view-rotx-vel* 0)

(defvar *view-roty-vel* 0)

(defvar *view-rotz-vel* 0)

(defvar last-update 0)

(defvar counter 0)

(defun calculate ()
  (when *walk-mode*
    (incf counter)
    (when (< (+ last-update internal-time-units-per-second)
             (get-internal-real-time))
      (format t "~a frames per second with ~a slices.~%" counter *slices*)
      (setq counter 0)
      (setq last-update (get-internal-real-time)))
    (incf *view-rotx-vel* (random-interval -.1 .1))
    (incf *view-roty-vel* (random-interval -.1 .1))
    (incf *view-rotz-vel* (random-interval -.1 .1))
    (lim-between *view-rotx-vel* -2 2)
    (lim-between *view-roty-vel* -2 2)
    (lim-between *view-rotz-vel* -2 2)
    (incf *view-rotx* *view-rotx-vel*)
    (incf *view-roty* *view-roty-vel*)
    (incf *view-rotz* *view-rotz-vel*)
    (incf *light-r* (random-interval -.02 .02))
    (incf *light-g* (random-interval -.02 .02))
    (incf *light-b* (random-interval -.02 .02))
    (lim-between *light-r* 0 1)
    (lim-between *light-g* 0 1)
    (lim-between *light-b* 0 1)))

(defun initialize-rendering-function (wish-stream)
  (declare (ignore wish-stream))
  (lambda (context)
    (declare (ignore context))))

(defparameter *3d-context* nil)

(defun stop-3d-animation ()
  (when (and *animation*
             (u:threadp (animation-thread *animation*)))
    (stop-drawing-thread *animation*)
    (wait-thread *animation*)))

(a:define-constant +sdl-frame-width+ 500 :test #'=)

(a:define-constant +sdl-frame-height+ 500 :test #'=)

(defun reshape (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((h (/ height width)))
    (gl:frustum -1 1 (- h) h 9 50))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0 0 -40))

(defun draw-molecule-thread ()
  (ctx:push-for-updating *3d-context*
                           (lambda (dt)
                             (declare (ignore dt))
     (reshape +sdl-frame-width+ +sdl-frame-height+)))
  (loop while (not (stop-drawing-thread-p *animation*)) do
    (ctx:sync *3d-context*)
    (ctx:push-for-updating *3d-context*
                           (lambda (dt)
                             (declare (ignore dt))
                             (calculate))
                           :force-push nil)
    (ctx:push-for-rendering *3d-context*
                            (lambda (dt)
                              (declare (ignore dt))
                              (display))
                            :force-push nil))
  (format t "STOP terrain rendering!~%"))

(defun quit ()
  (stop-3d-animation)
  (ctx:quit-sdl *3d-context*)
  (exit-nodgui))

(defmacro set-parameter (var amount)
  `(ctx:in-renderer-thread (*3d-context* dt)
    (declare (ignore dt))
    (setf ,var ,amount)))

(defun toggle-wireframe ()
  (ctx:in-renderer-thread (*3d-context* dt)
    (declare (ignore dt))
    (setf *model-type*
          (if (eq *model-type* 'wire)
              'solid
              'wire))))

(defun toggle-walk-mode ()
  (ctx:in-renderer-thread (*3d-context* dt)
    (declare (ignore dt))
    (setf *walk-mode* (not *walk-mode*))))

(defun toggle-show-light-source ()
  (ctx:in-renderer-thread (*3d-context* dt)
    (declare (ignore dt))
    (setf *show-light-source* (not *show-light-source*))))

(defmethod keyboard-bind (sdl-frame)
  (bind sdl-frame
        #$<KeyPress-x>$
        (lambda (event)
          (declare (ignore event))
          (incf *view-rotx* +spin-speed+)))
  (bind sdl-frame
        #$<KeyPress-X>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *view-rotx* +spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-y>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *view-roty* +spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-Y>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *view-roty* +spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-z>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *view-rotz* +spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-Z>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *view-rotz* +spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-t>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *light-theta* +light-spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-T>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *light-theta* +light-spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-p>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *light-phi* +light-spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-P>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *light-phi* +light-spin-speed+))))
  (bind sdl-frame
        #$<KeyPress-r>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *light-r* +light-colour-vel+)
            (lim-between *light-r* 0 1))))
  (bind sdl-frame
        #$<KeyPress-R>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *light-r* +light-colour-vel+)
            (lim-between *light-r* 0 1))))
  (bind sdl-frame
        #$<KeyPress-g>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *light-g* +light-colour-vel+)
            (lim-between *light-g* 0 1))))
  (bind sdl-frame
        #$<KeyPress-G>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *light-g* +light-colour-vel+)
            (lim-between *light-g* 0 1))))
  (bind sdl-frame
        #$<KeyPress-b>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *light-b* +light-colour-vel+)
            (lim-between *light-b* 0 1))))
  (bind sdl-frame
        #$<KeyPress-B>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *light-b* +light-colour-vel+)
            (lim-between *light-b* 0 1))))
  (bind sdl-frame
        #$<KeyPress-w>$
        (lambda (event)
          (declare (ignore event))
          (toggle-walk-mode)))
  (bind sdl-frame
        #$<KeyPress-M>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (toggle-wireframe))))
  (bind sdl-frame
        #$<KeyPress-s>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (incf *slices*)
            (lim-between *slices* 1 100))))
  (bind sdl-frame
        #$<KeyPress-S>$
        (lambda (event)
          (declare (ignore event))
          (ctx:in-renderer-thread (*3d-context* dt)
            (declare (ignore dt))
            (decf *slices*)
            (lim-between *slices* 1 100))))
  (bind sdl-frame
        #$<KeyPress-l>$
        (lambda (event)
          (declare (ignore event))
          (toggle-show-light-source)))
  (bind sdl-frame
        #$<KeyPress-q>$
        (lambda (event)
          (declare (ignore event))
          (quit))))

(defun demo-molecule ()
  (with-nodgui ()
    (let* ((sdl-frame      (ctx:make-sdl-frame +sdl-frame-width+ +sdl-frame-height+))
           (tool-frame     (make-instance 'frame))
           (model-frame    (make-instance 'labelframe
                                          :master tool-frame
                                          :text "Model"))
           (rotation-frame (make-instance 'labelframe
                                          :master tool-frame
                                          :text "Rotation"))
           (light-frame    (make-instance 'labelframe
                                          :master tool-frame
                                          :text "light"))
           (x-rot-label    (make-instance 'label
                                          :master rotation-frame
                                          :text "X:"))
           (x-rot-slider   (make-instance 'scale
                                          :master rotation-frame
                                          :from 0
                                          :to 360
                                          :length 150
                                          :command (lambda (value)
                                                     (set-parameter *view-rotx* value))))
           (y-rot-label    (make-instance 'label
                                          :master rotation-frame
                                          :text "Y:"))
           (y-rot-slider   (make-instance 'scale
                                          :master rotation-frame
                                          :from 0
                                          :to 360
                                          :length 150
                                          :command (lambda (value)
                                                     (set-parameter *view-roty* value))))
           (z-rot-label    (make-instance 'label
                                          :master rotation-frame
                                          :text "z:"))
           (z-rot-slider   (make-instance 'scale
                                          :master rotation-frame
                                          :from 0
                                          :to 360
                                          :length 150
                                          :command (lambda (value)
                                                     (set-parameter *view-rotz* value))))
           (wireframe-check (make-instance 'check-button
                                           :master model-frame
                                           :text "Wireframe"
                                           :command (lambda (x)
                                                      (declare (ignore x))
                                                      (toggle-wireframe))))
           (walk-check     (make-instance 'check-button
                                           :master model-frame
                                           :text "Random rotation animation"
                                           :command (lambda (x)
                                                      (declare (ignore x))
                                                      (toggle-walk-mode))))
           (molecule-combo (make-instance 'combobox
                                          :master model-frame
                                          :text "molecule"
                                          :values '(H20 C2H6O2)))
           (slices-label    (make-instance 'label
                                          :master model-frame
                                          :text "slices:"))
           (slices-slider   (make-instance 'scale
                                          :master model-frame
                                          :from 1
                                          :to   100
                                          :length 150
                                          :command (lambda (value)
                                                     (ctx:in-renderer-thread (*3d-context* dt)
                                                       (declare (ignore dt))
                                                       (set-parameter *slices* value)))))
           (show-light-check (make-instance 'check-button
                                            :master light-frame
                                            :text "Show light source"
                                            :command (lambda (x)
                                                      (declare (ignore x))
                                                       (toggle-show-light-source))))
           (r-label    (make-instance 'label
                                      :master light-frame
                                      :text "R:"))
           (r-slider   (make-instance 'scale
                                      :master light-frame
                                      :from 0
                                      :to   255
                                      :length 150
                                      :command (lambda (value)
                                                 (ctx:in-renderer-thread (*3d-context* dt)
                                                   (declare (ignore dt))
                                                   (set-parameter *light-r* value)))))
           (g-label    (make-instance 'label
                                      :master light-frame
                                      :text "G:"))
           (g-slider   (make-instance 'scale
                                      :master light-frame
                                      :from 0
                                      :to   255
                                      :length 150
                                      :command (lambda (value)
                                                 (ctx:in-renderer-thread (*3d-context* dt)
                                                   (declare (ignore dt))
                                                   (set-parameter *light-g* value)))))
           (b-label    (make-instance 'label
                                      :master light-frame
                                      :text "B:"))
           (b-slider   (make-instance 'scale
                                      :master light-frame
                                      :from 0
                                      :to   255
                                      :length 150
                                      :command (lambda (value)
                                                 (ctx:in-renderer-thread (*3d-context* dt)
                                                   (declare (ignore dt))
                                                   (set-parameter *light-b* value)))))
           (phi-label    (make-instance 'label
                                      :master light-frame
                                      :text "φ:"))
           (phi-slider   (make-instance 'scale
                                      :master light-frame
                                      :from 0
                                      :to   6.28
                                      :length 150
                                      :command (lambda (value)
                                                 (ctx:in-renderer-thread (*3d-context* dt)
                                                   (declare (ignore dt))
                                                   (set-parameter *light-phi* value)))))
           (theta-label    (make-instance 'label
                                      :master light-frame
                                      :text "θ:"))
           (theta-slider   (make-instance 'scale
                                      :master light-frame
                                      :from 0
                                      :to   6.28
                                      :length 150
                                      :command (lambda (value)
                                                 (ctx:in-renderer-thread (*3d-context* dt)
                                                   (declare (ignore dt))
                                                   (set-parameter *light-theta* value))))))
      (bind sdl-frame
              #$<1>$
              (lambda (event)
                (declare (ignore event))
                (force-focus sdl-frame)))
      (grid-implicit (list sdl-frame))
      (grid-implicit (list x-rot-label x-rot-slider))
      (grid-implicit (list y-rot-label y-rot-slider))
      (grid-implicit (list z-rot-label z-rot-slider))
      (grid wireframe-check 0 0 :sticky :w  :columnspan 2)
      (grid walk-check 1 0 :sticky :w  :columnspan 2)
      (grid molecule-combo 2 0 :sticky :w  :columnspan 2)
      (bind molecule-combo
            #$<<ComboboxSelected>>$
            (lambda (event)
              (declare (ignore event))
              (let ((wish-server *wish*))
                (ctx:in-renderer-thread (*3d-context* dt)
                 (declare (ignore dt))
                 (let ((*wish* wish-server))
                   (if (string-equal (text molecule-combo)
                                     "C2H6O2")
                       (setf *curr-mol* +ethanol+)
                       (setf *curr-mol* +water+)))))))
      (grid slices-label     3 0 :sticky :w)
      (grid slices-slider    3 1 :sticky :w)
      (grid theta-label      0 0 :sticky :w)
      (grid theta-slider     0 1 :sticky :w)
      (grid r-label          0 2 :sticky :w)
      (grid r-slider         0 3 :sticky :w)
      (grid phi-label        1 0 :sticky :w)
      (grid phi-slider       1 1 :sticky :w)
      (grid g-label          1 2 :sticky :w)
      (grid g-slider         1 3 :sticky :w)
      (grid b-label          2 2 :sticky :w)
      (grid b-slider         2 3 :sticky :w)
      (grid show-light-check 3 0 :sticky :w)
      (grid-implicit (list model-frame rotation-frame light-frame) :sticky :news)
      (grid tool-frame 1 0 :sticky :news)
      (grid-columnconfigure tool-frame :all :weight 1)
      (grid-rowconfigure    tool-frame :all :weight 1)
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
      (keyboard-bind sdl-frame)
      (glut:init)
      (wait-complete-redraw)
      (setf *animation*
            (make-animation :thread
                            (u:make-thread #'draw-molecule-thread
                                           :name "molecule"))))))

;; Main function

(defun main (&optional (mol +ethanol+))
  (setf *curr-mol* mol)
  (demo-molecule))

(main)
