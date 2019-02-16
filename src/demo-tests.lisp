;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui.demo)

(cl-syntax:use-syntax nodgui-event-syntax)

(cl-syntax:use-syntax nodgui-color-syntax)

(defun demo ()
  (with-nodgui (:debug-tcl nil)
    (let* ((widget          (make-instance 'button
                                           :text    "widget"
                                           :command #'(lambda () (demo-widget))))
           (eyes            (make-instance 'button
                                           :text    "eyes"
                                           :command #'(lambda () (demo-eyes))))
           (modal           (make-instance 'button
                                           :text    "modal"
                                           :command #'(lambda () (demo-modal))))
           (combo           (make-instance 'button
                                           :text    "combo"
                                           :command #'(lambda () (demo-combo))))
           (packtest1       (make-instance 'button
                                           :text    "packtest 1"
                                           :command #'(lambda () (demo-packtest1))))
           (packtest2       (make-instance 'button
                                           :text    "packtest 2"
                                           :command #'(lambda () (demo-packtest2))))
           (scrolled-frame  (make-instance 'button
                                           :text    "scrolled frame"
                                           :command #'(lambda () (demo-sct))))
           (button-text     (make-instance 'button
                                           :text    "button text escaped"
                                           :command #'(lambda () (demo-escape-text))))
           (treeview       (make-instance  'button
                                           :text    "treeview"
                                           :command #'(lambda () (demo-treeview))))
           (w/widget       (make-instance  'button
                                           :text    "with widget macro"
                                           :command #'(lambda () (demo-with-widgets))))
           (notebook-1     (make-instance  'button
                                           :text    "notebook 1"
                                           :command #'(lambda () (demo-notebook))))
           (notebook-2     (make-instance  'button
                                           :text    "notebook 2"
                                           :command #'(lambda () (demo-notebook-2))))
           (defwidget      (make-instance  'button
                                           :text    "defwidget"
                                           :command #'(lambda () (demo-defwidget))))
           (demo-canvas    (make-instance  'button
                                           :text    "canvas"
                                           :command #'(lambda () (demo-canvas))))
           (demo-image     (make-instance  'button
                                           :text    "images"
                                           :command #'(lambda () (demo-image))))
           (demo-treelist  (make-instance  'button
                                           :text    "(mw) treelist"
                                           :command #'(lambda () (nodgui.mw::treelist-test))))
           (demo-tooltip   (make-instance  'button
                                           :text    "(mw) tooltip"
                                           :command #'(lambda () (nodgui.mw::tooltip-test))))
           (demo-gtree     (make-instance  'button
                                           :text    "(mw) graphical tree"
                                           :command #'(lambda () (nodgui.mw::gtree-demo))))
           (demo-auto-listbox (make-instance 'button
                                             :text    "(mw) autocomplete listbox"
                                             :command #'(lambda ()
                                                          (nodgui.mw::autocomplete-listbox-demo))))
           (demo-search-listbox (make-instance 'button
                                             :text    "(mw) searchable listbox"
                                             :command #'(lambda ()
                                                          (nodgui.mw::searchable-listbox-demo))))
           (demo-list-select    (make-instance 'button
                                               :text    "(mw) list select demo"
                                               :command #'(lambda ()
                                                            (nodgui.mw::list-select-demo))))
           (demo-listbox-dialog (make-instance 'button
                                               :text    "(mw) listbox dialog"
                                               :command #'(lambda ()
                                                            (let ((chosen (nodgui.mw:listbox-dialog
                                                                           *tk*
                                                                           "listbox dialog"
                                                                           "Choose an entry"
                                                                           '("hello" "world"))))
                                                              (message-box (format nil
                                                                                   "chosen ~s~%"
                                                                                   chosen)
                                                                           "info"
                                                                           :ok
                                                                           "info"
                                                                           :parent *tk*)))))
           (demo-date-picker   (make-instance 'button
                                              :text    "(mw) date picker"
                                              :command #'(lambda ()
                                                           (nodgui.mw::date-picker-demo))))
           (demo-password      (make-instance 'button
                                              :text    "(mw) password entry"
                                              :command #'(lambda ()
                                                           (nodgui.mw::password-entry-demo))))
           (demo-star-progress (make-instance 'button
                                            :text    "(mw) star progress bar"
                                            :command #'(lambda ()
                                                         (nodgui.mw::star-progress-demo))))
           (b-quit             (make-instance  'button
                                               :text    "quit lisp :)"
                                               :command #'(lambda () (uiop:quit)))))
      (grid widget              0 0 :sticky :nswe)
      (grid eyes                0 1 :sticky :nswe)
      (grid modal               0 2 :sticky :nswe)
      (grid combo               1 0 :sticky :nswe)
      (grid packtest1           1 1 :sticky :nswe)
      (grid packtest2           1 2 :sticky :nswe)
      (grid scrolled-frame      2 0 :sticky :nswe)
      (grid button-text         2 1 :sticky :nswe)
      (grid treeview            2 2 :sticky :nswe)
      (grid w/widget            3 0 :sticky :nswe)
      (grid notebook-1          3 1 :sticky :nswe)
      (grid notebook-2          3 2 :sticky :nswe)
      (grid defwidget           4 0 :sticky :nswe)
      (grid demo-canvas         4 1 :sticky :nswe)
      (grid demo-image          4 2 :sticky :nswe)
      (grid demo-treelist       5 0 :sticky :nswe)
      (grid demo-tooltip        5 1 :sticky :nswe)
      (grid demo-gtree          5 2 :sticky :nswe)
      (grid demo-auto-listbox   6 0 :sticky :nswe)
      (grid demo-search-listbox 6 1 :sticky :nswe)
      (grid demo-list-select    6 2 :sticky :nswe)
      (grid demo-listbox-dialog 7 0 :sticky :nswe)
      (grid demo-date-picker    7 1 :sticky :nswe)
      (grid demo-password       7 2 :sticky :nswe)
      (grid demo-star-progress  8 0 :sticky :nswe)
      (grid b-quit              9 0 :sticky :nswe :columnspan 3)
      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1))))

(defvar *do-rotate* nil)

(defvar *demo-line* nil)

(defvar *demo-canvas* nil)

(defun eggs (radio)
  (format t "Prepare ~a eggs.~%"
          (case (value radio)
            (1 "fried")
            (2 "stirred")
            (3 "cooked")))
  (finish-output))

(defun demo-widget()
  (with-nodgui (:debug-tcl nil)
    (flet ((write-postscript (canvas)
             #'(lambda ()
                 (let ((file (get-save-file :file-types '(("Encapsulated Postscript" "*.ps")))))
                   (with-open-file (stream file
                                           :if-exists :supersede
                                           :direction :output)
                     (format stream "~a~%" (postscript canvas)))))))
      (let* ((bar (make-instance 'frame))
             (fradio (make-instance 'frame :master bar))
             (leggs (make-instance 'label :master fradio :text "Eggs:"))
             (r1 (make-instance 'radio-button
                                :master   fradio
                                :text     "fried"
                                :value    1
                                :variable "eggs"))
             (r2 (make-instance 'radio-button
                                :master   fradio
                                :text     "stirred"
                                :value    2
                                :variable "eggs"))
             (r3 (make-instance 'radio-button
                                :master   fradio
                                :text     "cooked"
                                :value    3
                                :variable "eggs"))
             (combo (make-instance 'combobox
                                   :master fradio
                                   :text   "foo"
                                   :values '(foo bar baz)))
             (fprogress (make-instance 'frame :master bar))
             (lprogress (make-instance 'label :master fprogress :text "Progress:"))
             (progress (make-instance 'progressbar
                                      :master fprogress
                                      :value    0
                                      :length 150))
             (bprogress (make-instance 'button :text "Step"
                                       :command (lambda ()
                                                  (incf (value progress) 10)
                                                  (when (> (value progress) 100)
                                                    (setf (value progress) 0)))
                                       :master fprogress))
             (fscale (make-instance 'frame :master bar))
             (scale (make-instance 'scale :master fscale :from 0 :to 100 :length 150))
             (separator (make-instance 'separator :master fscale))
             (fcheck (make-instance 'frame :master bar))
             (lcheck (make-instance 'label :master fcheck :text "Add:"))
             (ch1 (make-instance 'check-button :master fcheck :text "Salt"))
             (ch2 (make-instance 'check-button :master fcheck :text "Pepper"))
             (fstyles (make-instance 'frame :master bar))
             (lstyles (make-instance 'label :master fstyles :text "Tk styles:"))
             (fr (make-instance 'frame :master bar))
             (lr (make-instance 'label :master fr :text "Rotation:"))
             (bstart (make-instance 'button
                                    :master  fr
                                    :text    "Start"
                                    :command 'start-rotation))
             (bstop  (make-instance 'button
                                    :master  fr
                                    :text    "Stop"
                                    :command 'stop-rotation))
             (b1 (make-instance 'button
                                :master  bar
                                :text    "Hallo"
                                :command (lambda ()
                                           (format t "Hallo~%")
                                           (finish-output))))
             (b2 (make-instance 'button
                                :master  bar
                                :text    "Welt!"
                                :command (lambda ()
                                           (format t "Welt~%")
                                           (finish-output))))
             (f (make-instance 'frame :master bar))
             (l (make-instance 'label :master f :text "Test:"))
             (b3 (make-instance 'button :master f :text  "Ok." :command 'test-rotation))
             (e (make-instance 'entry :master bar))
             (b4 (make-instance 'button
                                :master  bar
                                :text    "get!"
                                :command (lambda ()
                                           (format t "content of entry:~A~%" (text e))
                                           (finish-output))))
             (b5 (make-instance 'button
                                :master  bar
                                :text    "set!"
                                :command (lambda ()
                                           (setf (text e) "test of set"))))
             (sc (make-instance 'scrolled-canvas :borderwidth 2 :relief :raised))
             (c  (canvas sc))
             (spinbox-label (make-instance 'label :text "A spinbox ->"))
             (spinbox (make-instance 'spinbox :from 0 :to 10 :increment 1))
             (lines nil)
             (mb (make-menubar))
             (mfile (make-menu mb "File" ))
             (mf-load (make-menubutton mfile "Load" (lambda () ;(error "asdf")
                                                      (format t "Load pressed~&")
                                                      (finish-output))
                                       :underline 1))
             (mf-save (make-menubutton mfile "Save"
                                       (lambda ()
                                         (format t "Save pressed~&")
                                         (finish-output))
                                       :underline 1))
             (sep1 (add-separator mfile))
             (mf-export (make-menu mfile "Export..."))
             (sep2 (add-separator mfile))
             (mf-print (make-menubutton mfile "Print" (write-postscript c)))
             (sep3 (add-separator mfile))
             (mfe-jpg (make-menubutton mf-export "jpeg" (lambda ()
                                                          (format t "Jpeg pressed~&")
                                                          (finish-output))))
             (mfe-gif (make-menubutton mf-export "png" (lambda ()
                                                         (format t "Png pressed~&")
                                                         (finish-output))))
             (mf-scale (make-menu mfile "Scale..."))
             (mfs-1 (make-menubutton mf-scale "0.5" (lambda ()
                                                      (scale c 0.5))))
             (mfs-2 (make-menubutton mf-scale "2" (lambda ()
                                                    (scale c 2))))
             (mfs-3 (make-menubutton mf-scale "2/0.5" (lambda ()
                                                        (scale c 2 0.5))))
             (mfs-4 (make-menubutton mf-scale "0.5/2" (lambda ()
                                                        (scale c 0.5 2))))
             (sep4 (add-separator mfile))
             (mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))
                                       :underline 1
                                       :accelerator "Alt Q"))
             (mp (make-menu nil "Popup"))
             (mp-1 (make-menubutton mp "Option 1" (lambda ()
                                                    (format t "Popup 1~&")
                                                    (finish-output))))
             (mp-2 (make-menubutton mp "Option 2" (lambda ()
                                                    (format t "Popup 2~&")
                                                    (finish-output))))
             (mp-3 (make-menubutton mp "Option 3" (lambda ()
                                                    (format t "Popup 3~&")
                                                    (finish-output)))))
        (declare (ignore mf-print mf-exit mfe-gif mfe-jpg mf-save mf-load sep1 sep2 sep3 sep4
                         mp-1 mp-2 mp-3 mfs-1 mfs-2 mfs-3 mfs-4))
        (setf (value progress) 10)
        (configure scale :orient :horizontal)
        (bind *tk* #$<Alt-q>$ (lambda (event)
                                (declare (ignore event))
                                (setf *exit-mainloop* t)))
        (bind c #$<1>$ (lambda (event)
                         (popup mp (event-root-x event) (event-root-y event))))
        (configure c :borderwidth 2 :relief :sunken)
        (pack sc :side :top :fill :both :expand t)
        (pack bar :side :bottom)
        (pack fradio :side :top :fill :x)
        (pack (list leggs r1 r2 r3) :side :left)
        (pack fprogress :side :top :fill :x)
        (pack lprogress :side :left)
        (pack progress :side :left :fill :x :padx 10)
        (pack bprogress :side :left)
        (pack fscale :side :top :fill :x)
        (pack scale :side :left :fill :x :padx 20)
        (setf (command scale) (lambda (a) (format t "scale changed ~a~%" a)))
        (pack separator :side :left)
        (configure separator :orient :vertical)
        (pack fcheck :side :top :fill :x)
        (pack (list lcheck ch1 ch2) :side :left)
        (setf (value r1) 1)
        (pack combo :side :left)
        (dolist (r (list r1 r2 r3))
          (let ((button r))
            (setf (command r) (lambda (val)
                                (declare (ignore val))
                                (eggs button)))))
        (pack fstyles :side :top :fill :x)
        (pack lstyles :side :left)
        (let ((radio nil))
          (dolist (theme (theme-names))
            (let* ((theme theme)
                   (r (make-instance 'radio-button :master fstyles
                                    :text theme
                                    :value theme
                                    :variable "radiostyles"
                                    :command (lambda (val)
                                               (declare (ignore val))
                                               (use-theme theme)))))
              (pack r :side :left)
              (setf radio r)))
          (setf (value radio) "default"))
        (scrollregion c 0 0 500 400)
        (pack fr :side :left)
        (pack lr :side :left)
        (configure fr :borderwidth 2 :relief :sunken)
        (pack bstart :side :left)
        (pack bstop :side :left)
        (pack b1 :side :left)
        (pack b2 :side :left)
        (configure f :borderwidth 2 :relief :sunken)
        (pack f :fill :x :side :left)
        (pack l :side :left)
        (pack b3 :side :left)
        (pack e :side :left)
        (pack b4 :side :left)
        (pack b5 :side :left)
        (pack spinbox-label :side :left)
        (pack spinbox :side :left)
        (dotimes (i 100)
          (let ((w (* i 2.8001f0)))
            (let ((x (+ 250 (* 150.0f0 (sin w))))
                  (y (+ 200 (* 150.0f0 (cos w)))))
              (push y lines)
              (push x lines))))
        (setf *demo-line* (create-line c lines))
        (setf *demo-canvas* c)
        (create-text c 10 10 "Nodgui Demonstration")))))

(defvar *angle* 0.0f0)

(defvar *angle2* 0.0f0)

(defvar *angle3* 0.0f0)

(declaim (single-float *angle* *angle2* *angle3*))

(defun rotate()
;  (declare (optimize speed)    (single-float *angle* *angle2* *angle3*))
  (let ((*debug-tk* nil))
    (let ((lines nil)
          (dx (* 50 (sin *angle2*)))
          (dy (* 50 (cos *angle2*)))
          (wx (sin *angle3*)))
;         (wy (cos *angle3*))
      (incf *angle* 0.1f0)
      (incf *angle2* 0.03f0)
      (incf *angle3* 0.01f0)
      (dotimes (i 100)
        (declare (fixnum i))
        (let ((w (+ *angle* (* i 2.8001f0))))
          (let ((x (+ dx 250 (* 150 (sin w) wx)))
                (y (+ dy 200 (* 150 (cos w)))))
            (push y lines)
            (push x lines)
            )))
      (set-coords *demo-canvas* *demo-line* lines))
    (if *do-rotate*
        (after 25 #'rotate))))

(defun test-rotation()
  (setf *debug-tk* nil)
  (time (dotimes (i 1000)
          (rotate)
          (flush-wish)))
  (finish-output))

(defun start-rotation()
  (setf *do-rotate* t)
  (rotate))

(defun stop-rotation()
  (setf *do-rotate* nil))

;;;; the eyes :)

(defun demo-eyes ()
  (with-nodgui ()
   (let* ((*debug-tk* nil)
          (w (screen-width))
          (h (screen-height))
          (c (make-instance 'canvas :width 400 :height 300))
          (e1 (create-oval c 10 10 190 290))
          (e2 (create-oval c 210 10 390 290))
          (p1 (create-oval c 10 10 40 40))
          (p2 (create-oval c 10 10 40 40))
          (old-x 0)
          (old-y 0))
     (setf *debug-tk* nil)
     (labels ((update ()
                      (multiple-value-bind (pos-x pos-y) (screen-mouse)
                        (let* ((wx (window-x *tk*))
                               (wy (window-y *tk*))
                               (width (window-width *tk*))
                               (height (window-height *tk*))
                               (mx pos-x)
                               (my pos-y)
                               (x (truncate (* width (/ mx w))))
                               (y (truncate (* height (/ my h))))
                               (diam (truncate width 8))
                               (dx1 (- mx (+ wx (truncate width 4))))
                               (dy1 (- my (+ wy (truncate height 2))))
                               (dx2 (- mx (+ wx (* 3 (truncate width 4)))))
                               (dy2 (- my (+ wy (truncate height 2))))
                               (p1x (+ (- (truncate width 4)
                                          (truncate diam 2))
                                       (truncate (* width  dx1) (* 4.5 w))))
                               (p1y (+ (- (truncate height 2)
                                          (truncate diam 2))
                                       (truncate (* height dy1) (* 2.3 h))))
                               (p2x (+ (- (* 3 (truncate width 4))
                                          (truncate diam 2))
                                       (truncate (*  width  dx2) (* 4.5 w))))
                               (p2y (+ (- (truncate height 2)
                                          (truncate diam 2))
                                       (truncate (* height dy2) (* 2.3 h)))))
                          (setf *debug-tk* nil)
                          (unless (and (= x old-x)
                                       (= y old-y))
                            (set-coords c e1 (list 10 10
                                                   (- (truncate width 2) 10)
                                                   (- height 10)))
                            (set-coords c e2 (list (+ (truncate width 2) 10) 10
                                                   (- width 10) (- height 10)))
                            (set-coords c p1 (list p1x p1y (+ diam p1x) (+ diam p1y)))
                            (set-coords c p2 (list p2x p2y (+ diam p2x) (+ diam p2y)))
                            (setf old-x x
                                  old-y y))))
                        (after 100 #'update)))
     (pack c :expand 1 :fill :both)
     (itemconfigure c e1 "width" 10)
     (itemconfigure c e2 "width" 10)
     (itemconfigure c p1 "fill" "blue")
     (itemconfigure c p2 "fill" "blue")
     (after 100 #'update)))))

(defun demo-modal ()
  (with-nodgui ()
   (let* ((b (make-instance 'button :text "Input"
                            :command (lambda ()
                                       (let ((erg (input-box "Enter a string:"
                                                             :title "String input")))
                                         (if erg
                                             (format t "input was: ~a~%" erg)
                                           (format t "input was cancelled~%"))
                                       (finish-output))))))
     (pack b))))

 (defun demo-combo ()
  (with-nodgui ()
    (let* ((c (make-instance 'combobox
                             :text "foo"
                             :values '("bar" "baz" "foo bar")))
           (add (make-instance 'button
                               :text    "Add values"
                               :command (lambda ()
                                          (setf (options c) (list 1 2 "asdf xx" "bb" "cc")))))
           (ok (make-instance 'button
                              :text "Ok"
                              :command (lambda ()
                                         (format t "text: ~a~%" (text c))
                                         (exit-wish)))))
      (bind c #$<KeyRelease>$ (lambda (event)
                               (declare (ignore event))
                               (format t "newsel:~a~%" (text c))
                               (finish-output)))
      (bind c #$<<ComboboxSelected>>$ (lambda (event)
                                       (declare (ignore event))
                                       (format t "newsel:~a~%" (text c))
                                       (finish-output)))
      (pack add :side :right)
      (pack ok :side :right)
      (pack c :side :left))))

(defun demo-packtest1 ()
  (with-nodgui ()
    (dotimes (i 10)
      (let ((s ""))
        (dotimes (j i)
          (setf s (format nil "~a " s)))
      (pack (make-instance 'button :text (format nil "Button~a Nr. ~a" s i)))
      (sleep 0.1)))))

(defun demo-packtest2 ()
  (with-nodgui (:debug-tcl t)
    (with-atomic
        (dotimes (i 10)
          (pack (make-instance 'button :text (format nil "Button Nr. ~a" i)))
          ;(sleep 0.1)
          ))))

(defun demo-sct()
  (with-nodgui (:debug-tcl t)
    (let* ((sf (make-instance 'scrolled-frame))
           (f (interior sf))
           (n 1)
           (b1 (make-instance 'button
                              :master  f
                              :text    "Button 1"
                              :command
                              (lambda ()
                                (incf n)
                                (pack (make-instance 'button
                                                     :master f
                                                     :text (format nil "Button ~a" n))
                                      :side :left)))))
      (pack sf :side :top :fill :both :expand t)
      (pack b1 :side :left))))

(defun demo-escape-text ()
  (with-nodgui ()
    (let ((b (make-instance 'button :text " a button [a] :)")))
      (pack b)
      (setf (text b) " )} [eee] ")
      (flush-wish))))

;;; treeview tests

(defwidget treeviewtest (frame)
  ()
  ((tree treeview
         :pack (:side :top :expand t :fill :both)
         :columns (list "a" "b")))
  (let* ((parent (make-instance 'tree-item
                                :text   "Hallo"
                                :id     "[a]"
                                :parent +treeview-root+
                                :index  +treeview-last-index+))
         (child  (make-instance 'tree-item
                                :text   "W[e]lt"
                                :parent (id parent)
                                :column-values '("} [hello]" "[world]")
                                :index  +treeview-last-index+)))
    (treeview-insert-item tree :item parent)
    (treeview-insert-item tree :item child)))

(defun demo-treeview ()
  (with-nodgui ()
    (pack (make-instance 'treeviewtest) :fill :both :expand t)))

(defun demo-with-widgets ()
  (with-nodgui ()
    (with-widgets
        (toplevel top-frame :title "with-widgets-test"
                  (label lb1 :text "Test, Test!" :pack '(:side :top))
                  (entry en1 :pack '(:side :top) :text "")
                  (frame fr1 :pack '(:side :bottom)
                         (button bt1 :text "OK" :pack '(:side :right)
                                 :command (lambda () (format t "Pressed OK~%")))
                         (button bt2 :text "CANCEL" :pack '(:side :left)
                                 :command (lambda () (withdraw top-frame)))))
      (setf (text lb1) "Test, Test, Test!"))))

;; notebook
(defun demo-notebook ()
  (with-nodgui ()
    (let* ((nb (make-instance 'notebook))
           (f1 (make-instance 'frame :master nb))
           (f2 (make-instance 'frame :master nb))
           (t1 (make-instance 'text :master f1 :width 40 :height 10))
           (b1 (make-instance 'button :master f1 :text "Press me"
                              :command (lambda ()
                                         (format t "the index is:~a~%" (notebook-index nb f1))
                                         (finish-output))))
           (b2 (make-instance 'button :master f2 :text "A button"
                              :command (lambda ()
                                         (format t "the index is:~a~%" (notebook-index nb f2))
                                         (finish-output)))))
      (pack nb :fill :both :expand t)
      (pack t1 :fill :both :expand t)
      (pack b1 :side :top)
      (pack b2 :side :top)
      (notebook-add nb f1 :text "Frame 1")
      (notebook-add nb f2 :text "Frame 2")
      (notebook-tab nb f2 :text "Frame 2 (changed after adding)")
      (notebook-enable-traversal nb)
      (append-text t1 "Foo [Bar] {Baz}"))))

(defwidget nbw (frame)
  ()
  ((nb notebook :pack (:fill :both :expand t)
       (f1 frame
           (t1 text :width 60 :height 20 :pack (:side :top))
           (b1 button :text "Press Me" :pack (:side :top)
               :command (lambda ()
                          (format t "the index is:~a~%" (notebook-index nb f1))
                          (finish-output))))
       (f2 frame
           (b2 button :text "A button" :pack (:side :top)
               :command (lambda ()
                          (format t "the index is:~a~%" (notebook-index nb f2))
                          (finish-output))))))
  (notebook-add nb f1 :text "Frame 1")
  (notebook-add nb f2 :text "Frame 2")
  (append-text t1 "Foo [Bar] [[[Baz]]]"))

(defun demo-notebook-2 ()
  (with-nodgui ()
    (let ((w (make-instance 'nbw)))
      (pack w :side :top :fill :both :expand t))))

(defwidget test-widget (frame)
  (a b c)
  ((bu button :text "A button"
       :pack (:side :top :anchor :w)
       :command (lambda ()
                  (format t "the content of enry is:~a~%" (text entry)) (finish-output)
                  (setf (text entry) "")))
   (f1 frame :pack (:side :top :fill :both :expand t)
       (lb label :text "A label" :pack (:side :left))
       (entry entry :pack (:side :left :fill :x :expand t) :text "")))
  ;; other code here
  )

(defgeneric firstline (widget))

(defgeneric (setf firstline) (val widget))

(defgeneric secondline (widget))

(defgeneric (setf secondline) (val widget))

(defgeneric entry-typed (widget keycode))

(defwidget (this test-widget2) (frame)
  ()
  ((mw test-widget :pack (:side :top :fill :x))
   (mw2 test-widget :pack (:side :top :fill :x))
   (b button :text "set" :command (lambda ()
                                    (setf (text (entry mw)) "foo")
                                    (setf (text (entry mw2)) "bar"))
      :pack (:side :top))
   (e entry :pack (:side :top) :text ""
      :on-type entry-typed))

   (:accessor firstline (text (entry (mw this))))
   (:accessor secondline (text (entry (mw2 this))))
   (bind this #$<Enter>$ (lambda (event)
                          (declare (ignore event))
                          (format t "Entered!~%") (finish-output))))

(defmethod entry-typed ((self test-widget2) keycode)
  (format t "typed:~a~%text:~a~%" keycode (text (e self))) (finish-output))

(defun demo-defwidget ()
  (declare (optimize (debug 3)))
  (with-nodgui ()
    (let ((mw (make-instance 'test-widget2)))
      (format t "mw is: ~a~%" mw) (finish-output)
      ;(error "test: ~a~%" mw)
      (pack mw :side :top :fill :both :expand t)
      (pack (make-instance 'button :text "get first"
                           :command (lambda ()
                                      (format t "the first is: ~a~%" (firstline mw))
                                      (finish-output))) :side :top :fill :both :expand t))))

(defun demo-canvas ()
  (flet ((bind-red (e)
           (declare (ignore e))
           (do-msg "You clicked on red!"))
         (bind-green (canvas item)
           (lambda (e)
             (declare (ignore e))
             (item-delete canvas item)
             (do-msg "You clicked on green and the blue arc disappeared!"))))
    (with-nodgui ()
      (let* ((size         500)
             (canvas       (make-canvas nil :width size :height size))
             (arc1         (create-arc canvas
                                       (/ size 4) (/ size 4)
                                       (* size 3/4) (* size 3/4) :start 0 :extent 180))
             (arc2         (create-arc canvas
                                       (/ size 4) (/ size 4)
                                       (* size 3/4) (* size 3/4) :start 180 :extent 180))
             (ball         (create-arc canvas
                                       0 0
                                       (* size 1/10) (* size 1/10)
                                       :start 0
                                       :extent 359
                                       :style :pieslice
                                       :fill  "#0000ff"))
             (star         (make-star canvas 80 0.5 #%red% #%black% 5))
             (bicolor-star (make-two-color-star canvas 80 0.5
                                                #%yellow% #%yellow%
                                                #%gray%   #%yellow%
                                                10
                                                :outline-width 10))
             (rotated-text (create-text canvas 0 20
                                        (strcat (format nil "Text can be rotated~%at an ")
                                                "arbitrary angle like this!")
                                        :angle -80)))
        (let ((aabb-rotated-text (canvas-item-bbox canvas rotated-text)))
          (item-move    canvas rotated-text (- (bbox-min-x aabb-rotated-text)) 0))
        (shape-move-to  bicolor-star (/ size 2) (/ size 2))
        (shape-move-to  star 30 30)
        (create-text    canvas 0 0  "Slices of the pie are clickable")
        (item-configure canvas arc1 "fill"  #%red%)    ;; using x11-colors via cl-colors2
        (item-configure canvas arc2 "fill"  "#00ff00") ;; strings are accepted, though
        (item-configure canvas arc1 "tag"   "red")
        (item-configure canvas arc2 "tag"   "green")
        (tag-bind       canvas      "red"   #$<ButtonPress-1>$ #'bind-red)
        (tag-bind       canvas      "green" #$<ButtonPress-1>$ (bind-green canvas ball))
        (item-move-to   canvas ball (* size 9/10) (* size 9/10))
        (item-raise     canvas ball arc1)
        (make-items canvas `((:rectangle 0  ,(/ size 2) 50 ,(+ (/ size 2) 50))
                             (:arc       0  ,(+ (/ size 2) 10)
                                         50 ,(+ (/ size 2) 60)
                                         :start 0 :extent 180 :fill #%purple%)
                             (:line      0 ,size ,size ,(- size 10) :width 5)))
        (pack canvas)))))

(defun demo-image ()
  (with-nodgui ()
    (let ((b (make-instance 'button :text "load image")))
      (setf (command b)
            #'(lambda ()
                (let ((file (get-open-file :file-types '(("PNG"     "*.png")
                                                         ("JPG"     "*.jpg")
                                                         ("TGA"     "*.tga")
                                                         ("RGB raw" "*.data")))))
                  (with-open-file (stream file :element-type '(unsigned-byte 8))
                    (let* ((data (read-into-array stream (file-length stream))))
                      (cond
                        ((cl-ppcre:scan "png$" file)
                         (setf (image b) (make-image data)))
                        ((cl-ppcre:scan "tga$" file)
                         (let ((bitmap (nodgui.pixmap:rotate-pixmap
                                        (nodgui.pixmap:scale-bilinear
                                         (nodgui.pixmap:slurp-pixmap 'nodgui.pixmap:tga file)
                                         2.0 2.1)
                                        30.0
                                        :repeat t)))
                           (setf (image b) (make-image bitmap))))
                        ((cl-ppcre:scan "jpg$" file)
                         (let ((bitmap (nodgui.pixmap:slurp-pixmap 'nodgui.pixmap:jpeg file)))
                           (setf (image b) (make-image bitmap))))
                        (t
                         (let ((w (input-box "Pixel width?"
                                             :title "Image size"))
                               (h (input-box "Pixel Height?"
                                              :title "Image size")))
                           (when (and w h)
                             (setf (image b) (make-image data
                                                        (parse-integer w)
                                                        (parse-integer h))))))))))))
      (place b 0 0))))
