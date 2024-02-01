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

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defun demo (&key theme)
  "Run the demo of the demos.

  :THEME a string designating a GUI theme. \"yaru\" is a modern looking theme shipped in nodgui. See also `*default-theme*'."
  (when theme
    (setf *default-theme* theme))
  (with-nodgui (:debug-tcl nil)
    (let* ((widget          (make-instance 'button
                                           :text    "widget"
                                           :command (lambda () (demo-widget))))
           (eyes            (make-instance 'button
                                           :text    "eyes"
                                           :command (lambda () (demo-eyes))))
           (modal           (make-instance 'button
                                           :text    "modal"
                                           :command (lambda () (demo-modal))))
           (combo           (make-instance 'button
                                           :text    "combo"
                                           :command (lambda () (demo-combo))))
           (packtest1       (make-instance 'button
                                           :text    "packtest 1"
                                           :command (lambda () (demo-packtest1))))
           (packtest2       (make-instance 'button
                                           :text    "packtest 2"
                                           :command (lambda () (demo-packtest2))))
           (scrolled-frame  (make-instance 'button
                                           :text    "scrolled frame"
                                           :command (lambda () (demo-sct))))
           (button-text     (make-instance 'button
                                           :text    "button text escaped"
                                           :command (lambda () (demo-escape-text))))
           (treeview       (make-instance  'button
                                           :text    "treeview"
                                           :command (lambda () (demo-treeview))))
           (w/widget       (make-instance  'button
                                           :text    "with widget macro"
                                           :command (lambda () (demo-with-widgets))))
           (notebook-1     (make-instance  'button
                                           :text    "notebook 1"
                                           :command (lambda () (demo-notebook))))
           (notebook-2     (make-instance  'button
                                           :text    "notebook 2"
                                           :command (lambda () (demo-notebook-2))))
           (defwidget      (make-instance  'button
                                           :text    "defwidget"
                                           :command (lambda () (demo-defwidget))))
           (demo-canvas    (make-instance  'button
                                           :text    "canvas"
                                           :command (lambda () (demo-canvas))))
           (demo-image     (make-instance  'button
                                           :text    "images"
                                           :command (lambda () (demo-image))))
           (demo-text      (make-instance 'button
                                          :text    "text widget"
                                          :command (lambda () (demo-text))))
           (demo-treelist  (make-instance  'button
                                           :text    "(mw) treelist"
                                           :command (lambda () (nodgui.mw::treelist-test))))
           (demo-tooltip   (make-instance  'button
                                           :text    "(mw) tooltip"
                                           :command (lambda () (nodgui.mw::tooltip-test))))
           (demo-gtree     (make-instance  'button
                                           :text    "(mw) graphical tree"
                                           :command (lambda () (nodgui.mw::gtree-demo))))
           (demo-auto-listbox (make-instance 'button
                                             :text    "(mw) autocomplete listbox"
                                             :command (lambda ()
                                                        (nodgui.mw::autocomplete-listbox-demo))))
           (demo-search-listbox (make-instance 'button
                                               :text    "(mw) searchable listbox"
                                               :command (lambda ()
                                                          (nodgui.mw::searchable-listbox-demo))))
           (demo-list-select    (make-instance 'button
                                               :text    "(mw) list select demo"
                                               :command (lambda ()
                                                          (nodgui.mw::list-select-demo))))
           (demo-listbox-dialog (make-instance 'button
                                               :text    "(mw) listbox dialog"
                                               :command (lambda ()
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
                                              :command (lambda ()
                                                         (nodgui.mw::date-picker-demo))))
           (demo-password      (make-instance 'button
                                              :text    "(mw) password entry"
                                              :command (lambda ()
                                                         (nodgui.mw::password-entry-demo))))
           (demo-star-progress (make-instance 'button
                                              :text    "(mw) star progress bar"
                                              :command (lambda ()
                                                         (nodgui.mw::star-progress-demo))))
           (demo-timeout-dialog (make-instance 'button
                                               :text    "(mw) message dialog with timeout"
                                               :command (lambda () (demo-message-timeout *tk*))))
           (demo-tklib-calendar (make-instance 'button
                                               :text    "(tklib) calendar"
                                               :command (lambda () (demo-tklib-calendar))))
           (demo-tklib-notify   (make-instance 'button
                                               :text    "(tklib) notify window"
                                               :command (lambda () (demo-tklib-notify))))
           (demo-tklib-dot-plot (make-instance 'button
                                               :text    "(tklib) scatter plot"
                                               :command (lambda () (demo-tklib-dot-plot))))
           (demo-tklib-bar-plot (make-instance 'button
                                               :text    "(tklib) histogram plot"
                                               :command (lambda () (demo-tklib-bar-plot))))
           (demo-tklib-swaplist (make-instance 'button
                                               :text    "(tklib) swaplist"
                                               :command (lambda () (demo-tklib-swaplist))))
           (demo-tklib-equalizer-bar (make-instance 'button
                                                    :text    "(tklib) equalizer bar"
                                                    :command (lambda ()
                                                               (demo-tklib-equalizer-bar))))
           (demo-validate-command    (make-instance 'button
                                                    :text    "entry with validate command"
                                                    :command (lambda ()
                                                               (demo-validate-command))))
           (demo-multithread         (make-instance 'button
                                                    :text    "Multithreading"
                                                    :command (lambda ()
                                                               (demo-multithread))))
           (demo-style               (make-instance 'button
                                                    :text "Custom style"
                                                    :command (lambda ()
                                                               (demo-custom-style))))
           (demo-autocomplete-entry  (make-instance 'button
                                                    :text "(mw) Entry with auto completion"
                                                    :command (lambda ()
                                                               (demo-autocomplete-entry))))
           (demo-multifont-listbox    (make-instance 'button
                                                     :text "(mw) multifont listbox"
                                                     :command (lambda ()
                                                                (demo-multifont-listbox))))
           (demo-paned-window         (make-instance 'button
                                                     :text "Paned windows"
                                                     :command (lambda ()
                                                                (demo-paned-window))))
           (demo-animation            (make-instance 'button
                                                     :text "Pixels buffer window (animations)"
                                                     :command (lambda ()
                                                                (demo-pixel-buffer-animation))))
           (demo-pixel-buffer         (make-instance 'button
                                                     :text "Pixels buffer window (blocking)"
                                                     :command (lambda ()
                                                                (demo-pixel-buffer))))
           (demo-3d                   (make-instance 'button
                                                     :text "3D rendering demo"
                                                     :command (lambda ()
                                                                (demo-terrain))))
           (b-quit                    (make-instance 'button
                                                     :text    "Quit lisp 🙂"
                                                     :command (lambda ()
                                                                (exit-nodgui)))))
      (grid widget                   0 0  :sticky :nswe)
      (grid eyes                     0 1  :sticky :nswe)
      (grid modal                    0 2  :sticky :nswe)
      (grid combo                    1 0  :sticky :nswe)
      (grid packtest1                1 1  :sticky :nswe)
      (grid packtest2                1 2  :sticky :nswe)
      (grid scrolled-frame           2 0  :sticky :nswe)
      (grid button-text              2 1  :sticky :nswe)
      (grid treeview                 2 2  :sticky :nswe)
      (grid w/widget                 3 0  :sticky :nswe)
      (grid notebook-1               3 1  :sticky :nswe)
      (grid notebook-2               3 2  :sticky :nswe)
      (grid defwidget                4 0  :sticky :nswe)
      (grid demo-canvas              4 1  :sticky :nswe)
      (grid demo-image               4 2  :sticky :nswe)
      (grid demo-text                5 0  :sticky :nswe)
      (grid demo-treelist            5 1  :sticky :nswe)
      (grid demo-tooltip             5 2  :sticky :nswe)
      (grid demo-gtree               6 0  :sticky :nswe)
      (grid demo-auto-listbox        6 1  :sticky :nswe)
      (grid demo-search-listbox      6 2  :sticky :nswe)
      (grid demo-list-select         7 0  :sticky :nswe)
      (grid demo-listbox-dialog      7 1  :sticky :nswe)
      (grid demo-date-picker         7 2  :sticky :nswe)
      (grid demo-password            8 0  :sticky :nswe)
      (grid demo-star-progress       8 1  :sticky :nswe)
      (grid demo-timeout-dialog      8 2  :sticky :nswe)
      (grid demo-tklib-calendar      9 0  :sticky :nswe)
      (grid demo-tklib-notify        9 1  :sticky :nswe)
      (grid demo-tklib-dot-plot      9 2  :sticky :nswe)
      (grid demo-tklib-bar-plot      10 0 :sticky :nswe)
      (grid demo-tklib-swaplist      10 1 :sticky :nswe)
      (grid demo-tklib-equalizer-bar 10 2 :sticky :nswe)
      (grid demo-validate-command    11 0 :sticky :nswe)
      (grid demo-multithread         11 1 :sticky :nswe)
      (grid demo-style               11 2 :sticky :nswe)
      (grid demo-autocomplete-entry  12 0 :sticky :nswe)
      (grid demo-multifont-listbox   12 1 :sticky :nswe)
      (grid demo-paned-window        12 2 :sticky :nswe)
      (grid demo-animation           13 0 :sticky :nswe)
      (grid demo-pixel-buffer        13 1 :sticky :nswe)
      (grid demo-3d                  13 2 :sticky :nswe)
      (grid b-quit                   14 0 :sticky :nswe :columnspan 3)
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

(defun demo-widget(&key theme)
  (setf *debug-tk* t)
  (with-nodgui (:debug-tcl nil
                :theme theme)
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
             (mf-exit (make-menubutton mfile "Exit" (lambda () (exit-wish))
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
                                (exit-wish)))
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

(defun demo-eyes (&key theme)
  (with-nodgui (:theme theme)
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

(defun demo-modal (&key theme)
  (with-nodgui (:theme theme)
   (let* ((b (make-instance 'button :text "Input"
                            :command (lambda ()
                                       (let ((erg (text-input-dialog *tk*
                                                                     "Enter a string:"
                                                                     "String input")))
                                         (if erg
                                             (format t "input was: ~a~%" erg)
                                           (format t "input was cancelled~%"))
                                       (finish-output))))))
     (pack b))))

(defun demo-combo (&key theme)
  (setf *debug-tk* t)
  (with-nodgui (:theme theme)
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

(defun demo-packtest1 (&key theme)
  (with-nodgui (:theme theme)
    (dotimes (i 10)
      (let ((s ""))
        (dotimes (j i)
          (setf s (format nil "~a " s)))
      (pack (make-instance 'button :text (format nil "Button~a Nr. ~a" s i)))
      (sleep 0.1)))))

(defun demo-packtest2 (&key theme)
  (with-nodgui (:debug-tcl t
                :theme theme)
    (with-send-batch
        (dotimes (i 10)
          (pack (make-instance 'button :text (format nil "Button Nr. ~a" i)))))))

(defun demo-sct (&key theme)
  (with-nodgui (:debug-tcl t
                :theme theme)
    (let* ((sf (make-instance 'scrolled-frame))
           (f  (interior sf))
           (n  1)
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

(defun demo-escape-text (&key theme)
  (with-nodgui (:theme theme)
    (let ((b (make-instance 'button :text " a button [a] :)")))
      (pack b)
      (setf (text b) " )} ~[eee]~ ")
      (flush-wish))))

;;; treeview tests

(defclass treeviewtest (frame)
   ((tree :accessor tree :initform nil :initarg :tree)))

(defmethod initialize-instance :after ((object treeviewtest) &key)
  (let ((tree (make-instance 'scrolled-treeview
                             :master  object
                             :pack    '(:side :top :expand t :fill :both)
                             ;; the following are going  to be the ids
                             ;; of  the  column   and  theirs  default
                             ;; labels too
                             ;; note: every item has an implicit first column
                             :columns (list "cid1" "cid2"))))
    (setf (tree object) tree)
    ;; a tree-item's instance represent a row, with or without parent,
    ;; of this treeview
    (let* ((parent    (make-instance 'tree-item
                                     :text    "~hallo~" ; text of the first column
                                     :id      "[a]"
                                     ;; the parent of this item is the root of the treeview
                                     :parent  +treeview-root+
                                     :index   +treeview-last-index+))
           (child      (make-instance 'tree-item
                                      ;; text of the first column
                                      :text          "w[e]lt"
                                      ;; the parent of this item is the first item defined
                                      :parent        (id parent)
                                      ;; text of the second and third column
                                      :column-values '("} [~hello]" "[world]")
                                      :index          +treeview-last-index+))
           (second-row nil)) ; used later...
       ;; setup headers
       (treeview-heading tree     +treeview-first-column-id+ ; or #0
                         :text    "column 0"
                         :command (lambda ()
                                    (when (> (length (items (treeview tree))) 1)
                                      (setf (children tree parent) (list child second-row))
                                      (do-msg "You clicked on column 0, row collapsed."))))
       (treeview-heading tree     "cid1" ; or "#1"
                         :text    "column 1"
                         :command (lambda ()
                                    (when (treeview-find-item tree parent)
                                      (treeview-delete tree parent)
                                      (do-msg "First row deleted"))))
       (treeview-heading tree     "cid2" ; or '#2' or +treeview-last-index+ in this case
                         :text    "column 2"
                         :command (lambda ()
                                    (do-msg "You clicked on column 2")))
       (treeview-insert-item tree :item parent) ; first row
       (treeview-insert-item tree :item child)  ; child of first row
       ;; you can insert value without instancing a tree-item object
       (setf second-row
             (treeview-insert-item tree                            ; actual second row
                                   :text          "foo"            ; first column
                                   :column-values '("bar" "baz"))) ; second and third column
       (format t "~a~%" tree))))

(defun demo-treeview (&key theme)
  (with-nodgui (:theme theme)
    (pack (make-instance 'treeviewtest) :fill :both :expand t)))

(defun demo-with-widgets (&key theme)
  (with-nodgui (:theme theme)
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
(defun demo-notebook (&key theme)
  (with-nodgui (:theme theme)
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

(defun demo-notebook-2 (&key theme)
  (with-nodgui (:theme theme)
    (let ((w (make-instance 'nbw)))
      (pack w :side :top :fill :both :expand t))))

(defwidget test-widget (frame)
  (a b c)
  ((bu button :text "A button"
       :pack (:side :top :anchor :w)
       :command (lambda ()
                  (format t "the content of entry is:~a~%" (text entry)) (finish-output)
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

(defun demo-defwidget (&key theme)
  (declare (optimize (debug 3)))
  (with-nodgui (:theme theme)
    (let ((mw (make-instance 'test-widget2)))
      (format t "mw is: ~a~%" mw) (finish-output)
      ;(error "test: ~a~%" mw)
      (pack mw :side :top :fill :both :expand t)
      (pack (make-instance 'button :text "get first"
                           :command (lambda ()
                                      (format t "the first is: ~a~%" (firstline mw))
                                      (finish-output))) :side :top :fill :both :expand t))))

(defun demo-canvas (&key theme)
  (flet ((bind-red (e)
           (declare (ignore e))
           (do-msg "You clicked on red!"))
         (bind-green (canvas item)
           (lambda (e)
             (declare (ignore e))
             (item-delete canvas item)
             (do-msg "You clicked on green and the blue arc disappeared!"))))
    (let ((*debug-tk* t))
      (with-nodgui (:theme theme)
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
               (star         (make-star canvas 80 0.5 "#ff0000" "#000000" 5))
               (bicolor-star (make-two-color-star canvas 80 0.5
                                                  "#FFFF00" "#FFFF00"
                                                  "#BEBEBE"   "#FFFF00"
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
          (item-configure canvas arc1 "fill"  "#ff0000") ;; using x11-colors via cl-colors2
          (item-configure canvas arc2 "fill"  "#00ff00") ;; strings are accepted, though
          (item-configure canvas arc1 "tag"   "red")
          (item-configure canvas arc2 "tag"   "green")
          (tag-bind       canvas      "red"   #$<ButtonPress-1>$ #'bind-red)
          (tag-bind       canvas      "green" #$<ButtonPress-1>$ (bind-green canvas ball))
          (item-move-to   canvas ball (* size 9/10) (* size 9/10))
          (item-raise     canvas ball arc1)
          (make-rectangle canvas
                          0  (* 3/4 size)
                          50 (+ (/ size 2) 50)
                          :fill    "#ff0000"
                          :outline "#0000ff")
          (make-items canvas `((:rectangle 0  ,(/ size 2) 50 ,(+ (/ size 2) 50))
                               (:arc       0  ,(+ (/ size 2) 10)
                                           50 ,(+ (/ size 2) 60)
                                           :start 0 :extent 180 :fill "#A020F0")
                               (:line      0 ,size ,size ,(- size 10) :width 5)))
          (pack canvas))))))

(defun demo-image (&key theme)
  (setf *debug-tk* t)
  (with-nodgui (:theme theme)
    (let ((b (make-instance 'button :text "load image")))
      (setf (command b)
            #'(lambda ()
                (let ((file (get-open-file :file-types '(("JPG"     "*.jpg")
                                                         ("PNG"     "*.png")
                                                         ("TGA"     "*.tga")
                                                         ("RGB raw" "*.data")))))
                  (cond
                    ((cl-ppcre:scan "jpg$" file)
                     (let ((bitmap (nodgui.pixmap:scale-bilinear
                                     (nodgui.pixmap:slurp-pixmap 'nodgui.pixmap:jpeg file)
                                     0.1 0.1)))
                       (setf (image b) (make-image bitmap))))
                    ((cl-ppcre:scan "tga$" file)
                     (let ((bitmap (nodgui.pixmap:rotate-pixmap
                                    (nodgui.pixmap:scale-bilinear
                                     (nodgui.pixmap:slurp-pixmap 'nodgui.pixmap:tga file)
                                     2.0 2.1)
                                    30.0
                                    :fill-value (nodgui.ubvec4:ubvec4 0 128 0)
                                    :repeat nil)))
                       (setf (image b) (make-image bitmap))))
                    ((cl-ppcre:scan "png$" file)
                     (let ((bitmap (nodgui.pixmap:rotate-pixmap
                                    (nodgui.pixmap:scale-bilinear
                                     (nodgui.pixmap:slurp-pixmap 'nodgui.pixmap:png file)
                                     2.0 2.1)
                                    45.0
                                    :repeat t)))
                       (setf (image b) (make-image bitmap))))
                    (t
                     (let ((w (text-input-dialog (root-toplevel)
                                                 "Enter a string"
                                                 "Pixel width? "))
                           (h (text-input-dialog (root-toplevel)
                                                 "Enter a string"
                                                 "Pixel height? ")))
                       (when (and w h)
                         (with-open-file (stream file :element-type '(unsigned-byte 8))
                           (let* ((data (read-into-array stream (file-length stream))))
                             (setf (image b) (make-image data
                                                        (parse-integer w)
                                                        (parse-integer h))))))))))))
      (place b 0 0))))

(defun demo-message-timeout (parent)
  (nodgui.mw:message-with-timeout parent
                                  "This window will be closed after 10 seconds"
                                  10
                                  "OK"))

(define-constant +bell-icon+
    (strcat "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA7wDGACtqWfjDAA"
            "AACXBIWXMAAA7DAAAOwwHHb6hkAAABXklEQVRYw9WWPU4DQQyF7RUpo1UKGlqUioqWG+wd"
            "0uUA6RElF+AAdLlDbpCWigbECSiiUZpIkTJUg2bNeO3xzhB41f5pbH9+6xkEo54fr318v3"
            "z4QMs6jTG46plG2VnvtjceAKC9vE1XNF9jNQKn94Uv8Y2JQKg8iCOQS6KBMwsVSAEAwH2+"
            "sL130xXA4Q3a4+bH2s18PZ6AGDxcT7r4lcoLF9rgnNr9Uy8RBxCT8BLlKh4gJGwEot5nBS"
            "c++F6H84KagPTbWdWcE7+YgGRAZRI+ywOp3ufgDx5wk67nB84LQwR8wXZ4cRKGDFP4tQTC"
            "LAizgZBAslfwBCzBaWAtURyq3PL7UQqhDZEfkPWA1fWZ8kkCqeBjho+brjgv9HfLGsFTOy"
            "VHAulJp/TYpSTESVhj5g+RQOmUW1t//0wonY6pZnev+K8IjE/g6v7XE8CBJLC6Bzgv5Pa+"
            "RAuwRCFf0dKhJufiL2QAAAAASUVORK5CYII=")
  :test          #'string=
  :documentation "A bell icon in png format.")

(defun demo-text (&key theme)
  (setf *debug-tk* t)
  (with-nodgui (:theme theme)
    (let* ((text-widget          (make-instance 'scrolled-text
                                                :read-only                  nil
                                                :use-horizontal-scrolling-p nil))
           (default-font         (font-create   "default"
                                                :family "Sans"
                                                :size   "14")) ; positive number = units in points
           (link-font            (font-create   "linkFont"
                                                :family "Sans"
                                                :size   "14"
                                                :weight :bold
                                                :slant  :italic
                                                :underline (lisp-bool->tcl t)))
           (tag-link-index-start (parse-indices '(+ (:line 6 :char 0) 12 :chars)))
           (tag-link-index-end   (parse-indices '(+ (:line 6 :char 0) 34 :chars)))
           (link-color           (rgb->tk cl-colors2:+blue+))
           (re-matched-color     (rgb->tk cl-colors2:+red+))
           (bell-image           (make-image +bell-icon+)))
      (configure text-widget :font default-font)
      (configure text-widget :wrap :word)
      (grid text-widget 0 0 :sticky :news)
      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure *tk* :all :weight 1)
      (append-line text-widget
                   "type random text, it will try fit it to the width of this widget")
      (append-line text-widget "")
      (append-line text-widget
                   (format nil
                           "this line will replaced with an image if you click below 👇"))
      (loop repeat 2 do
        (append-newline text-widget))
      (loop for i from 0 below 200 do
        (append-line text-widget (format nil "~a some text is clickable like that." i)))
      (let* ((tag-placeholder-image (highlight-text text-widget
                                                    '(:line 3 :char 0)
                                                    :end-index '(:line 3 :char :end)))
             (tag-link (make-link-button text-widget
                                         tag-link-index-start
                                         tag-link-index-end
                                         link-font
                                         link-color
                                         (rgb->tk cl-colors2:+green+)
                                         (lambda ()
                                           (delete-in-range text-widget
                                                            `(:tag ,tag-placeholder-image :first)
                                                            `(:tag ,tag-placeholder-image :last))
                                           (insert-image text-widget
                                                         bell-image
                                                         '(:line 3 :char 0))
                                           (format t
                                                   "line under cursor info ~s~%"
                                                   (line-info text-widget)))
                                         :button-2-callback
                                         (lambda ()
                                           (format t
                                                   "match data for ~a lines: ~s~%"
                                                   (maximum-lines-number text-widget)
                                                   (search-all-text text-widget
                                                                    "[aeiou].")))
                                         :button-3-callback
                                         (lambda ()
                                           (multiple-value-bind (start-index
                                                                 end-index
                                                                 tag-name
                                                                 lines
                                                                 chars
                                                                 size)
                                               (search-regexp text-widget
                                                              "click.+l"
                                                              "1.0"
                                                              :tag-matching-region t)
                                             (format t
                                                     "matching ~a ~a ~a ~a ~a tag; ~s"
                                                     start-index
                                                     end-index
                                                     lines
                                                     chars
                                                     size
                                                     tag-name)
                                             (tag-configure text-widget
                                                            tag-name
                                                            :underline  (lisp-bool->tcl nil)
                                                            :foreground re-matched-color)
                                             (see text-widget `(:tag ,tag-name :first)))))))
        (append-line text-widget (format nil
                                         "link@ ~s"
                                         (tag-ranges text-widget tag-link)))
        (move-cursor-to-last-line text-widget)))))

(defun demo-multifont-listbox (&key theme)
  (with-nodgui (:theme theme)
    (let ((listbox (make-instance 'multifont-listbox
                                  :master *tk*)))
      (grid listbox 0 0 :sticky :news)
      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure *tk* :all :weight 1)
      (wait-complete-redraw)
      (loop for word in (split-words "lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
            do
               (listbox-append listbox word))
      (boldify-multifont-item listbox 1 '(1 3 4))
      (let* ((custom-font     (font-create "sans" :size 14 :overstrike t))
             (tag-custom-font (tag-create listbox
                                          (create-tag-name)
                                          `(:line 3 :char 0)
                                          `(:line 3 :char 5))))
        (tag-configure listbox
                       tag-custom-font
                       :font       custom-font
                       :foreground (rgb->tk cl-colors2:+white+)
                       :background (rgb->tk cl-colors2:+red+))))))

;; tklib

(defun demo-tklib-calendar (&key theme)
  (with-nodgui (:theme theme)
    (let ((cal (nodgui.tklib.calendar:make-calendar)))
      (nodgui.tklib.calendar:set-date* cal "10/10/2010")
      (setf (command cal)
            (lambda (a)
              (let ((date-selected (multiple-value-list
                                    (nodgui.tklib.calendar:parse-selected-date cal a))))
                (message-box (format nil "chosen ~s~%" date-selected)
                             "info"
                             :ok
                             "info"
                             :parent *tk*))))
      (grid cal 0 0))))

(defun demo-tklib-notify (&key theme)
  (with-nodgui (:theme theme)
    (let ((message (text-input-dialog *tk*
                                      "info"
                                      "Insert the text you want shown as notify")))
      (format t "message ~a~%" message)
      (nodgui.tklib.notify:notify-window message))))

(defun demo-tklib-dot-plot (&key theme)
  (with-nodgui (:theme theme)
    (let* ((canvas     (make-canvas nil :width  800 :height 600))
           (series-callback (lambda (a)
                              (lambda (event)
                                (format t
                                        "click on a ~a point @ x: ~a y: ~a~%"
                                        a
                                        (event-x event)
                                        (event-y event)))))
           (all-series (list (make-instance 'nodgui.tklib.plot:dot-series
                                            :xs     '(10   20   30)
                                            :ys     '(20.1 29.9 60.5)
                                            :errors '(1.1   5.5 1.2)
                                            :legend "first"
                                            :color  "#ff00ff")
                             (make-instance 'nodgui.tklib.plot:dot-series
                                            :xs            '(10 25 30)
                                            :ys            '(60 30 20)
                                            :legend        "second"
                                            :value-symbol  :upfilled
                                            :callback      (funcall series-callback
                                                                    :red))))
           (plot       (make-instance 'nodgui.tklib.plot:dot-plot
                                      :all-series all-series)))
      ;; as an example, fit a line on the first data series
      (setf (nodgui.tklib.plot:callback (first all-series))
            (lambda (event)
              (declare (ignore event))
              (let* ((series      (first all-series))
                     (data        (map 'vector
                                       #'identity
                                       (mapcar #'nodgui.vec2:vec2
                                               (nodgui.tklib.plot:xs series)
                                               (nodgui.tklib.plot:ys series))))
                     (errors      (map 'vector #'identity (nodgui.tklib.plot:errors series))))
                (multiple-value-bind (m q)
                    (nodgui.fit-line:fit-line data errors)
                  (flet ((line (x)
                           (+ (* m x) q)))
                    (let* ((x1 0)
                           (x2 40)
                           (y1 (line x1))
                           (y2 (line x2)))
                      (nodgui.tklib.plot:place-line plot series x1 y1 x2 y2 "#0000ff" :width 2)))))))
      (grid canvas 0 0 :sticky :news)
      (nodgui.tklib.plot:draw-on-canvas plot canvas)
      (bind plot #$<ButtonPress-1>$ (lambda (event)
                                      (format t "click on plot x: ~a y: ~a~%"
                                              (event-x event)
                                              (event-y event)))))))

(defun demo-tklib-swaplist (&key theme)
  (with-nodgui (:theme theme)
    (let* ((swaplist  (nodgui.tklib.swaplist:make-swaplist '(1 2 3) '(4 5 6)
                                                           :left-list-label  "Left"
                                                           :right-list-label "Right"))
           (ok-button (make-instance 'button
                                     :text    "OK"
                                     :command (lambda () (to-stderr "~s"
                                                                    (value swaplist))))))
      (grid swaplist  0 0)
      (grid ok-button 1 0 :pady 10))))

(defun demo-tklib-equalizer-bar (&key theme)
  (with-nodgui (:theme theme)
    (let ((equalizer-bar (nodgui.tklib.misc-widget:make-equalizer-bar :from           0.0
                                                                      :to            10.0
                                                                      :warning-level  8.0
                                                                      :segments      10
                                                                      :number         3))
          (time          0.0)
          (dt            0.01))
      (setf (value equalizer-bar) '(10 0.0 0.0))
      (grid equalizer-bar 0 0)
      (labels ((animate ()
                 (after 20
                        (lambda ()
                          (incf time dt)
                          (setf (value equalizer-bar)
                                (loop for i from 0 below 3 collect
                                     (abs (* 11 (sin (+ time
                                                        (* 10.0 i)))))))
                          (animate)))))
        (animate)))))

(defun demo-tklib-bar-plot (&key theme)
  (with-nodgui (:theme theme)
    (let* ((canvas          (make-canvas nil :width  800 :height 600))
           (all-series (list (make-instance 'nodgui.tklib.plot:bar-series
                                            :ys  '(20 40 60)
                                            :legend "first"
                                            :color  "#ff00ff")
                             (make-instance 'nodgui.tklib.plot:bar-series
                                            :ys     '(60 40)
                                            :legend "second")))
           (plot       (make-instance 'nodgui.tklib.plot:bar-chart
                                      :x-labels '("~A label~" "B" "C")
                                      :all-series all-series)))
      (grid canvas 0 0 :sticky :news)
      (nodgui.tklib.plot:draw-on-canvas plot canvas)
      (bind plot #$<ButtonPress-1>$ (lambda (event)
                                      (format t "click on plot x: ~a y: ~a~%"
                                              (event-x event)
                                              (event-y event)))))))

(defun demo-validate-command (&key theme)
  (with-nodgui (:theme theme)
    (flet ((validate-function (action
                               current-string
                               new-string
                               index
                               validation-action)
             (format t
                     "action ~a current string ~a new ~a index ~a validation action ~a~%"
                     action
                     current-string
                     new-string
                     index
                     validation-action)
             (let ((true  (lisp-bool->tcl t))
                   (false (lisp-bool->tcl nil)))
               (if (cl-ppcre:scan "[0-9]" new-string)
                   (send-wish-line true)
                   (send-wish-line false)))))
      (let ((label  (make-instance 'label
                                   :master nil
                                   :text  (strcat "This entry uses \"validatecommand\" "
                                                  "to prevent user to insert "
                                                  "values that are not digits (base 10).")))
            (entry  (make-instance
                     'entry
                     :validate        :key
                     :validatecommand #'validate-function)))
        (pack label)
        (pack entry)))))

(defun demo-multithread (&key theme)
  (setf *debug-tk* nil)
  (with-nodgui (:theme theme)
    (let* ((description-text (strcat "In this demo many threads simultaneously "
                                     "compete to write to and read from the same text widget"
                                     "moving the mouse over the text area will make the cursor jump at the end of the text"))
           (description      (make-instance 'label
                                            :font "12"
                                            :text description-text))
           (text-area        (make-instance 'scrolled-text))
           (button           (make-instance 'button :text "start"))
           (wish-subprocess  *wish*))
      (flet ((start-write-thread (name)
               (make-thread (lambda ()
                                 (let ((*wish* wish-subprocess))
                                   (loop repeat 20 do
                                     (append-line text-area name))))
                               :name "read thread"))
             (start-read-thread ()
               (make-thread (lambda ()
                                 (let ((*wish* wish-subprocess))
                                   (loop for i from 0 below 100 do
                                     (format t
                                             "text ~a ~a~%" i
                                             (text text-area)))))
                               :name "write thread")))
        (grid description 0 0 :sticky :nswe)
        (grid text-area   1 0 :sticky :nswe)
        (grid button      2 0 :sticky :nswe)
        (bind (inner-text text-area)
              #$<Motion>$ (lambda (e)
                            (declare (ignore e))
                            (see (inner-text text-area) (make-indices-end))))
        (setf (command button)
              (lambda ()
                (let ((read-thread (start-read-thread))
                      (write-threads (loop for i from 0 below 3000
                                             collect
                                             (start-write-thread (format nil "thread-~a " i))))
                      (more-read-threads (loop repeat 2000
                                               collect
                                               (make-thread
                                                (lambda ()
                                                  (let ((*wish* wish-subprocess))
                                                    (maximum-lines-number text-area)))))))
                  (loop for i in more-read-threads do (join-thread i))
                  (loop for i in write-threads do (join-thread i))
                  (format t "end write threads~%")
                  (join-thread read-thread)
                  (format t "end read thread~%"))))))))


(define-constant +red-corner+
  (strcat "R0lGODlhEAAQAIABAP8AAGdqcSH+EUNyZWF0ZWQgd2l0aCBHSU1QACH5BAEKAAEALAAAAAAQABAA"
          "AAIqjAOAyWy6VksH0knrU/i8LWWexTneF5knAoorya5aJI9gPUM4utv9jysAADs=")
  :test #'string=)

(define-constant +blue-corner+
  (strcat "R0lGODlhEAAQAIABAAAA/2dqcSH+EUNyZWF0ZWQgd2l0aCBHSU1QACH5BAEKAAEALAAAAAAQABAA"
          "AAIqjAOAyWy6VksH0knrU/i8LWWexTneF5knAoorya5aJI9gPUM4utv9jysAADs=")
  :test #'string=)

(defun demo-custom-style (&key theme)
  (let ((*debug-tk* t))
    (with-nodgui (:theme theme)
      (let* ((b            (make-instance 'button :text "Click to change style"))
             (corner-state 0)
             (corner-style (make-style corner-style (:extend "TButton")
                                       :foreground #%dim-gray%
                                       :padding    0
                                       :font      "times 20"))
             (red-corner   (make-image +red-corner+))

             (blue-corner  (make-image +blue-corner+))
             (red-style    (make-style red-style (:action :element-create)
                               :image (name red-corner)))
             (blue-style   (make-style blue-style (:action :element-create)
                               :image (name blue-corner)))
             (red-layout   (insert-layout (fetch-layout :corner-style)
                                                     '(:red-style
                                                       :side "right"
                                                       :sticky "ne")
                                                     "Button.label"))
             (blue-layout  (insert-layout (fetch-layout :corner-style)
                                                      '(:blue-style
                                                        :side "right"
                                                        :sticky "ne")
                                                      "Button.label")))
        (apply-style corner-style)
        (apply-style red-style)
        (apply-style blue-style)
        (layout-configure corner-style blue-layout)
        (style-configure b corner-style)
        (setf (command b)
              (lambda ()
                (if (= (rem corner-state 2) 0)
                    (layout-configure corner-style red-layout)
                    (layout-configure corner-style blue-layout))
                (incf corner-state)))
        (pack b)))))

(defun demo-autocomplete-entry (&key theme)
  (let ((*debug-tk* t))
    (with-nodgui (:theme theme)
      (let* ((data                  (append '("foo" "bar" "baz")
                                            (loop for i from 0 to 10 collect
                                                                     (format nil "~2,'0d" i))))
             (autocomplete-function (lambda (hint)
                                      (values (loop for datum in data when (cl-ppcre:scan hint datum)
                                                    collect datum)
                                              (loop for datum in data when (cl-ppcre:scan hint datum)
                                                    collect
                                                    (multiple-value-bind (start end)
                                                        (cl-ppcre:scan hint datum)
                                                      (loop for i from start below end collect i))))))
             (autocomplete-widget   (make-instance 'autocomplete-entry
                                                   :autocomplete-function autocomplete-function))
             (button-command        (lambda ()
                                      (do-msg (format nil
                                                      "selected ~s~%"
                                                      (text autocomplete-widget)))))
             (label                 (make-instance 'label
                                                   :text "Type a digit or a letter in [aobfrz]"))
             (button                (make-instance 'button
                                                   :text "OK"
                                                   :command button-command)))
        (grid label               0 0)
        (grid autocomplete-widget 1 0)
        (grid button              2 0)))))

(defun demo-multithread-2 (&key theme)
  (setf *debug-tk* t)
  (let* ((wish-subprocess nil))
    (with-nodgui (:theme theme)
      (let* ((text-area (make-instance 'scrolled-text))
             (button    (make-instance 'button
                                       :text "start"
                                       :command
                                       (lambda ()
                                         (make-thread
                                          (lambda ()
                                            (setf *wish* wish-subprocess)
                                            (format t "ww ~a~%" (screen-width)))
                                          :name "read thread"))))
             (button-append (make-instance 'button
                                           :text "append \"foobar\""
                                           :command
                                           (lambda ()
                                             (let ((text (alexandria:random-elt '("foo"
                                                                                  "bar"
                                                                                  "baz"))))
                                               (append-line text-area text))))))
        (setf wish-subprocess *wish*)
        (grid text-area     0 0 :sticky :nswe)
        (grid button        1 0 :sticky :nswe)
        (grid button-append 2 0 :sticky :nswe)
        (append-line text-area "lorem isum ecceterea")))))

(defparameter *queue* '())

(defparameter *queue-lock* (make-lock "queue lock"))

(defparameter *queue-cond* (make-condition-variable))

(defun pop-event-block ()
  (with-lock-held (*queue-lock*)
    (loop while (null *queue*)
          do
             (condition-wait *queue-cond* *queue-lock*))
    (pop *queue*)))

(defun push-event-unblock (value)
  (with-lock-held (*queue-lock*)
    (push value *queue*)
    (condition-notify *queue-cond*)))

(defparameter *stop-events-loop* t)

(defparameter *events-loop-lock* (make-lock "events-loop-lock"))

(defparameter *events-loop-thread* nil)

(defparameter *gui-server* nil)

(defun events-loop-running-p ()
  (with-lock-held (*events-loop-lock*)
    (not *stop-events-loop*)))

(defun stop-events-loop ()
  (with-lock-held (*events-loop-lock*)
    (setf *stop-events-loop* t)))

(defun start-events-loop ()
  (with-lock-held (*events-loop-lock*)
    (setf *stop-events-loop* nil))
  (setf *events-loop-thread*
        (make-thread (lambda ()
                          (let ((*wish* *gui-server*))
                            (format  t "start~%")
                            (loop while (events-loop-running-p) do
                              (dispatch-program-events-or-wait))
                            (format  t "end~%"))))))

(defun dispatch-program-events-or-wait ()
  (let ((event (pop-event-block)))
    (funcall event)))

(defun demo-test-loop ()
  (start-events-loop)
  (push-event-unblock (lambda ()
                        (format t "event!~%")
                        (finish-output)))
  (sleep 1) ; simulate user interation
  (stop-events-loop)
  (push-event-unblock (lambda ()
                        (format t "dummy~%")
                        (finish-output))))

(defun demo-multithread-loop (&key theme)
  (setf *debug-tk* t)
  (start-events-loop)
  (with-nodgui (:theme theme)
    (setf *gui-server* *wish*)
    (let* ((text-area (make-instance 'scrolled-text))
           (button    (make-instance 'button
                                     :text "start"
                                     :command
                                     (lambda ()
                                       (push-event-unblock
                                        (lambda ()
                                          (let ((*wish* *gui-server*))
                                            (format t
                                                    "screen width ~a~%"
                                                    (screen-width))))))))
           (button-append (make-instance 'button
                                         :text "append \"foobar\""
                                         :command
                                         (lambda ()
                                           (let ((text (alexandria:random-elt '("foo"
                                                                                "bar"
                                                                                "baz"))))
                                             (append-line text-area text)))))
           (button-quit   (make-instance 'button
                                         :text "quit"
                                         :command (lambda ()
                                                    (stop-events-loop)
                                                    (push-event-unblock (lambda ()
                                                                          (format t "dummy~%")
                                                                          (finish-output)))
                                                    (exit-wish)))))
      (grid text-area     0 0 :sticky :nswe)
      (grid button        1 0 :sticky :nswe)
      (grid button-append 2 0 :sticky :nswe)
      (grid button-quit   3 0 :sticky :nswe)
      (append-line text-area "lorem isum ecceterea"))))

(defun demo-paned-window (&key theme)
  (setf *debug-tk* t)
  (with-nodgui (:theme theme)
    (let* ((paned-frame (make-instance 'paned-window :orientation :horizontal))
           (text-left   (make-instance 'text :master paned-frame))
           (listbox     (make-instance 'listbox :master paned-frame)))
      (add-pane paned-frame text-left)
      (add-pane paned-frame listbox)
      (grid paned-frame 0 0 :sticky :news)
      (append-line text-left "drag me! ———————⮞")
      (let ((tag (tag-create text-left
                             (create-tag-name)
                             (make-indices-start)
                             (make-indices-end))))
        (tag-configure text-left tag :justify :right))
      (listbox-append listbox '("a listbox"))
      (listbox-append listbox '("click me"))
      (bind listbox
            #$<<ListboxSelect>>$
            (lambda (e)
              (declare (ignore e))
              (message-box (format nil
                                   "the paned window contains ~a panes: ~s"
                                   (length (panes paned-frame))
                                   (panes paned-frame))
                           "info"
                           :ok
                           "info"
                           :parent (root-toplevel)))))))
