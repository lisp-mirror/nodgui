;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2019 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.tklib.plot)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)
;(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(define-constant +plotchart-library-name+ "Plotchart" :test #'string=)

(defstruct series
  handle
  (xs     '())
  (ys     '())
  (legend "")
  (color  #%red%))

(defstruct (dot-series
             (:include series))
  (size   3.0)
  (errors '()))

(defclass plot ()
  ((handle
    :initform nil
    :accessor handle)
   (x-text
    :initform "x title"
    :initarg  :x-text
    :accessor x-text)
   (y-text
    :initform "y title"
    :initarg  :y-text
    :accessor y-text
    :documentation "(broken) This is vtext on the tcl side!")
   (x-subtext
    :initform ""
    :initarg  :x-subtext
    :accessor x-subtext)
   (y-subtext
    :initform ""
    :initarg  :y-subtext
    :accessor y-subtext
    :documentation "(broken) This is ysubtext on the tcl side!")
   (title
    :initform "plot title"
    :initarg  :title
    :accessor title)
   (subtitle
    :initform "plot subtitle"
    :initarg  :subtitle
    :accessor subtitle)
   (all-series
    :initform '()
    :initarg  :all-series
    :accessor all-series)))

(defstruct axis-conf min max step)

(defmethod initialize-instance :after ((object plot) &key &allow-other-keys)
  (require-tcl-package +plotchart-library-name+))

(defgeneric draw-on-canvas (object destination &key &allow-other-keys))

(defclass xy-plot (plot)
  ((x-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :x-axis-conf
    :accessor x-axis-conf)
   (y-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :y-axis-conf
    :accessor y-axis-conf)))

(defmethod draw-on-canvas ((object xy-plot) (destination canvas) &key &allow-other-keys)
  (with-accessors ((handle handle)
                   (x-axis-conf x-axis-conf)
                   (y-axis-conf y-axis-conf)
                   (x-text      x-text)
                   (y-text      y-text)
                   (x-subtext   x-subtext)
                   (y-subtext   y-subtext)
                   (title       title)
                   (subtitle    subtitle)) object
    (let* ((x-min   (axis-conf-min  x-axis-conf))
           (x-max   (axis-conf-max  x-axis-conf))
           (x-step  (axis-conf-step x-axis-conf))
           (y-min   (axis-conf-min  y-axis-conf))
           (y-max   (axis-conf-max  y-axis-conf))
           (y-step  (axis-conf-step y-axis-conf))
           (*suppress-newline-for-tcl-statements*             t)
           (*add-space-after-emitted-unspecialized-element*   nil))
      ;; create
      (format-wish (tclize `(senddata
                             ["::Plotchart::createXYPlot"      " "
                             {+,#[(widget-path destination) ]}  " "
                             [list ,#[x-min ] " " ,#[x-max ] " " ,#[x-step ]]
                             [list ,#[y-min ] " " ,#[y-max ] " " ,#[y-step ]]])))
      ;; get the handle (used below)
      (setf handle (read-data))
      ;; add title
      (format-wish (tclize `(senddata [ ,handle title {+ ,title } ])))
      ;; add subtitle
      (format-wish (tclize `(senddata [ ,handle subtitle {+ ,subtitle } ])))
      ;; axis labels
      (format-wish (tclize `(senddata [ ,handle xtext    {+ ,x-text } ])))
      (format-wish (tclize `(senddata [ ,handle vtext    {+ ,y-text } ])))
      (format-wish (tclize `(senddata [ ,handle xsubtext {+ ,x-subtext } ])))
      (format-wish (tclize `(senddata [ ,handle ysubtext {+ ,y-subtext } ])))
      object)))

(defclass dot-plot (xy-plot) ())

(defun draw-error-bar (plot-handle series-handle x y error-value stopper-width color)
  "The low level drawing procedure for error bar"
  (let ((start-bar     (- y error-value))
        (end-bar       (+ y error-value))
        (start-stopper (- x stopper-width))
        (stop-stopper  (+ x stopper-width)))
    (flet ((draw-line (x-start y-start x-end y-end)
             (format-wish (tclize `(senddata [,plot-handle
                                             object
                                             line " " ,series-handle " "
                                             ,x-start " " ,y-start   " "
                                             ,x-end   " " , y-end    " "
                                             ,(empty-string-if-nil color
                                                  `(-fill  {+ ,#[color ]}))
                                             ])))))
      (draw-line x start-bar x end-bar)
      (draw-line start-stopper start-bar stop-stopper start-bar)
      (draw-line start-stopper end-bar stop-stopper end-bar))))

(defun parse-number-or-0 (n)
  (if (numberp n)
      n
      (safe-parse-number n
                         :fix-fn (lambda (a)
                                   (declare (ignore a))
                                   0.0))))

(defmethod draw-on-canvas :after ((object dot-plot) (destination canvas)
                                  &key (error-bar-color nil))
  (with-accessors ((handle     handle)
                   (all-series all-series)) object
    (format-wish (tclize `(senddata [ ,handle legendconfig -legendtype rectangle])))
    (loop for series in all-series do
         (setf (series-handle series) (strcat "series_" (nodgui::create-name)))
         (let* ((size-as-num   (dot-series-size series))
                (size          (nodgui::process-coords size-as-num))
                (series-handle (series-handle   series))
                (xs            (series-xs       series))
                (ys            (series-ys       series))
                (errors        (or (dot-series-errors series)
                                   (make-fresh-list (length xs) 0.0)))
                (color         (series-color    series))
                (legend        (series-legend   series)))
           (format-wish (tclize `(senddata [ ,handle
                                           dotconfig
                                           ,series-handle " " -colour ,#[color ] " "
                                           -outline on])))
           (format-wish (tclize `(senddata [ ,handle
                                           dataconfig
                                           ,series-handle " " -colour ,#[color ] " "
                                           ])))
           (loop
              for x   in (split " " (nodgui::process-coords xs))
              for y   in (split " " (nodgui::process-coords ys))
              for err in (split " " (nodgui::process-coords errors)) do
                (let ((*suppress-newline-for-tcl-statements* t)
                      (x-as-num   (parse-number-or-0 x))
                      (y-as-num   (parse-number-or-0 y))
                      (err-as-num (parse-number-or-0 err)))
                  ;; errors
                  (when (not (epsilon= err-as-num 0.0))
                    (draw-error-bar handle series-handle
                                    x-as-num y-as-num err-as-num
                                    (/ size-as-num 2)
                                    error-bar-color))
                  (format-wish (tclize `(senddata [ ,handle " " dot " "  ,series-handle " "
                                                  {+ ,x } {+ ,y }
                                                  ,size ])))))
           (format-wish (tclize `(senddata [ ,handle
                                           legend
                                           ,series-handle " " ,#[legend ]]))))))
  object)
