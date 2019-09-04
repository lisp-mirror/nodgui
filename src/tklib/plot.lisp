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

(define-constant +plotchart-library-name+ "Plotchart" :test #'string=)

(define-constant +plotchart-data-tag+     "data" :test #'string=)

(defstruct series
  "keeps a set of data
- handle  the internal identifier of this series (do not touch it!);
- xs:     a list of x for each point of this series;
- ys:     a list of x for each point of this series;
- legend: a descriptive label (shown on the plot) for this series;
- color:  plotted color of each datum.
"
  handle
  (xs     '())
  (ys     '())
  (legend "")
  (color  #%red%))

(defstruct (dot-series
             (:include series))
"represent a point for a scatter plot (see: 'series')
- size:       radius of the point;
- error:      a list error for each point of the series;
- bind-event: the event a point of a series is reactve to;
- callback:   the function called (with an 'event' struct as parameter) when a bind-event is fired.
"
  (size       3.0)
  (errors     '())
  (bind-event #$<ButtonPress-1>$)
  (callback   nil))

(defclass plot ()
  ((handle
    :initform nil
    :accessor handle
    :documentation "The internal identifier of this series (do not touch it!)")
   (x-text
    :initform "x title"
    :initarg  :x-text
    :accessor x-text
    :documentation "The descriptive text of the x axis")
   (y-text              ; This is vtext on the tcl side
    :initform "y title"
    :initarg  :y-text
    :accessor y-text
    :documentation "The descriptive text of the y axis")
   (x-subtext
    :initform ""
    :initarg  :x-subtext
    :accessor x-subtext
    :documentation "(broken do not use)")
   (y-subtext
    :initform ""
    :initarg  :y-subtext
    :accessor y-subtext
    :documentation "(broken do not use)")
   (title
    :initform "plot title"
    :initarg  :title
    :accessor title
    :documentation "A descriptive text of the plot")
   (subtitle
    :initform "plot subtitle"
    :initarg  :subtitle
    :accessor subtitle
    :documentation "More descriptive text of the plot")
   (all-series
    :initform '()
    :initarg  :all-series
    :accessor all-series
    :documentation "The data of this plot, must be instance of the struct 'series' or derived")))

(defstruct axis-conf min max step)

(defmethod initialize-instance :after ((object plot) &key &allow-other-keys)
  (require-tcl-package +plotchart-library-name+)
  (nodgui::send-wish
   (defproc sendeventplot (x y fun ({+ other {+ }+ }))
     (sendevent $fun
                $x " " $y
                {?} {?} {?} {?} {?} {?} {?} {?}))))

(defgeneric draw-on-canvas (object destination &key &allow-other-keys))

(defclass xy-plot (plot)
  ((x-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :x-axis-conf
    :accessor x-axis-conf)
   (y-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :y-axis-conf
    :accessor y-axis-conf))
  (:documentation "A plot with the data representing points in the x/y plane."))

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
      (format-wish (tclize `(,handle title {+ ,title })))
      ;; add subtitle
      (format-wish (tclize `(,handle subtitle {+ ,subtitle })))
      ;; axis labels
      (format-wish (tclize `(,handle xtext    {+ ,x-text })))
      (format-wish (tclize `(,handle vtext    {+ ,y-text })))
      (format-wish (tclize `(,handle xsubtext {+ ,x-subtext })))
      (format-wish (tclize `(,handle ysubtext {+ ,y-subtext })))
      object)))

(defclass dot-plot (xy-plot)
  ()
  (:documentation "A scatter plot, see: https://en.wikipedia.org/wiki/Scatter_plot"))

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
                                             ])))
             (read-data)))
      (list (draw-line x start-bar x end-bar)
            (draw-line start-stopper start-bar stop-stopper start-bar)
            (draw-line start-stopper end-bar stop-stopper end-bar)))))

(defun parse-number-or-0 (n)
  (if (numberp n)
      n
      (safe-parse-number n
                         :fix-fn (lambda (a)
                                   (declare (ignore a))
                                   0.0))))

(defmethod draw-on-canvas :after ((object dot-plot) (destination canvas)
                                  &key
                                    (error-bar-color nil))
  "  Draw a scatter plot on a canvas.
The plot must be initialized with series (see: the 'all-series' slot of 'plot' class)

- object: a 'dot-plot' instance;
- destination a 'canvas' instance;
- error-bar-color: a string or named color like:  \"#FF0000\" or #%red%;

example:

  (with-nodgui ()
    (let ((canvas (make-canvas nil :width 800 :height 600))
          (plot   (make-instance dot-plot
                                 :all-series (list (make-dot-series :xs     '(10 20 30)
                                                                    :ys     '(20 40 60)
                                                                    :errors '(1.5 0.5 2.5)
                                                                    :legend \"data\"
                                                                    :color  #%red%)))))
      (grid canvas 0 0 :sticky :news)
      (draw-on-canvas plot canvas)))
"
  (with-accessors ((handle     handle)
                   (all-series all-series)) object
    (let ((all-error-handlers '()))
      (format-wish (tclize `(,handle legendconfig -legendtype rectangle)))
      (loop for series in all-series do
           (setf (series-handle series) (strcat "series_" (nodgui::create-name)))
           (let* ((size-as-num        (dot-series-size series))
                  (size               (nodgui::process-coords size-as-num))
                  (series-handle      (series-handle   series))
                  (xs                 (series-xs       series))
                  (ys                 (series-ys       series))
                  (errors             (or (dot-series-errors series)
                                          (make-fresh-list (length xs) 0.0)))
                  (color              (series-color    series))
                  (legend             (series-legend   series)))
             (format-wish (tclize `(,handle
                                    dotconfig
                                    ,series-handle " " -colour ,#[color ] " "
                                    -outline on)))
             (format-wish (tclize `(,handle
                                    dataconfig
                                    ,series-handle " " -colour ,#[color ])))
             (loop
                for x   in (split " " (nodgui::process-coords xs))
                for y   in (split " " (nodgui::process-coords ys))
                for err in (split " " (nodgui::process-coords errors)) do
                  (let ((*suppress-newline-for-tcl-statements* t)
                        (x-as-num           (parse-number-or-0 x))
                        (y-as-num           (parse-number-or-0 y))
                        (err-as-num         (parse-number-or-0 err)))
                    ;; errors
                    (when (not (epsilon= err-as-num 0.0))
                      (let ((error-handlers (draw-error-bar handle series-handle
                                                            x-as-num y-as-num err-as-num
                                                            (/ size-as-num 2)
                                                            error-bar-color)))
                        (setf all-error-handlers
                              (append all-error-handlers error-handlers))))
                    (format-wish (tclize `(,handle " " dot " "  ,series-handle " "
                                                   {+ ,x } {+ ,y }
                                                   ,size)))
                    (when (dot-series-callback series)
                      (bind-last object
                                 series
                                 (dot-series-bind-event series)
                                 (dot-series-callback series)))))
             (format-wish (tclize `(,handle
                                    legend
                                    ,series-handle " " ,#[legend ])))))
      ;; lower all errors bars
      (loop for error-handle in all-error-handlers do
           (nodgui::item-lower destination error-handle +plotchart-data-tag+))))
  object)

(defgeneric bind-last (object series event fn))

(defgeneric bind-series (object series event fn))

(defmethod bind-last ((object dot-plot) (series series) event fn)
  "Set the callback bound to the last added point"
  (let ((name (nodgui::create-name))
        (*suppress-newline-for-tcl-statements* t))
    (with-accessors ((handle handle)) object
      (nodgui::add-callback name fn)
      (format-wish (tclize `(,handle  " "
                                      bindlast ,(series-handle series) " "
                                      ,event        " "
                                      {sendeventplot ,name   " "
                                      ,(series-handle series) }))))))

(defmethod bind-series ((canvas canvas) (series series) event fn)
  "Set the callback bound to the last added point"
  (tagbind canvas
           (strcat +plotchart-data-tag+ "_" (series-handle series))
           event
           fn))

(defmethod bind ((object dot-plot) event fun &key append exclusive)
  "Bind fun to event of the plot (to bind the data see 'bind-data')"
  (declare (ignore append exclusive))
  (let ((name (nodgui::create-name))
        (*suppress-newline-for-tcl-statements* t))
    (with-accessors ((handle     handle)
                     (all-series all-series)) object
      (nodgui::add-callback name fun)
      (format-wish (tclize `(,handle                " "
                             bindplot ,event        " "
                             {sendeventplot ,name   " " ,handle })))
    object)))
