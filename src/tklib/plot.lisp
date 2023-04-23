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

(define-constant +plotchart-library-name+    "Plotchart" :test #'string=)

(define-constant +plotchart-data-tag+        "data"      :test #'string=)

(define-constant +pseudo-series-item-suffix+ "-series"   :test #'string=)

(defmacro gen-plot-component (type &rest values)
  `(progn
     ,@(loop for value in values collect
            `(define-constant ,(format-fn-symbol t "+comp-~a-~a+" type value)
                 ,(format nil " ~a " (string-downcase value))
              :test #'string=))))

(gen-plot-component xyplot
                    title subtitle margin
                    text legend
                    leftaxis rightaxis bottomaxis
                    background mask)

(defclass series ()
  ((handle
    :initform nil
    :initarg  :handle
    :accessor handle
    :documentation  "the internal  identifier of  this series  (do not
    touch it!)")
  (xs
   :initform '()
   :initarg  :xs
   :accessor xs
   :documentation "a list of x for each point of this series")
  (ys
   :initform '()
   :initarg  :ys
   :accessor ys
   :documentation "a list of y for each point of this series")
  (legend
   :initform ""
   :initarg  :legend
   :accessor legend
   :documentation "a  descriptive label (shown  on the plot)  for this
   series")
  (color
   :initform "#ff0000"
   :initarg  :color
   :accessor color
   :documentation "plotted color of each datum"))
  (:documentation "keeps a set of data"))

(defclass event-responsive-series ()
  ((bind-event
    :initform #$<ButtonPress-1>$
    :initarg  :bind-event
    :accessor bind-event
    :documentation "the event a point of a series is reactve to")
   (callback
    :initform nil
    :initarg  :callback
    :accessor callback
    :documentation  "the function  called (with  an 'event'  struct as
    parameter) when a bind-event is fired."))
  (:documentation "A series  that respond to user  events (e.g. ckick
   with a mouse button)"))

(defclass dot-series (series event-responsive-series)
  ((size
    :initform 3.0
    :initarg  :size
    :accessor size
    :documentation "radius of the point")
   (value-symbol
    :initform :dot
    :initarg  :value-symbol
    :accessor value-symbol
    :documentation "The symbol drawn for each point of this series, allowed values are:
                   :plus,  :cross, :circle,  :up :down  :dot :upfilled
                   :downfilled. Default is :dot")
   (errors
    :initform '()
    :initarg  :errors
    :accessor errors
    :documentation "a list of statistical  error for each point of the
    series"))
  (:documentation  "represents  a  series  of  point  for  a  scatter
   plot (see: 'series')"))

(defclass bar-series (series) ()
  (:documentation "A series of data displayed in bar form"))

(defclass series-holder ()
  ((all-series
    :initform '()
    :initarg  :all-series
    :accessor all-series
    :documentation "The  data of  this plot, must  be instance  of the
    struct 'series' or derived")))

(defclass title-holder ()
  ((title
    :initform "plot title"
    :initarg  :title
    :accessor title
    :documentation "A descriptive text of the plot")))

(defclass handle-holder ()
  ((handle
    :initform nil
    :accessor handle
    :documentation "The internal identifier of this series (do not touch it!)")))

(defclass plot (series-holder title-holder handle-holder)
  ((x-text
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
    :documentation "More descriptive text of the plot")))

(defclass axis-conf ()
  ((minimum
    :initform      0.0
    :initarg       :minimum
    :accessor      minimum
    :documentation "minimum value for the axis")
   (maximum
    :initform 1.0
    :initarg  :maximum
    :accessor maximum
    :documentation "maximum value for the axis")
   (ticks-step
    :initform 0.1
    :initarg  :ticks-step
    :accessor ticks-step
    :documentation "Tick's step")
   (tick-length
    :initform 5
    :initarg  :tick-length
    :accessor tick-length
    :documentation "length of the axis tick"))
   (:documentation "represents an axis"))

(defun send-wish-preamble-code ()
  (require-tcl-package +plotchart-library-name+)
  (nodgui::send-wish
   (defproc sendeventplot (x y fun ({+ other {+ }+ }))
     (sendevent $fun
                $x " "
                $y
                {?}
                {?}
                {?}
                {?}
                {?}
                {?}
                {?}
                {?}
                {?}))))

(defmethod initialize-instance :after ((object plot) &key &allow-other-keys)
  (send-wish-preamble-code))

(defgeneric draw-on-canvas (object destination &key &allow-other-keys))

(defgeneric erase-plot (object))

(defmethod erase-plot ((object plot))
  (with-accessors ((handle handle)) object
    (format-wish (tclize `("::Plotchart::eraseplot " ,handle)))))

(defclass xy-plot (plot)
  ((x-axis-conf
    :initform (make-instance 'axis-conf :minimum 0.0 :maximum 100.0 :ticks-step 10.0)
    :initarg  :x-axis-conf
    :accessor x-axis-conf)
   (y-axis-conf
    :initform (make-instance 'axis-conf :minimum 0.0 :maximum 100.0 :ticks-step 10.0)
    :initarg  :y-axis-conf
    :accessor y-axis-conf))
  (:documentation "A plot with the data representing points in the x/y plane."))

(defun configure-plot-style (type component key value)
  "Configure various parts of the plot.

This values must be configured before the plot is drawn.

example: (configure-plot-style 'xyplot +comp-xyplot-bottomaxis+ 'ticklength 10)

- type:      'xyplot
- component: one of:
   - +comp-xyplot-title+
   - +comp-xyplot-subtitle+
   - +comp-xyplot-margin+
   - +comp-xyplot-text+
   - +comp-xyplot-legend+
   - +comp-xyplot-leftaxis+
   - +comp-xyplot-rightaxis+
   - +comp-xyplot-bottomaxis+
   - +comp-xyplot-background+
   - +comp-xyplot-mask+

- key: 'ticklength, 'color ... please see the original documentation of tklib:
  https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html#146

- value: dependent from value stored in 'key'

"
  (format-wish (tclize `("::Plotchart::plotstyle "
                         configure default
                         ,type ,component ,key ,value))))

(defmethod draw-on-canvas ((object xy-plot) (destination canvas) &key &allow-other-keys)
  (with-read-data (nil)
    (with-accessors ((handle handle)
                     (x-axis-conf x-axis-conf)
                     (y-axis-conf y-axis-conf)
                     (x-text      x-text)
                     (y-text      y-text)
                     (x-subtext   x-subtext)
                     (y-subtext   y-subtext)
                     (title       title)
                     (subtitle    subtitle)) object
      (let* ((x-min         (minimum     x-axis-conf))
             (x-max         (maximum     x-axis-conf))
             (x-step        (ticks-step  x-axis-conf))
             (x-tick-length (tick-length x-axis-conf))
             (y-min         (minimum     y-axis-conf))
             (y-max         (maximum     y-axis-conf))
             (y-step        (ticks-step  y-axis-conf))
             (y-tick-length (tick-length y-axis-conf))
             (*suppress-newline-for-tcl-statements*             t)
             (*add-space-after-emitted-unspecialized-element*   nil))
        ;; create
        (when x-tick-length
          (configure-plot-style 'xyplot +comp-xyplot-bottomaxis+ 'ticklength x-tick-length))
        (when y-tick-length
          (configure-plot-style 'xyplot +comp-xyplot-leftaxis+   'ticklength y-tick-length))
        (format-wish (tclize `(senddata
                               ["::Plotchart::createXYPlot"      " "
                               {+ ,(widget-path destination)}  " "
                               [list ,x-min " " ,x-max " " ,x-step ]
                               [list ,y-min " " ,y-max " " ,y-step ]])))
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
        object))))

(defclass dot-plot (xy-plot)
  ()
  (:documentation "A scatter plot, see: https://en.wikipedia.org/wiki/Scatter_plot"))

(defgeneric place-line (object series x1 y1 x2 y2 color &key width))

(defmethod place-line ((plot dot-plot) (series series) x1 y1 x2 y2 color &key (width 1))
  "Draw a  line from (x1, y1)  to (x2 y2) with  specified color. Units
are in plot space"
  (with-read-data ()
    (with-accessors ((plot-handle handle)) plot
      (let* ((series-handle      (handle series))
             (pseudo-series-item (strcat series-handle +pseudo-series-item-suffix+)))
        (with-no-emitted-newline
          (format-wish (tclize `(senddata [,plot-handle
                                          object
                                          line " " ,pseudo-series-item " "
                                          ,x1 " " ,y1   " "
                                          ,x2 " " ,y2   " "
                                          ,(empty-string-if-nil color
                                                                `(-fill  {+ ,color })) " "
                                          -width ,width
                                          ]))))))))


(defun draw-error-bar (plot series x y error-value stopper-width color)
  "The low level drawing procedure for error bar"
  (let ((start-bar     (- y error-value))
        (end-bar       (+ y error-value))
        (start-stopper (- x stopper-width))
        (stop-stopper  (+ x stopper-width)))
    (flet ((draw-line (x-start y-start x-end y-end)
             (place-line plot series x-start y-start x-end y-end color)))
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
                                    (error-bar-color "#000000"))
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
           (assert (member (value-symbol series)
                           '(:plus  :cross :circle
                             :up    :down  :dot
                             :upfilled :downfilled)))
           (setf (handle series) (strcat "series_" (create-name)))
           (let* ((size-as-num        (size series))
                  (size               (nodgui::process-coords size-as-num))
                  (displayed-symbol   (keyword->tcl (value-symbol series)
                                                    :downcase t))
                  (series-handle      (handle       series))
                  (xs                 (xs           series))
                  (ys                 (ys           series))
                  (errors             (or (errors series)
                                          (make-fresh-list (length xs) 0.0)))
                  (color              (color    series))
                  (legend             (legend   series)))
             (let ((*suppress-newline-for-tcl-statements* t))
               ;; note:  for  some  reasons seems  that  the  `radius'
               ;; option is  passed to canvas  item and, if  radius is
               ;; not a valid option for that item, a crash occurs
               (format-wish (tclize `(,handle
                                      dataconfig
                                      ,series-handle             " "
                                      -colour ,color             " "
                                      -type   symbol             " "
                                      -symbol ,displayed-symbol  " "
                                      -radius ,size))))
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
                      (let* ((x-axis          (x-axis-conf object))
                             (x-axis-w        (abs (- (maximum x-axis)
                                                      (minimum x-axis))))
                             (error-bar-width (* (/ size-as-num 2)
                                                 (* 0.01 x-axis-w)))
                             (error-handlers  (draw-error-bar object
                                                              series
                                                              x-as-num
                                                              y-as-num
                                                              err-as-num
                                                              error-bar-width
                                                              error-bar-color)))
                        (setf all-error-handlers
                              (append all-error-handlers error-handlers))))
                    (format-wish (tclize `(,handle " " plot " "  ,series-handle " "
                                                   {+ ,x } {+ ,y })))

                    (when (callback series)
                      (bind-last object
                                 series
                                 (bind-event series)
                                 (callback series)))))
             (when (not (string-empty-p legend))
               (format-wish (tclize `(,handle
                                      legend
                                      ,series-handle " " ,legend))))))
      ;; lower all errors bars
      (loop for error-handle in all-error-handlers do
           (nodgui::item-lower destination error-handle +plotchart-data-tag+))))
  object)

(defgeneric bind-last (object series event fn))

(defgeneric bind-series (object series event fn))

(defmethod bind-last (object (series event-responsive-series) event fn)
  "Set the callback bound to the last added point"
  (let ((name (create-name))
        (*suppress-newline-for-tcl-statements* t))
    (with-accessors ((handle handle)) object
      (nodgui::add-callback name fn)
      (format-wish (tclize `(,handle  " "
                                      bindlast ,(handle series) " "
                                      ,event        " "
                                      {sendeventplot ,name   " "
                                      ,(handle series) }))))))

(defmethod bind-series ((canvas canvas) (series event-responsive-series) event fn)
  "Set the callback bound to the last added point"
  (tagbind canvas
           (strcat +plotchart-data-tag+ "_" (handle series))
           event
           fn))

(defmethod bind ((object dot-plot) event fun &key append exclusive)
  "Bind fun to event of the plot (to bind the data see 'bind-data')"
  (declare (ignore append exclusive))
  (let ((name (create-name))
        (*suppress-newline-for-tcl-statements* t))
    (with-accessors ((handle     handle)
                     (all-series all-series)) object
      (nodgui::add-callback name fun)
      (format-wish (tclize `(,handle                " "
                             bindplot ,event        " "
                             {sendeventplot ,name   " " ,handle })))
    object)))

(defclass bar-chart (series-holder title-holder handle-holder)
  ((x-labels
    :initform '()
    :initarg  :x-labels
    :accessor x-labels)
   (y-axis-conf
    :initform (make-instance 'axis-conf :minimum 0.0 :maximum 100.0 :ticks-step 10.0)
    :initarg  :y-axis-conf
    :accessor y-axis-conf)
   (x-label-angle
    :initform 0
    :initarg  :x-label-angle
    :accessor x-label-angle)))

(defmethod initialize-instance :after ((object bar-chart) &key &allow-other-keys)
  (send-wish-preamble-code))

(define-condition plot-error (simple-error) ())

(defun check-barchart-data (plot)
  (with-accessors ((handle handle)
                   (y-axis-conf y-axis-conf)
                   (x-labels    x-labels)
                   (title       title)
                   (all-series  all-series)) plot
    (let ((max-num-data (apply #'max
                               (mapcar (lambda (a) (length (ys a)))
                                       all-series))))
      (when (< (length x-labels)
               max-num-data)
        (error 'plot-error
               :format-control (strcat "Barchart error: you are providing a number "
                                       "of x labels that is less of the maxmum number of "
                                       "data in all the series. "
                                       "If you are providing two labes at least a single series "
                                       "must provide two values in its 'ys' slot"))))))

(defmethod draw-on-canvas ((object bar-chart) (destination canvas) &key &allow-other-keys)
  (with-read-data (nil)
    (with-accessors ((handle handle)
                     (y-axis-conf   y-axis-conf)
                     (x-labels      x-labels)
                     (x-label-angle x-label-angle)
                     (title         title)
                     (all-series    all-series)) object
      (check-barchart-data object)
      (let* ((y-min           (minimum    y-axis-conf))
             (y-max           (maximum    y-axis-conf))
             (y-step          (ticks-step y-axis-conf))
             (actual-x-labels (mapcar (lambda (a) (strcat a " ")) x-labels))
             (*suppress-newline-for-tcl-statements*             t)
             (*add-space-after-emitted-unspecialized-element*   nil))
        (format-wish (tclize `(senddata ["::Plotchart::createBarchart"     " "
                                        {+,(widget-path destination) }     " "
                                        {+ ,@(loop for label in actual-x-labels collect
                                                   `(\"+ ,label \"))
                                        }
                                        { ,y-min " " ,y-max " " ,y-step }
                                        {+ ,(length all-series) }
                                        -xlabelangle ,(process-coords x-label-angle)
                                        ])))
        ;; get the handle (used below)
        (setf handle (read-data))
        ;; add title
        (format-wish (tclize `(,handle title {+ ,title })))
        (loop for series in all-series do
          (setf (handle series) (strcat "series_" (create-name)))
          (let ((*suppress-newline-for-tcl-statements* t)
                (ys (process-coords (ys series))))
            (format-wish (tclize `(,handle " "
                                           plot " "  ,(handle series) " "
                                           {+ ,ys }
                                           ,(color series))))
            ;; even  if  the  doc  says  that  bindlast  is  a  valid
            ;; subcommand it does not seems  to work here, where am i
            ;; wrong?
            ;; (when (callback series)
            ;;   (bind-last object
            ;;              series
            ;;              (bind-event series)
            ;;              (callback series)))
            (format-wish (tclize `(,handle
                                   legend
                                   ,(handle series) " "
                                   ,(legend series))))))
        object))))
