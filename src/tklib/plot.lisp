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

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(define-constant +plotchart-library-name+ "Plotchart" :test #'string=)

(defstruct series
  handle xs ys legend
  (color "#ff0000"))

(defstruct (dot-series
             (:include series))
  (size  3.0))

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

(defgeneric draw-on-canvas (object destination))

(defclass xy-plot (plot)
  ((x-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :x-axis-conf
    :accessor x-axis-conf)
   (y-axis-conf
    :initform (make-axis-conf :min 0.0 :max 100.0 :step 10.0)
    :initarg  :y-axis-conf
    :accessor y-axis-conf)))

(defmethod draw-on-canvas ((object xy-plot) (destination canvas))
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

(defmethod draw-on-canvas :after ((object dot-plot) (destination canvas))
  (with-accessors ((handle     handle)
                   (all-series all-series)) object
    (format-wish (tclize `(senddata [ ,handle legendconfig -legendtype rectangle])))
    (loop for series in all-series do
         (setf (series-handle series) (strcat "series_" (nodgui::create-name)))
         (let ((size          (dot-series-size series))
               (series-handle (series-handle   series))
               (xs            (series-xs       series))
               (ys            (series-ys       series))
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
              for x in (split " " (nodgui::process-coords xs))
              for y in (split " " (nodgui::process-coords ys)) do
                (let ((*suppress-newline-for-tcl-statements* t))
                  (format-wish (tclize `(senddata [ ,handle " " dot " "  ,series-handle " "
                                                  {+ ,x } {+ ,y }
                                                  ,size ])))))
           (format-wish (tclize `(senddata [ ,handle
                                           legend
                                           ,series-handle " " ,#[legend ]]))))))
  object)
