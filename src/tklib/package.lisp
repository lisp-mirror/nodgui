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

(defpackage :nodgui.tklib.calendar
  (:use :cl
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:local-nicknames (:a  :alexandria))
  (:export
   :calendar
   :make-calendar
   :set-date
   :set-date*
   :parse-selected-date)
  (:documentation "Wrapper for Calendar in tklib
https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/widget/widget_calendar.html.

Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))

(defpackage :nodgui.tklib.notify
  (:use :cl
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:local-nicknames (:a  :alexandria))
  (:export
   :notify-window)
  (:documentation "Wrapper for notify-window in tklib
 (https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/notifywindow/notifywindow.html).

Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))

(defpackage :nodgui.tklib.plot
  (:use :cl
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:local-nicknames (:a  :alexandria))
  (:export
   :+plotchart-data-tag+
   :+comp-xyplot-title+
   :+comp-xyplot-subtitle+
   :+comp-xyplot-margin+
   :+comp-xyplot-text+
   :+comp-xyplot-legend+
   :+comp-xyplot-leftaxis+
   :+comp-xyplot-rightaxis+
   :+comp-xyplot-bottomaxis+
   :+comp-xyplot-background+
   :+comp-xyplot-mask+
   :series
   :handle
   :xs
   :ys
   :legend
   :color
   :dot-series
   :size
   :errors
   :bind-event
   :callback
   :bar-series
   :width
   :configure-plot-style
   :plot
   :handle
   :x-text
   :y-text
   :title
   :subtitle
   :all-series
   :make-axis-conf
   :axis-conf-min
   :axis-conf-max
   :axis-conf-step
   :axis-conf-tick-length
   :erase-plot
   :draw-on-canvas
   :xy-plot
   :dot-plot
   :place-line
   :bar-chart
   :x-labels
   :x-label-angle
   :bind-last
   :bind-series)
  (:documentation "Wrapper for a subset of plotchart in tklib
 (https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html#section14).

Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))

(defpackage :nodgui.tklib.swaplist
  (:use :cl
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:local-nicknames (:a  :alexandria))
  (:export
   :make-swaplist)
  (:documentation "Wrapper for swaplib in tklib
(https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/swaplist/swaplist.html)
Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))

(defpackage :nodgui.tklib.misc-widget
  (:use :cl
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:local-nicknames (:a  :alexandria))
  (:export
   :make-equalizer-bar)
  (:documentation "Wrapper for a subset of control widgets in tklib
(https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/controlwidget/controlwidget.html)
Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))
