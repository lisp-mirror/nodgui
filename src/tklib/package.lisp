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
        :alexandria
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:shadow :alexandria :rotate)
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
        :alexandria
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:shadow :alexandria :rotate)
  (:export
   :notify-window)
  (:documentation "Wrapper for notify-window in tklib
 (https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/notifywindow/notifywindow.html).

Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))

(defpackage :nodgui.tklib.plot
  (:use :cl
        :alexandria
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:shadow :alexandria :rotate)
  (:export
   :+plotchart-data-tag+
   :make-series
   :series-handle
   :series-xs
   :series-ys
   :series-legend
   :series-color
   :make-dot-series
   :dot-series-size
   :dot-series-errors
   :dot-series-bind-event
   :dot-series-callback
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
   :draw-on-canvas
   :xy-plot
   :dot-plot
   :bind-last
   :bind-series)
  (:documentation "Wrapper for a subset of plotchart in tklib
 (https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html#section14).

Tklib (https://core.tcl-lang.org/tklib/home) *must*  be installed on a
system for this wrapper to works"))
