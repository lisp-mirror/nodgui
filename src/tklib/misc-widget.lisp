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

(in-package :nodgui.tklib.misc-widget)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(define-constant +control-widget-library-name+ "controlwidget" :test #'string=)

(defclass control-widget (widget) ())

(defmethod initialize-instance :after ((object control-widget)
                                       &key &allow-other-keys)
  (require-tcl-package +control-widget-library-name+)
  (with-accessors ((widget-path widget-path)
                   (name        name)) object
    (with-no-emitted-newline
      (format-wish (tclize `(global ,name " ; set " ,name " " {+ })))))
  object)

(defmethod value ((object control-widget))
  (with-accessors ((widget-path widget-path)
                   (name        name)) object
    (with-no-emitted-newline
      (format-wish (tclize `(senddata ,widget-path " " get)))
      (read-data))))

(defmethod (setf value) (new-value (object control-widget))
  (with-accessors ((widget-path widget-path)
                   (name        name)) object
    (with-no-emitted-newline
      (format-wish (tclize `(,widget-path " set " { ,(process-coords new-value) }))))))

(defclass equalizer-bar (control-widget) ())

(defmethod initialize-instance :after ((object equalizer-bar)
                                       &key
                                         (from               0.0)
                                         (to                 1.0)
                                         (number             1)
                                         (background         #%white%)
                                         (height             200)
                                         (width              100)
                                         (bar-width          20)
                                         (segments           5)
                                         (safe-color         #%green%)
                                         (warning-color      #%red%)
                                         (warning-level      .8)
                                         &allow-other-keys)
  (with-accessors ((widget-path widget-path)
                   (name        name)) object
    (with-no-emitted-newline
      (format-wish (tclize `("::controlwidget::equalizerBar "
                             ,widget-path  " "
                             -variable
                             ,name         " "
                             -number        ,(process-coords number)           " "
                             -from          ,(process-coords from)             " "
                             -to            ,(process-coords to)               " "
                             -background    {+ ,background        }            " "
                             -height        ,(process-coords height)           " "
                             -width         ,(process-coords width)            " "
                             -barwidth      ,(process-coords bar-width)        " "
                             -segments      ,(process-coords segments)         " "
                             -safecolor     {+ ,safe-color     }
                             -warningcolor  {+ ,warning-color  }
                             -warninglevel  ,(process-coords warning-level)    " ")))))
  object)

(defun make-equalizer-bar (&key
                             (from               0.0)
                             (to                 1.0)
                             (number             1)
                             (background         #%white%)
                             (height             200)
                             (width              100)
                             (bar-width          20)
                             (segments           5)
                             (safe-color         #%green%)
                             (warning-color      #%red%)
                             (warning-level      .8))
  "Make a widget of vertical bars composed by horizontal segments"
  (make-instance 'equalizer-bar
                 :from          from
                 :to            to
                 :number        number
                 :background    background
                 :height        height
                 :width         width
                 :bar-width     bar-width
                 :segments      segments
                 :safe-color    safe-color
                 :warning-color warning-color
                 :warning-level warning-level))
