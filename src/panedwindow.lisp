;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui)

(defargs paned-window ()
  class
  cursor
  orient
  style
  takefocus
  width
  height)

(defwrapper paned-window (widget) () "ttk::panedwindow")

(defgeneric add-pane (window widget &rest options))

(defmethod add-pane ((pw paned-window) (w widget) &rest options)
  (format-wish "~a add ~a ~{ {-~(~a~)} {~a}~}"
               (widget-path pw)
               (widget-path w)
               (mapcar #'down options))
  pw)

(defgeneric forget-pane (window widget))

(defmethod forget-pane ((pw paned-window) (w widget))
  (format-wish "~a forget ~a" (widget-path pw) (widget-path w))
  pw)

(defgeneric sash-coord (window index))

(defmethod sash-coord ((pw paned-window) index)
  (with-read-data ()
    (format-wish "senddata \"([~a sashpos {~a}])\"" (widget-path pw) index)))

(defgeneric sash-place (window index pos))

(defmethod sash-place ((pw paned-window) index pos)
  (format-wish "~a sashpos {~a} {~a}" (widget-path pw) index pos))

(defgeneric panes (object))

(defmethod panes ((object paned-window))
  (with-read-data ()
    (format-wish "senddatastrings [~a panes]" (widget-path object))))

(defgeneric paned-widget-p (object widget))

(defmethod paned-widget-p ((object paned-window) (widget widget))
  (member (widget-path widget) (panes object) :test #'string=))
