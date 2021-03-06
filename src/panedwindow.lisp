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

(defgeneric pane-configure (l i &rest options))

(defmethod pane-configure ((p paned-window) (w widget)  &rest options)
  (format-wish "~a paneconfigure ~a ~{ {-~(~a~)} {~a}~}"
               (widget-path p)
               (widget-path w)
               (mapcar #'down options))
  p)
