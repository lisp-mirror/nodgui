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

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defargs notebook ()
  class
  cursor
  style
  takefocus
  height
  padding
  width)

(defwrapper notebook (widget) () "ttk::notebook")

(defgeneric notebook-add (nb widget &rest options))

(defmethod notebook-add ((nb notebook) (w widget) &rest options)
  (format-wish "~a add ~a ~{ -~(~a~) {~a}~}" (widget-path nb) (widget-path w)
               (mapcar #'down options)))

(defgeneric notebook-tab (nb widget option value))

(defmethod notebook-tab ((nb notebook) (w widget) option value)
  (format-wish "~a tab ~a {-~a} {~a}"
               (widget-path nb)
               (widget-path w)
               (down option)
               (down value)))

(defgeneric notebook-forget (nb widget))

(defmethod notebook-forget ((nb notebook) (w widget))
  (format-wish "~a forget ~a" (widget-path nb) (widget-path w)))

(defgeneric notebook-hide (nb tab))

(defmethod notebook-hide ((nb notebook) (tab widget))
  (format-wish "~a hide ~a" (widget-path nb) (widget-path tab)))

(defgeneric notebook-identify (nb x y))

(defmethod notebook-identify ((nb notebook) x y)
  (format-wish "senddatastring [~a identify {~a} {~a}]" (widget-path nb) x y)
  (read-data))

(defgeneric notebook-index (nb tab))

(defmethod notebook-index ((nb notebook) (tab widget))
  (format-wish "senddata [~a index ~a]" (widget-path nb) (widget-path tab))
  (read-data))

(defgeneric notebook-select (nb tab))

(defmethod notebook-select ((nb notebook) (tab widget))
  (format-wish "~a select ~a" (widget-path nb) (widget-path tab)))

(defun notebook-events ()
  #$<<NotebookTabChanged>>$)

(defgeneric notebook-enable-traversal (nb))

(defmethod notebook-enable-traversal ((nb notebook))
  (format-wish (tclize `("ttk::notebook::enableTraversal " ,(widget-path nb)))))
