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

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(defun screen-width (&optional (w nil))
  "give the width of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenwidth ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-height (&optional (w nil))
  "give the height of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenheight ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-width-mm (&optional (w nil))
  "give the width of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmwidth ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-height-mm (&optional (w nil))
  "give the height of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmheight ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse-x (&optional (w nil))
  "give x position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointerx ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse-y (&optional (w nil))
  "give y position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointery ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse (&optional (w nil))
  "give the position of the mouse on screen as (x y) (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata \"([winfo pointerxy ~a])\"" (if w (widget-path w) "."))
  (let ((vals (read-data)))
    (values (first vals) (second vals))))

(defun toplevel-pathname (widget)
  (format-wish (tclize `(senddatastring [winfo toplevel ,(widget-path widget) ])))
  (read-data))

(defun children-pathname (widget)
  (format-wish (tclize `(senddatastring [winfo children ,(widget-path widget) ])))
  (split-words (read-data)))

(defun parent-pathname (widget)
  (format-wish (tclize `(senddatastring [winfo parent ,(widget-path widget) ])))
  (let ((parent (read-data)))
    (if (string= parent "")
        nil
        parent)))

(defun root-x (widget)
  (format-wish (tclize `(senddatastring [winfo rootx ,(widget-path widget) ])))
  (let ((x (read-data)))
    (if (string= x "")
        nil
        (parse-integer x))))

(defun root-y (widget)
  (format-wish (tclize `(senddatastring [winfo rooty ,(widget-path widget) ])))
  (let ((y (read-data)))
    (if (string= y "")
        nil
        (parse-integer y))))

(defun rootx (widget)
  (root-x widget))

(defun rooty (widget)
  (root-y widget))

(defun relative-x (widget)
  (format-wish (tclize `(senddatastring [winfo x ,(widget-path widget) ])))
  (let ((x (read-data)))
    (if (string= x "")
        nil
        (parse-integer x))))

(defun relative-y (widget)
  (format-wish (tclize `(senddatastring [winfo y ,(widget-path widget) ])))
  (let ((y (read-data)))
    (if (string= y "")
        nil
        (parse-integer y))))

(defun window-width (widget)
  (format-wish (tclize `(senddatastring [winfo width ,(widget-path widget) ])))
  (let ((w (read-data)))
    (if (string= w "")
        nil
        (parse-integer w))))

(defun window-height (widget)
  (format-wish (tclize `(senddatastring [winfo height ,(widget-path widget) ])))
  (let ((h (read-data)))
    (if (string= h "")
        nil
        (parse-integer h))))
