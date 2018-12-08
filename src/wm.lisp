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

(defgeneric resizable (widget x y))

(defmethod resizable ((tl widget) x y)
  (format-wish "wm resizable ~a ~a ~a" (widget-path tl) x y)
  tl)

(defgeneric set-wm-overrideredirect (widget value))

(defmethod set-wm-overrideredirect ((w widget) val)
  (format-wish "wm overrideredirect ~a ~a" (widget-path w) val)
  w)

(defgeneric wm-title (widget title))

(defmethod wm-title ((w widget) title)
  (format-wish "wm title ~a {~a}" (widget-path w) title)
  w)

(defgeneric wm-manage (widget))

(defmethod wm-manage ((w widget))
  (format-wish "wm manage ~a" (widget-path w))
  w)

(defgeneric wm-forget (widget))

(defmethod wm-forget ((w widget))
  (format-wish "wm forget ~a" (widget-path w))
  w)

(defgeneric wm-state (widget))

(defmethod wm-state ((w widget))
  (format-wish "senddatastring [wm state ~a]" (widget-path w))
  (read-wish))

(defgeneric (setf wm-state) (new-state widget))

(defmethod (setf wm-state) (new-state (w widget))
  (format-wish "wm state ~a ~a" (widget-path w) new-state)
  new-state)

(defgeneric minsize (widget x y))

(defmethod minsize ((w widget) x y)
  (format-wish "wm minsize ~a ~a ~a" (widget-path w)
               (tk-number x) (tk-number y))
  w)

(defgeneric maxsize (widget x y))

(defmethod maxsize ((w widget) x y)
  (format-wish "wm maxsize ~a ~a ~a" (widget-path w) (tk-number x) (tk-number y))
  w)

(defgeneric withdraw (toplevel))

(defmethod withdraw ((tl widget))
  (format-wish "wm withdraw ~a" (widget-path tl))
  tl)

(defgeneric normalize (toplevel))

(defmethod normalize ((tl widget))
  (format-wish "wm state ~a normal" (widget-path tl))
  tl)

(defgeneric iconify (toplevel))

(defmethod iconify ((tl toplevel))
  (format-wish "wm iconify ~a" (widget-path tl))
  tl)

(defgeneric deiconify (toplevel))

(defmethod deiconify ((tl toplevel))
  (format-wish "wm deiconify ~a" (widget-path tl))
  tl)

(defgeneric geometry (toplevel))

;; TODO use regex?
(defmethod geometry ((tl widget))
  (format-wish "senddatastring [wm geometry ~a]" (widget-path tl))
  (let ((str (read-data)))
    (multiple-value-bind (width letterx) (parse-integer str :junk-allowed t)
      (assert (char= (char str letterx) #\x))
      (multiple-value-bind (height letter+)
          (parse-integer str :start (1+ letterx) :junk-allowed t)
        (assert (or (char= (char str letter+) #\+)
                    (char= (char str letter+) #\-)))
        (multiple-value-bind (x letter+)
            (parse-integer str :start (1+ letter+) :junk-allowed t)
          (assert (or (char= (char str letter+) #\+)
                      (char= (char str letter+) #\-)))
          (let ((y (parse-integer str :start (1+ letter+))))
            (when (char= (char str letter+) #\-)
              (let ((sh (screen-height tl)))
                (setf y (- sh y))))
            (list width height x y)))))))

(defgeneric (setf geometry) (geometry widget))

(defmethod (setf geometry) (geometry (tl widget))
  (format-wish "wm geometry ~a ~a" (widget-path tl) geometry)
  geometry)

(defgeneric set-geometry (toplevel width height x y))

(defmethod set-geometry ((tl widget) width height x y)
  ;;(format-wish "wm geometry ~a ~ax~a+~a+~a" (widget-path tl) width height x y)
  (format-wish "wm geometry ~a ~ax~a+~D+~D" (widget-path tl)
               (tk-number width) (tk-number height) (tk-number x) (tk-number y))
  tl)

(defgeneric set-geometry-wh (toplevel width height))

(defmethod set-geometry-wh ((tl widget) width height)
  (format-wish "wm geometry ~a ~ax~a" (widget-path tl)
               (tk-number width) (tk-number height))
  tl)

(defgeneric set-geometry-xy (toplevel x y))

(defmethod set-geometry-xy ((tl widget) x y)
  (format-wish "wm geometry ~a +~D+~D" (widget-path tl) (tk-number x) (tk-number y))
  tl)

(defgeneric on-close (toplevel fun))

(defmethod on-close ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (widget-path tl) name))
  tl)

(defmethod on-close ((tl widget) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (widget-path tl) name))
  tl)

(defmethod on-close ((tl (eql *tk*)) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol . WM_DELETE_WINDOW {callback ~A}" name)
    tl))

(defgeneric on-focus (toplevel fun))

(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol WM_TAKE_FOCUS {callback ~A}"
              name))
  tl)

(defun iconwindow (tl wid)
  (format-wish "wm iconwindow ~a ~a" (widget-path tl) (widget-path wid))
  tl)
