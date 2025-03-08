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

(defmacro gen-wm-constant (name name-prefix)
  `(alexandria:define-constant ,(format-fn-symbol t "+~a-~a+" name-prefix name)
       ,(format nil "~(~a~)" name)
     :test #'string=))

(defmacro gen-wm-type-constants (&rest names)
  `(progn
     ,@(loop for name in names collect
            `(gen-wm-constant ,name wm-type))))

(gen-wm-type-constants
 desktop
 dock
 toolbar
 menu
 utility
 splash
 dialog
 dropdown_menu
 popup_menu
 tooltip
 notification
 combo
 dnd
 normal)

(defstruct wm-attrib
  (alpha       1.0)
  (fullscreen  0)
  (topmost     0)
  (type        +wm-type-normal+)
  (zoomed      0))

(defgeneric resizable (widget x y))

(defgeneric set-wm-overrideredirect (widget value))

(defgeneric wm-title (widget title))

(defgeneric wm-manage (widget))

(defgeneric wm-forget (widget))

(defgeneric wm-state (widget))

(defgeneric (setf wm-state) (new-state widget))

(defgeneric minsize (widget x y))

(defgeneric maxsize (widget x y))

(defgeneric withdraw (toplevel))

(defgeneric transient (toplevel master))

(defgeneric normalize (toplevel))

(defgeneric iconify (toplevel))

(defgeneric deiconify (toplevel))

(defgeneric geometry (toplevel))

(defgeneric (setf geometry) (geometry widget))

(defgeneric set-geometry (toplevel width height x y))

(defgeneric set-geometry-wh (toplevel width height))

(defgeneric set-geometry-xy (toplevel x y))

(defgeneric on-close (toplevel fun))

(defgeneric on-focus (toplevel fun))

(defgeneric icon-window (toplevel win-id))

(defgeneric iconwindow (toplevel win-id))

(defgeneric icon-photo (object photo))

(defgeneric set-wm-attrib (toplevel attributes))

(defmethod resizable ((tl widget) x y)
  (format-wish "wm resizable ~a {~a} {~a}" (widget-path tl) x y)
  tl)

(defmethod set-wm-overrideredirect ((w widget) val)
  (format-wish "wm overrideredirect ~a {~a}" (widget-path w) val)
  w)

(defmethod wm-title ((w widget) title)
  (format-wish "wm title ~a {~a}" (widget-path w) title)
  w)

(defmethod wm-manage ((w widget))
  (format-wish "wm manage ~a" (widget-path w))
  w)

(defmethod wm-forget ((w widget))
  (format-wish "wm forget ~a" (widget-path w))
  w)

(defmethod wm-state ((w widget))
  (with-read-data (read-wish)
    (format-wish "senddatastring [wm state ~a]" (widget-path w))))

(defmethod (setf wm-state) (new-state (w widget))
  (format-wish "wm state ~a {~a}" (widget-path w) new-state)
  new-state)

(defmethod minsize ((w widget) x y)
  (format-wish "wm minsize ~a ~a ~a" (widget-path w)
               (tk-number x) (tk-number y))
  w)

(defmethod maxsize ((w widget) x y)
  (format-wish "wm maxsize ~a ~a ~a" (widget-path w) (tk-number x) (tk-number y))
  w)

(defmethod withdraw ((tl widget))
  (format-wish "wm withdraw ~a" (widget-path tl))
  tl)

(defmethod transient ((toplevel widget) (master widget))
  (format-wish (tclize `(wm transient
                            ,(widget-path toplevel) " "
                            ,(widget-path master)))))

(defmethod transient ((toplevel widget) (master (eql nil)))
  (format-wish (tclize `(wm transient ,(widget-path toplevel) " "  \"+ \"))))

(defmethod normalize ((tl widget))
  (format-wish "wm state ~a normal" (widget-path tl))
  tl)

(defun %iconify (widget-path)
  (format-wish "wm iconify ~a" widget-path))

(defun %deiconify (widget-path)
  (format-wish "wm deiconify ~a" widget-path))

(defmethod iconify ((tl toplevel))
  (%iconify (widget-path tl))
  tl)

(defmethod iconify ((object (eql *tk*)))
  (%iconify (widget-path *tk*))
  *tk*)

(defmethod deiconify ((tl toplevel))
  (%deiconify (widget-path tl))
  tl)

(defmethod deiconify ((object (eql *tk*)))
  (%deiconify (widget-path *tk*))
  *tk*)

;; TODO use regex?
(defmethod geometry ((tl widget))
  (with-read-data (nil)
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
              (list width height x y))))))))

(defmethod (setf geometry) (geometry (tl widget))
  (format-wish "wm geometry ~a {~a}" (widget-path tl) geometry)
  geometry)

(defmethod set-geometry ((tl widget) width height x y)
  ;;(format-wish "wm geometry ~a ~ax~a+~a+~a" (widget-path tl) width height x y)
  (format-wish "wm geometry ~a ~ax~a+~D+~D" (widget-path tl)
               (tk-number width) (tk-number height) (tk-number x) (tk-number y))
  tl)

(defmethod set-geometry-wh ((tl widget) width height)
  (format-wish "wm geometry ~a ~ax~a" (widget-path tl)
               (tk-number width) (tk-number height))
  tl)

(defmethod set-geometry-xy ((tl widget) x y)
  (format-wish "wm geometry ~a +~D+~D" (widget-path tl) (tk-number x) (tk-number y))
  tl)

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

(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol WM_TAKE_FOCUS {callback ~A}"
              name))
  tl)

(defmethod iconwindow ((tl toplevel) win-id)
  (icon-window tl win-id))

(defmethod icon-window ((tl toplevel) win-id)
  (format-wish "wm iconwindow ~a ~a" (widget-path tl) (widget-path win-id))
  tl)

(defmethod icon-photo ((object widget) (photo photo-image))
  (format-wish (tclize `(wm iconphoto ,(widget-path object) " "
                            -default  ,(widget-path photo))))
  object)

(defmethod set-wm-attrib ((toplevel widget) (attributes wm-attrib))
  (set-wm-attrib (widget-path toplevel) attributes))

(defmethod set-wm-attrib ((toplevel string) (attributes wm-attrib))
  (let ((*add-space-after-emitted-string* t))
    (format-wish (tclize `(wm attributes
                              ,toplevel
                              -alpha      {+ ,(down (wm-attrib-alpha      attributes)) }
                              -fullscreen {+ ,(down (wm-attrib-fullscreen attributes)) }
                              -topmost    {+ ,(down (wm-attrib-topmost    attributes)) }
                              -type       {+ ,(down (wm-attrib-type       attributes)) }
                              -zoomed     {+ ,(down (wm-attrib-zoomed     attributes)) })))))
