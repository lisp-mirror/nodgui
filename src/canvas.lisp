;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui)

(cl-syntax:use-syntax nodgui-force-escape-syntax)

(cl-syntax:use-syntax nodgui-color-syntax)

(defargs canvas ()
  background
  borderwidth
  closeenough
  confine
  cursor
  height
  highlightbackground
  highlightcolor
  highlightthickness
  insertbackground
  insertborderwidth
  insertofftime
  insertontime
  insertwidth
  offset
  relief
  scrollregion
  selectbackground
  selectborderwidth
  selectforeground
  state
  takefocus
  width
  xscrollcommand
  xscrollincrement
  yscrollcommand
  yscrollincrement)

(defwrapper canvas (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (scrollregion-x0 :accessor scrollregion-x0 :initform nil)
   (scrollregion-y0 :accessor scrollregion-y0 :initform nil)
   (scrollregion-x1 :accessor scrollregion-x1 :initform nil)
   (scrollregion-y1 :accessor scrollregion-y1 :initform nil))
  "canvas")

(defclass canvas-holder ()
  ((canvas
    :initform nil
    :initarg  :canvas
    :accessor canvas)))

;; wrapper class for canvas items
(defclass canvas-item (canvas-holder)
  ((handle
    :accessor handle
    :initarg  :handle)))

(defmethod print-object ((self canvas-item) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (when (slot-boundp self 'handle)
      (format stream "~a" (handle self)))))

(defmethod canvas ((canvas canvas)) canvas)

(defun make-canvas (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'canvas
                 :master  master
                 :width   width
                 :height  height
                 :xscroll xscroll
                 :yscroll yscroll))

(defgeneric scale (canvas factor &optional factory))

(defgeneric bbox (item))

(defgeneric calc-scroll-region (canvas))

(defgeneric set-coords* (canvas item &rest coords))

(defgeneric (setf coords) (val item))

(defgeneric set-coords (canvas item coords))

(defgeneric coords (item))

(defgeneric scrollregion (canvas x0 y0 x1 y1))

(defgeneric canvasx (canvas screenx))

(defgeneric canvasy (canvas screeny))

(defgeneric itembind (canvas w event fun))

(defgeneric itemmove (canvas item dx dy))

(defgeneric itemdelete (canvas item))

(defgeneric itemconfigure (widget item option value))

(defgeneric itemlower (w i &optional below))

(defgeneric itemraise (w i &optional above))

(defgeneric item-bind (canvas w event fun))

(defgeneric item-move (canvas item dx dy))

(defgeneric item-delete (canvas item))

(defgeneric item-configure (widget item option value))

(defgeneric item-lower (widget item &optional below))

(defgeneric item-raise (widget item &optional above))

(defgeneric item-move-to (object item-handle x y))

(defgeneric tagbind  (canvas tag event fun &key exclusive))

(defgeneric move-to (object x y))

(defgeneric move (item dx dy))

(defgeneric clear (widget))

(defmethod scale ((canvas canvas) factor &optional factory)
  (format-wish "~a scale all 0 0 {~a} {~a}"
               (widget-path canvas) factor (or factory factor))
  canvas)

(defun move-all (canvas dx dy)
  (format-wish "~a move all {~a} {~a}" (widget-path canvas) dx dy)
  canvas)

(defmethod bbox ((item canvas-item))
  (canvas-bbox (canvas item) (handle item)))

(defmethod bbox ((canvas canvas))
  (format-wish "senddata \"([~a bbox all])\"" (widget-path canvas))
  (read-data))

(defun canvas-bbox (canvas handle)
  (warn "Canvas bbox is a misleading name: use \"canvas-item-bbox\" instead")
  (multiple-value-bind (aabb raw-data)
      (canvas-item-bbox canvas handle)
    (declare (ignore aabb))
    raw-data))

(defmacro with-canvas-path ((path canvas) &body body)
  `(with-accessors ((,path widget-path)) ,canvas
     ,@body))

(defparameter  *bbox-scale-fix* 1.0
  "According to the  tk documentation the calculated aabb  for a shape
  may overstemite  the boudaries  by \"a  few pixels\".  This variable
  scale the bounding box.")

(defun bbox-min-x (aabb)
  (elt aabb 0))

(defun bbox-max-x (aabb)
  (elt aabb 2))

(defun bbox-min-y (aabb)
  (elt aabb 1))

(defun bbox-max-y (aabb)
  (elt aabb 3))

(defun canvas-item-bbox (canvas handle)
  (format-wish "senddata \"([~a bbox {~a}])\"" (widget-path canvas) handle)
  (let ((bbox (read-data)))
    (if (epsilon= *bbox-scale-fix* 1.0)
        bbox
        (mapcar (lambda (a) (round (* a *bbox-scale-fix*))) bbox))))

(defmethod calc-scroll-region ((canvas canvas))
  (format-wish "~a configure -scrollregion [~a bbox all]"
               (widget-path canvas)
               (widget-path canvas))
  canvas)

(defmethod set-coords (canvas item coords)
  (format-wish "~a coords {~a}~{ {~a}~}" (widget-path canvas) item coords)
  canvas)

(defmethod set-coords ((canvas canvas) (item canvas-item) (coords list))
  (set-coords canvas (handle item) coords))

(defmethod set-coords* (canvas item &rest coords)
  (funcall #'set-coords canvas item coords))

(defmethod set-coords* ((canvas canvas) (item canvas-item) &rest coords)
  (funcall #'set-coords canvas (handle item) coords))

(defmethod coords ((item canvas-item))
  (error "not implemented, patches welcome! :)"))

(defun format-number (stream number)
  (cond
   ((complexp number)
    (format-number stream (realpart number))
    (format-number stream (imagpart number)))
   ((integerp number)
    (format stream " ~d" number))
   ((typep number 'single-float)
    (format stream " ~a" number))
   ((numberp number)
    (format-number stream (coerce number 'single-float)))
   ((null number)
    )
   ((listp number)
    (format-number stream (car number))
    (format-number stream (cdr number)))
   ((arrayp number)
    (dotimes (i (length number))
      (format-number stream (aref number i))))))

(defun process-coords (input)
  (with-output-to-string (s)
    (format-number s input)))

(defmethod (setf coords) (val (item canvas-item))
  (let ((coord-list (process-coords val)))
    (set-coords (canvas item) (handle item) (cl-ppcre:split " " (trim coord-list)))
    coord-list))

(defmethod itembind ((canvas canvas) (item canvas-item) event fun)
  (item-bind canvas item event fun))

(defmethod item-bind ((canvas canvas) (item canvas-item) event fun)
  (item-bind canvas (handle item) event fun))

(defmethod itembind ((canvas canvas) (item integer) event fun)
  "bind fun to event of the widget w"
  (item-bind canvas item event fun))

(defmethod item-bind ((canvas canvas) (item integer) event fun)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a bind ~a {~a} {sendevent ~a %x %y %N %k %K %w %h %X %Y %b}"
                 (widget-path canvas) item event name))
  canvas)

(defmethod tagbind ((canvas canvas) tag event fun &key exclusive)
  "bind fun to event of the widget w"
  (tag-bind canvas tag event fun :exclusive exclusive))

(defmethod tag-bind ((canvas canvas) tag event fun &key exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a bind {~(~a~)} {~a} {sendevent ~a %x %y %N %k %K %w %h %X %Y %b ~:[~;;break~]}"
                 (widget-path canvas) tag event name exclusive))
  canvas)

(defmethod bind ((w canvas-item) event fun &key append exclusive)
  (declare (ignore append exclusive))
  (itembind (canvas w) (handle w) event fun))

(defmethod tcl-bind ((w canvas-item) event code &key append exclusive)
  (declare (ignore append exclusive))
  (format-wish "~a bind ~a {~a} {~a}"
               (widget-path (canvas w))
               (handle w)
               event
               code))

(defmethod scrollregion ((c canvas) x0 y0 x1 y1)
  (setf (scrollregion-x0 c) (tk-number x0))
  (setf (scrollregion-y0 c) (tk-number y0))
  (setf (scrollregion-x1 c) (tk-number x1))
  (setf (scrollregion-y1 c) (tk-number y1))
  (configure c :scrollregion (format nil "~a ~a ~a ~a" (tk-number x0) (tk-number y0) (tk-number x1) (tk-number y1)))
  c)

(defmethod canvasx ((canvas canvas) screenx)
  (format-wish "senddata [~a canvasx ~a]" (widget-path canvas) (tk-number screenx))
  (read-data))

(defmethod canvasy ((canvas canvas) screeny)
  (format-wish "senddata [~a canvasy ~a]" (widget-path canvas) (tk-number screeny))
  (read-data))

(defmethod itemmove ((canvas canvas) (item integer) dx dy)
  (item-move canvas item dx dy))

(defmethod item-move ((canvas canvas) (item integer) dx dy)
  (format-wish "~a move ~a ~a ~a" (widget-path canvas) item (tk-number dx) (tk-number dy))
  canvas)

(defmethod itemmove ((canvas canvas) (item canvas-item) dx dy)
  (item-move canvas item dx dy))

(defmethod itemmove ((canvas canvas) (item canvas-item) dx dy)
  (item-move (canvas item) (handle item) (tk-number dx) (tk-number dy)))

(defmethod itemmove ((canvas canvas) item dx dy)
  (item-move item dx dy))

(defmethod item-move ((canvas canvas) item dx dy)
  (format-wish "~a move {~a} ~a ~a"
               (widget-path canvas)
               (down item)
               (tk-number dx)
               (tk-number dy)))

(defmethod item-move-to ((object canvas) (item-handle integer) x y)
  (with-canvas-path (path object)
    (let ((tk-x (tk-number x))
          (tk-y (tk-number y)))
      (let ((*add-space-after-emitted-string* t))
        (format-wish (tclize `(,path moveto
                                     ,#[down item-handle ] " "
                                     ,tk-x ,tk-y)))))))

(defmethod itemdelete ((canvas canvas) (item integer))
  (item-delete canvas item))

(defmethod item-delete ((canvas canvas) (item integer))
  (format-wish "~a delete ~a" (widget-path canvas) item)
  canvas)

(defmethod itemdelete ((canvas canvas) (item canvas-item))
  (format-wish "~a delete ~a" (widget-path canvas) (handle item))
  canvas)

(defmethod item-delete ((canvas canvas) (item canvas-item))
  (item-delete canvas (handle item)))

(defmethod move ((item canvas-item) dx dy)
  (itemmove (canvas item) (handle item) (tk-number dx) (tk-number dy)))

(defmethod move-to ((object canvas-item) x y)
  (item-move-to (canvas object) x y))

(defmethod clear ((canvas canvas))
  "delete all items within a canvas"
  (format-wish "~a delete all" (widget-path canvas))
  canvas)

;; canvas item functions

(defun create-line (canvas coords)
  (format-wish "senddata [~a create line ~a]" (widget-path canvas) (process-coords coords))
  (read-data))

(defun create-line* (canvas &rest coords)
  (funcall #'create-line canvas coords))

(defclass canvas-line (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-line) &key canvas coords)
  (setf (handle c) (create-line canvas coords)))

(defun make-line (canvas coords)
  (make-instance 'canvas-line :canvas canvas :coords coords))

(defun create-oval (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create oval ~a ~a ~a ~a]" (widget-path canvas)
               (tk-number x0) (tk-number y0)
               (tk-number x1) (tk-number y1))
  (read-data))

(defclass canvas-oval (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-oval) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-oval canvas x0 y0 x1 y1)))

(defun make-oval (canvas x0 y0 x1 y1)
  (make-instance 'canvas-oval :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))

(defun create-rectangle (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create rectangle ~a ~a ~a ~a]" (widget-path canvas)
               (tk-number x0) (tk-number y0) (tk-number x1) (tk-number y1))
  (read-data))

(defclass canvas-rectangle (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-rectangle) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-rectangle canvas x0 y0 x1 y1)))

(defun make-rectangle (canvas x0 y0 x1 y1)
  (make-instance 'canvas-rectangle :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))

(defun create-item-command (canvas item stream)
  "Create the tk command string for creating a canvas item according to the item spec.
   The item spec has the format '(itemtype args @rest)
   Where itemtype is the type of item to create, args its mandatory arguments
   and rest any number of option value pairs.
   Understood item types are:
     :rectangle x0 y0 y1 y2
     :arc x0 y0 x1 y1
     :line x0 y0 x1 y1
     :text x y text
     :ctext x y text
"
  (labels ((arg ()
             (pop item))
           (number ()
             (tk-number (pop item)))
           (args ()
             (loop
                while item
                do
                  (format stream " {-~a} {~a}"
                          (down (pop item))
                          (down (pop item))))))
    (let ((itemtype (pop item))
          (cpath (widget-path canvas)))
      (when (consp itemtype)
        (setf itemtype (car itemtype)))
      (cond
        ((eq itemtype :rectangle)
         (format stream "~a create rectangle ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :polygon)
         (format stream "~a create polygon ~a" cpath (process-coords (pop item)))
         (args))

        ((eq itemtype :arc)
         (format stream "~a create arc ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :oval)
         (format stream "~a create oval ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :line)
         (format stream "~a create line ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :image)
         (format stream "~a create image ~a ~a " cpath (number) (number))
         (args))

        ((eq itemtype :text)
         (format stream "~a create text ~a ~a -anchor nw -text {~a} "
                 cpath (number) (number) (tkescape (arg)))
         (args))

        ((eq itemtype :ctext)
         (format stream "~a create text ~a ~a -anchor n -text {~a} "
                 cpath (number) (number) (tkescape (arg)))
         (args))))))

(defun create-items (canvas items)
  "Create canvas items according to the item specs without returning nodgui objects for them.
   This means, they cannot be accessed in any way, but also the creation does not flush
   the nodgui output buffer."
  (let ((code (with-output-to-string (s)
                (dolist (item items)
                  (create-item-command canvas item s)
                  (format s "~%")))))
    (send-wish code)))

(defun make-items (canvas items)
  "Create canvas items according to the item specs and return a list of canvas-items."
  (let ((code (with-output-to-string (s)
                (format s "senddata \"( ~%")
                (dolist (item items)
                  (format s " [")
                  (create-item-command canvas item s)
                  (format s " ]~%"))
                (format s ")\"~%"))))
    (send-wish code)
    (let ((handles (read-data)))
      ;;(format t "data: ~s~%" erg) (finish-output)
      (loop for handle in handles as (itemtype) in items collect
           (let ((class (if (consp itemtype)
                            (cdr itemtype)
                            'canvas-item)))
             (make-instance class :canvas canvas :handle handle))))))

(defun create-text (canvas x y text &key (anchor :nw) (justify :left) (angle 0.0))
  (assert (find justify '(:left :right :center)))
  (format-wish (tcl-str (senddata [~a create text ~a ~a
                                  -angle   {~a}
                                  -justify ~\(~a~\)
                                  -anchor  {~\(~a~\)}
                                  -text    {~a}]))
               (widget-path canvas)
               (tk-number x) (tk-number y)
               angle
               justify
               anchor
               text)
  (read-data))

(defclass canvas-text (canvas-item) ())

(defmethod initialize-instance :after ((c canvas-text) &key canvas x y text)
  (setf (handle c) (create-text canvas x y text)))

(defun create-image (canvas x y &key image)
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -image ~a~]]" (widget-path canvas)
               (tk-number x) (tk-number y)
               (and image (name image)))
  (read-data))

(defclass canvas-image (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-image) &key canvas x y image)
  (setf (handle c) (create-image canvas x y :image image)))

(defun image-setpixel (image data x y &optional x2 y2 )
  (format-wish "~A put {~{{~:{#~2,'0X~2,'0X~2,'0X ~} } ~} } -to ~a ~a~@[ ~a~]~@[ ~a~]"
               (name image)
               data
               (tk-number  x)
               (tk-number  y)
               (tk-number x2)
               (tk-number y2))
  image)

(defun create-bitmap (canvas x y &key (bitmap nil))
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -bitmap ~a~]]"
               (widget-path canvas)
               (tk-number x) (tk-number y)
               (and bitmap (name bitmap)))
  (read-data))

(defun create-arc (canvas x0 y0 x1 y1
                   &key (start 0) (extent 180) (style "pieslice") (fill "#ff0000"))
  (with-canvas-path (path canvas)
    (format-wish (tclize
                  `(senddata [ ,path         " "
                             create arc
                             ,(tk-number x0) " "
                             ,(tk-number y0) " "
                             ,(tk-number x1) " "
                             ,(tk-number y1) " "
                             -start  {+ ,#[down start  ] }
                             -extent {+ ,#[down extent ] }
                             -style  {+ ,#[down style  ] }
                             -fill   {+ ,#[down fill   ] } ])))
    (read-data)))

(defclass canvas-arc (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-arc)
                                       &key
                                         canvas x0 y0 x1 y1
                                         (start 0) (extent 180) (style "pieslice"))
  (setf (handle c) (create-arc canvas x0 y0 x1 y1 :start start :extent extent :style style)))

(defclass canvas-window (canvas-item) ())

(defmethod itemconfigure ((widget canvas) item option value)
  (item-configure widget item option value))

(defmethod item-configure ((widget canvas) item option value)
  (format-wish "~A itemconfigure {~A} {-~(~A~)} {~A}"
               (widget-path widget)
               item
               option
               (if (stringp value) ;; There may be values that need to be passed as
                   value           ;; unmodified strings, so do not downcase strings
                   (format nil "~(~a~)" value))) ;; if its not a string, print it downcased
  widget)

;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure ((widget canvas) item option (value tkobject))
  (item-configure widget item option value))

;; TODO: call less specialized method
(defmethod item-configure ((widget canvas) item option (value tkobject))
  (format-wish "~A itemconfigure {~A} {-~(~A~)} {~A}"
               (widget-path widget)
               item
               option
               (widget-path value))
  widget)

(defmethod itemlower ((widget canvas) item &optional below)
  (item-lower widget item below))

(defmethod item-lower ((widget canvas) item &optional below)
  (format-wish "~A lower {~A} ~@[{~A}~]" (widget-path widget)
               item below)
  widget)

(defmethod lower ((item canvas-item) &optional below)
  (itemlower (canvas item) (handle item) (and below (handle below))))

(defmethod itemraise ((widget canvas) item &optional above)
  (item-raise widget item above))

;; TODO: refactor item-lower and item-raise, duplicated code
(defmethod item-raise ((widget canvas) item &optional above)
  (with-canvas-path (path widget)
    (format-wish "~A raise {~A} ~@[{~A}~]" (widget-path widget)
                 item above)
    widget))

(defun item-cget (canvas item option)
  (format-wish (tcl-str (senddatastring [~a itemcget {~\(~a~\)} {-~\(~a~\)}]))
               (widget-path canvas) item (down option))
  (read-data))

(defmethod initialize-instance :after ((c canvas-window) &key canvas (x 0) (y 0) widget (anchor :nw))
  (setf (handle c) (create-window canvas x y widget :anchor anchor)))

(defun create-window (canvas x y widget &key (anchor :nw))
  (format-wish "senddata [~a create window ~a ~a -anchor {~(~a~)} -window ~a]"
               (widget-path canvas)
               (tk-number x)
               (tk-number y)
               (down anchor)
               (widget-path widget))
  (read-data))

(defun postscript (canvas &key rotate pagewidth pageheight)
  (if (and (scrollregion-x0 canvas)
           (scrollregion-x1 canvas)
           (scrollregion-y0 canvas)
           (scrollregion-y1 canvas))
      (format-wish "senddatastring [~a postscript -colormode color -x ~a -y ~a -width ~a -height ~a~@[ -rotate ~a~]~@[ -pagewidth ~a~]~@[ -pageheight ~a~]]"
                (widget-path canvas)
                (scrollregion-x0 canvas) (scrollregion-y0 canvas)
                (- (scrollregion-x1 canvas) (scrollregion-x0 canvas))
                (- (scrollregion-y1 canvas) (scrollregion-y0 canvas))
                (tk-number rotate) (tk-number pageheight) (tk-number pagewidth))
    (format-wish "senddatastring [~a postscript -colormode color]" (widget-path canvas)))
  (read-data))

(defclass scrolled-canvas (frame)
  ((canvas :accessor canvas)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)))

(defun make-scrolled-canvas (master)
  (make-instance 'scrolled-canvas :master master ))

(defmethod initialize-instance :after ((sc scrolled-canvas) &key)
  (setf (hscroll sc) (make-scrollbar sc :orientation "horizontal"))
  (setf (vscroll sc) (make-scrollbar sc))
  (setf (canvas sc) (make-canvas sc :xscroll (hscroll sc) :yscroll (vscroll sc)))
  (grid (canvas sc) 0 0 :sticky :news)
  (grid (hscroll sc) 1 0 :sticky :we)
  (grid (vscroll sc) 0 1 :sticky :ns)
  (grid-columnconfigure sc 0 :weight 1)
  (grid-columnconfigure sc 1 :weight 0)
  (grid-rowconfigure sc 0 :weight 1)
  (grid-rowconfigure sc 1 :weight 0)
  (configure (hscroll sc)
             "command"
             (concatenate 'string (widget-path (canvas sc)) " xview"))
  (configure (vscroll sc)
             "command"
             (concatenate 'string (widget-path (canvas sc)) " yview"))
  (configure (canvas sc)
             "xscrollcommand"
             (concatenate 'string (widget-path (hscroll sc)) " set"))
  (configure (canvas sc)
             "yscrollcommand"
             (concatenate 'string (widget-path (vscroll sc)) " set")))
