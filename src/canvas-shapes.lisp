;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018,2019 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui.shapes)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defclass canvas-handler-holder ()
  ((handle
    :initform nil
    :accessor handle
    :initarg  :handle
    :documentation "The TK handle (the ID) of this object."))
  (:documentation "A class that holds a canvas."))

(defclass shape (canvas-holder)
  ((coordinates
    :initform '()
    :initarg  :coordinates
    :accessor coordinates
    :documentation  "The  coordinates  of  the  vertices  (or  control
    points) belonging to this shape")
   (fill-color
    :initform "#ffffff"
    :initarg  :fill-color
    :accessor fill-color
    :documentation  "The  inner color of this shape")
   (outline-color
    :initform "#FFFFFF"
    :initarg  :outline-color
    :accessor outline-color
    :documentation  "The  outline color of this shape")
   (outline-width
    :initform 1
    :initarg  :outline-width
    :accessor outline-width
    :documentation  "The  width of the outline that surrounds this shape"))
  (:documentation "A generic shape to be drawn on a canvas"))

(defgeneric shape-move (object dx dy)
  (:documentation "Move this shape by the amoount specified in dx and dx"))

(defgeneric shape-move-to (object x y)
  (:documentation "Move this shape so  that its (minimum x, minimum y)
  point coincides with (x, y)"))

(defgeneric shape-delete (object)
  (:documentation "Remove this shape trom canvas"))

(defmethod shape-delete ((object shape))
  (with-accessors ((canvas canvas)
                   (handle handle)) object
    (and handle
         (item-delete canvas handle))))

(defmethod shape-move ((object shape) dx dy)
  (with-accessors ((canvas canvas)
                   (handle handle)) object
    (and handle
         (item-move canvas handle dx dy))))

(defmethod shape-move-to ((object shape) x y)
  (with-accessors ((canvas canvas)
                   (handle handle)) object
    (and handle
         (item-move-to canvas handle x y))))

(defun create-polygon (canvas coords
                       &key
                         (fill-color "#0000ff") (outline-color "#ff0000")
                         (outline-width 1))
  "Create a polygon.
This is the low level procedure that deal with TK.
canvas:        the canvas where draw the polygon
coords:        the vertices of this polygon
fill-color:    the color that fills this polygon
outline-color: the color of  the contour that surrond this polygon
outline-width: the width in pixel of the outline of this polygon"
  (with-read-data ()
    (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(senddata [,(widget-path canvas) " "
                                      create polygon
                                      ,(process-coords coords) " "
                                      ,(empty-string-if-nil fill-color
                                                            `(-fill  {+ ,fill-color }))
                                      ,(empty-string-if-nil outline-color
                                                            `(-outline  {+ ,outline-color }))
                                      ,(empty-string-if-nil outline-width
                                                            `(-width  {+ ,outline-width  }))
                                      ]))))))

(defclass polygon (shape canvas-handler-holder)
  ()
  (:documentation "A filled polygon"))

(defmethod initialize-instance :after ((object polygon) &key &allow-other-keys)
  (with-accessors ((handle        handle)
                   (fill-color    fill-color)
                   (coordinates   coordinates)
                   (canvas        canvas)
                   (outline-color outline-color)
                   (outline-width outline-width)) object
    (setf handle (create-polygon canvas
                                 coordinates
                                 :fill-color    fill-color
                                 :outline-color outline-color
                                 :outline-width outline-width))))

(defun make-polygon (canvas coords
                     &key
                       (fill-color "#ffffff") (outline-color "#BEBEBE")
                       (outline-width 1))
    "Make a polygon.
canvas:        the canvas where draw the polygon
coords:        the vertices of this polygon  (x1 y1 x2 y2 ...)
fill-color:    the color that fills this polygon
outline-color: the color of  the contour that surrond this polygon
outline-width: the width in pixel of the outline of this polygon"
  (make-instance 'polygon
                 :canvas        canvas
                 :coordinates   coords
                 :fill-color    fill-color
                 :outline-color outline-color
                 :outline-width outline-width))

(define-constant +star-bbox-fix-scale+ 0.97 :test #'=)

(defclass star (polygon)
  ()
  (:documentation "A star-shaped polygon"))

(defun make-star (canvas ext-radius inner-radius-ratio inner-color outer-color corners
                    &key
                      (draw-left-half nil) (draw-right-half nil)
                      (outline-width 1))
  "draw a star shaped polygon.
   canvas:             the canvas where draw this star to
   ext-radius:         the external radius of the circle that inscribe this star
   inner-radius-ratio: the ratio between concave and convex point length of this star
   inner-color:        the color of this star
   outer-color:        the color of the outline of this star
   corners:            the number of spikes for this star
   draw-left-half      draw left half side of the star only
   draw-right-half     draw right half side of the star only
   outline-width:      the width in pixel of the outline of this polygon.
   Return an instance of 'star'
"
  (assert (> corners 0))
  (assert (not (and draw-left-half draw-right-half)))
  (flet ((make-points (start num)
           (let ((inc (->f (/ nodgui.constants:+2pi+ num)))
                 (dir (vec2-normalize start)))
             (loop
                repeat num
                for angle from 0.0 downto -1000.0 by inc collect
                  (let* ((rotated (vec2-rotate dir     angle))
                         (scaled  (vec2*       rotated (vec2-length start))))
                    scaled))))
         (slice (seq num)
           (subseq seq 0 num)))
    (let* ((corners-num    (if (or draw-left-half
                                   draw-right-half)
                               (ceiling (/ corners 2))
                               corners))
           (starting-angle (if (oddp corners)
                               (->f (- (/ nodgui.constants:+2pi+
                                          (* 2 corners))))
                               (->f (- (/ nodgui.constants:+2pi+
                                          corners)))))
           (ext-start      (vec2-rotate (vec2 0.0 (->f ext-radius))
                                                    starting-angle))
           (ext-points     (make-points ext-start corners))
           (inner-start    (vec2-rotate (vec2* ext-start
                                               inner-radius-ratio)
                                        (->f (/ nodgui.constants:+2pi+
                                                (* 2 corners)))))
           (inner-points   (make-points inner-start corners))
           (points         (alexandria:flatten (mapcar #'list
                                                       (slice inner-points corners-num)
                                                       (slice ext-points   corners-num)))))
      (when (and (or draw-left-half
                     draw-right-half)
                 (evenp corners))
        (push (alexandria:last-elt ext-points) points))
      (when draw-left-half
        (setf points
              (mapcar (lambda (a) (vec2 (- (vec2-x a)) (vec2-y a)))
                      points)))
      (make-instance 'star
                     :outline-width outline-width
                     :canvas        canvas
                     :coordinates   points
                     :fill-color    inner-color
                     :outline-color outer-color))))

(defclass two-color-star (shape)
  ((bbox-fix
    :initform +star-bbox-fix-scale+
    :initarg  :bbox-fix
    :accessor bbox-fix)
   (left-side
    :initform nil
    :initarg  :left-side
    :accessor left-side)
   (right-side
    :initform nil
    :initarg  :right-side
    :accessor right-side))
  (:documentation "A  star-shaped polygon  with two  differents colors
  for left and right side"))

(defun make-two-color-star (canvas
                            ext-radius
                            inner-radius-ratio
                            inner-color-left
                            outer-color-left
                            inner-color-right
                            outer-color-right
                            corners
                            &key (outline-width 1))
  "draw a star shaped polygon.
   canvas:             the canvas where draw this star to
   ext-radius:         the external radius of the circle that inscribe this star
   inner-radius-ratio: the ratio between concave and convex point length of this star
   inner-color-left:   the color of the left side of this  star
   outer-color-left:   the color of the outline of the left side of this star
   inner-color-right:  the color of the right side of this  star
   outer-color-right:  the color of the outline of the right side of this star
   corners:            the number of spikes for this star
   outline-width:      the width in pixel of the outline of this polygon.
   return an istance of two-color-star.
"
  (let ((star-left  (make-star canvas ext-radius inner-radius-ratio
                               inner-color-left outer-color-left
                               corners
                               :outline-width   outline-width
                               :draw-right-half nil
                               :draw-left-half t))
        (star-right (make-star canvas ext-radius inner-radius-ratio
                               inner-color-right outer-color-right
                               corners
                               :outline-width   outline-width
                               :draw-right-half t
                               :draw-left-half  nil)))
    (make-instance 'two-color-star
                   :canvas     canvas
                   :left-side  star-left
                   :right-side star-right)))

(defmethod shape-move ((object two-color-star) dx dy)
  (with-accessors ((left-side  left-side)
                   (right-side right-side)) object
    (shape-move left-side  dx dy)
    (shape-move right-side dx dy)))

(defmethod shape-move-to ((object two-color-star) x y)
  (with-accessors ((left-side  left-side)
                   (right-side right-side)) object
    (with-accessors ((left-side-handle handle)
                     (canvas           canvas)) left-side
      (let* ((*bbox-scale-fix* (bbox-fix object))
             (aabb             (canvas-item-bbox canvas left-side-handle))
             (max-x            (bbox-max-x aabb))
             (min-x            (bbox-min-x aabb))
             (outline-width    (floor (safe-parse-number (item-cget canvas
                                                                    left-side-handle
                                                                    :width)
                                                         :fix-fn (lambda (e)
                                                                   (declare (ignore e))
                                                                   0))))
             (w     (- max-x min-x outline-width)))
        (shape-move-to left-side       x  y)
        (shape-move-to right-side (+ w x) y)))))

(defmethod shape-delete ((object two-color-star))
  (with-accessors ((left-side  left-side)
                   (right-side right-side)) object
    (shape-delete left-side)
    (shape-delete right-side)))

(defun two-color-star-handle (object accessor)
  (handle (funcall accessor object)))

(defgeneric left-side-handle (object))

(defgeneric right-side-handle (object))

(defmethod left-side-handle ((object two-color-star))
  (two-color-star-handle object #'left-side))

(defmethod right-side-handle ((object two-color-star))
  (two-color-star-handle object #'right-side))
