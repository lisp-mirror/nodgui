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

(in-package :nodgui.pixmap)

(alexandria:define-constant +red-channel+   0 :test #'=)

(alexandria:define-constant +green-channel+ 1 :test #'=)

(alexandria:define-constant +blue-channel+  2 :test #'=)

(alexandria:define-constant +alpha-channel+ 3 :test #'=)

(alexandria:define-constant +targa-stream-element-type+ '(unsigned-byte 8) :test 'equalp)

(alexandria:define-constant +jpeg-stream-element-type+  '(unsigned-byte 8) :test 'equalp)

(defun make-bits-array (size)
  (make-fresh-array size 0 '(unsigned-byte 8) nil))

(defclass pixmap ()
  ((data
    :initarg :data
    :initform (make-array-frame 0)
    :accessor data
    :type vector
    :documentation "A  vector of 'nodgui.ubvec4:ubvec4',  each element
    of the  latter i a  (unsigned-byte 8) representing a  single color
    channel of this bitmap. Example: #(#(Red Green Blue Alpha) ...)")
   (width
    :initarg :width
    :initform 0
    :accessor width
    :type integer
    :documentation "Width (in pixel) of this pixmap")
   (height
    :initarg :height
    :initform 0
    :accessor height
    :type integer
    :documentation "Height (in pixel) of this pixmap")
   (depth
    :initform 4
    :accessor depth
    :initarg :depth
    :documentation  "Number   of  color  channels,  It   is  developer
    responsibility that this  value's slot matches the  length of each
    element of the slot 'data'")
   (bits
    :initform (make-bits-array 0)
    :accessor bits
    :initarg :bits
    :documentation "This  is the same  of 'data'  slots but as  a flat
    vector  (ex: #(RGBA  R_1G_2B_1 ...))   Notes that  this must  be a
    simple-array of (unsigned-byte 8), use 'make-bits-array' to ensure
    that this vector has the correct type."))
  (:documentation "This  class represent a  pixmap image, a  matrix of
   pixel element, each pixel's value is described by a variable number
   of color  channel; in the  RGB spaces the  color is described  by a
   triplet of  \"Red\", \"Green\"  and \"Blue\" values.   Optionally a
   channel can  specify the opacity of  the color: this is  called the
   \"Alpha\" value. Note that this package only supports colors with 4
   channel."))

(defmacro loop-matrix ((matrix x y &optional loop-name) &body body)
  "loop on each  element of a pixmap 'matrix' with  'x' and 'y' bounds
to the current scanned element"
  `(loop named ,loop-name for ,y fixnum from 0 below (height ,matrix) do
        (loop for ,x fixnum from 0 below (width ,matrix) do
             ,@body)))

(defun make-pixmap-frame (w h &optional (bg (ubvec4 0 0 0 0)))
  "Instantiate a pixmap object of width 'w' and height 'h' filled with
the color  bg. Please  note that  all the elements  of the  data slots
points to the same memory location (i.e. bg)."
  (assert (ubvec4p bg))
  (make-instance 'pixmap
                 :depth  4
                 :height h
                 :width  w
                 :data   (make-array-frame (* h w) bg 'ubvec4 t)))

(defun make-pixmap (w h &optional (bg (ubvec4 0 0 0 0)))
  "Instantiate a pixmap object of width 'w' and height 'h' filled with
 enough copies of the color bg."
  (assert (ubvec4p bg))
  (make-instance 'pixmap
                 :depth  4
                 :height h
                 :width  w
                 :data   (make-fresh-array (* h w) bg 'ubvec4 t)))

(defun copy-pixmap (object)
  "Make a copy of a pixmap"
  (make-instance 'pixmap
                 :depth  (depth object)
                 :width  (width object)
                 :height (height object)
                 :data   (copy-array (data object))))

(defgeneric bits-pixel@ (object x y))

(defgeneric (setf bits-pixel@) (color object x y))

(defgeneric (setf alpha-bits@) (alpha-value object x y))

(defgeneric sync-data-to-bits (object))

(defgeneric sync-bits-to-data (object))

(defgeneric pixmap->tga-file (object))

(defgeneric save-pixmap (object path))

(defgeneric h-mirror (object))

(defgeneric v-mirror (object))

(defgeneric scale-bilinear (object scale-x scale-y))

(defgeneric scale-nearest (object scale-x scale-y))

(defgeneric rotate-pixmap (object angle &key fill-value pivot repeat rounding-fn))

(defgeneric rotate-pixmap-w-repeat (object angle &key fill-value pivot rounding-fn))

(defgeneric rotate-pixmap-180-degree (object fill-value pivot))

(defgeneric rotate-pixmap-90-degree-ccw (object fill-value pivot))

(defgeneric rotate-pixmap-90-degree-cw (object fill-value pivot))

(defgeneric swap-elements (object row column row2 column2 &key destructive))

(defmacro with-check-borders ((x y x-bond y-bond w h) then else)
  `(if (and
        (>= (the fixnum ,x) (the fixnum ,x-bond))
        (<= (the fixnum ,x) (+ (the fixnum ,x-bond) (the fixnum ,w)))
        (>= (the fixnum ,y) (the fixnum ,y-bond))
        (<= (the fixnum ,y) (+ (the fixnum ,y-bond) (the fixnum ,h))))
       ,then
       ,else))

(defmacro with-check-matrix-borders ((matrix x y) &body then)
  `(with-check-borders (,x ,y 0 0 (- (the fixnum (width ,matrix)) 1)
                           (- (the fixnum (height ,matrix)) 1))
     (progn ,@then)
     nil))

(defmethod swap-elements ((object pixmap) row column row2 column2 &key (destructive t))
  (let ((res (if destructive object (copy-pixmap object))))
    (with-check-matrix-borders (res column row)
      (with-check-matrix-borders (res column2 row2)
        (let ((saved (pixel@ res column row)))
          (setf (pixel@ res column row)   (pixel@ res column2 row2)
                (pixel@ res column2 row2) saved))))
    res))

(defmethod h-mirror ((object pixmap))
  "Make a symmetric copy of a  pixmap using an horizontal line placed
on the middle of the pixture as reflection plane"
  (let ((row-pivot (floor (/ (height object) 2))))
    (loop for y from 0 below row-pivot do
         (loop for x from 0 below (width object) do
              (let ((row-destination (- (1- (height object)) y)))
                (swap-elements object y x row-destination x :destructive t)))))
  object)

(defmethod v-mirror ((object pixmap))
  "Make a symmetric copy of a  pixmap using a vertical line placed
on the middle of the pixture as reflection plane"
  (let ((col-pivot (floor (/ (width object) 2))))
    (loop for x from 0 below col-pivot do
         (loop for y from 0 below (height object) do
              (let ((col-destination (- (1- (width object)) x)))
                (swap-elements object y x y col-destination :destructive t)))))
  object)

(defun interpolate (weight px1 px2)
  (map 'ubvec4
       #'(lambda (c1 c2)
           (round (lerp weight (coerce c1 'single-float) (coerce c2 'single-float))))
       px1 px2))

(defmethod scale-bilinear ((object pixmap) scale-x scale-y)
  "Scale  a  bitmap  by  factors   'scale-x'  and  'scale-y'  (in  the
range (0-1.0]), scaling use bilinear filtering"
  (let* ((old-width   (width  object))
         (old-height  (height object))
         (dnew-width  (* scale-x old-width))
         (dnew-height (* scale-y old-height))
         (new-width   (floor dnew-width))
         (new-height  (floor dnew-height))
         (res         (make-pixmap-frame new-width new-height)))
    (loop-matrix (res x y)
       (let* ((y-frac   (/ (+ y (* 0.5 (- 1.0 scale-y))) scale-y))
              (sy       (floor y-frac))
              (wy       (- y-frac sy))
              (x-frac   (/ (+ x (* 0.5 (- 1.0 scale-x))) scale-x))
              (sx       (floor x-frac))
              (wx       (- x-frac sx))
              (floor-x  (alexandria:clamp sx      0 (1- old-width)))
              (ceil-x   (alexandria:clamp (1+ sx) 0 (1- old-width)))
              (floor-y  (alexandria:clamp sy      0 (1- old-height)))
              (ceil-y   (alexandria:clamp (1+ sy) 0 (1- old-height)))
              (a        (pixel@ object floor-x floor-y))
              (b        (pixel@ object floor-x ceil-y))
              (c        (pixel@ object ceil-x  ceil-y))
              (d        (pixel@ object ceil-x  floor-y))
              (inter-x1 (interpolate wx a d))
              (inter-x2 (interpolate wx b c))
              (inter-y  (interpolate wy inter-x1 inter-x2)))
         (setf (pixel@ res x y) (round-all inter-y))))
    res))

(defmethod scale-nearest ((object pixmap) scale-x scale-y)
    "Scale  a  bitmap  by  factors 'scale-x'  and  'scale-y'  (in  the
range (0-1.0]), scaling use nearest-neighbor."
  (let* ((new-width  (* scale-x (width object)))
         (new-height (* scale-y (height object)))
         (res        (make-pixmap-frame (floor new-width) (floor new-height))))
    (loop-matrix (res x y)
       (let* ((new-pixel (vector (/ x scale-x)
                                 (/ y scale-y)))
              (fx        (elt new-pixel 0))
              (fy        (elt new-pixel 1))
              (floor-x   (floor fx))
              (floor-y   (floor fy))
              (a         (pixel@ object floor-x floor-y)))
         (setf (pixel@ res x y) a)))
    res))

(defmethod rotate-pixmap-180-degree ((object pixmap) fill-value pivot)
  "Rotate a pixmap 180 degrees"
  (let* ((w   (width object))
         (h   (height object))
         (res (make-pixmap-frame w h fill-value)))
    (declare (fixnum w h))
    (loop-matrix (object x y)
      (let* ((point  (vec2 x y))
             (vpivot (vec2 (elt pivot 0) (elt pivot 1)))
             (rev    (vec2- (vec2+ (vec2-negate (vec2- point vpivot)) vpivot)
                            (vec2 1.0 1.0)))
             (rev-x  (floor (elt rev 0)))
             (rev-y  (floor (elt rev 1))))
        (with-check-matrix-borders (res rev-x rev-y)
          (setf (pixel@ res rev-x rev-y) (pixel@ object x y)))))
    res))

(defmethod rotate-pixmap-90-degree-ccw ((object pixmap) fill-value pivot)
  "Rotate a pixmap 90° counterclockwise"
  (let* ((w        (floor (width object)))
         (h        (floor (height object)))
         (res      (make-pixmap-frame w h fill-value)))
    (declare (fixnum w h))
    (loop-matrix (res x y)
       (let* ((point  (vec2 x y))
              (vpivot (vec2  (elt pivot 0) (elt pivot 1)))
              (rev    (vec2- point vpivot))
              (rev-x  (floor (+ (elt vpivot 0) -1 (- (elt rev 1)))))
              (rev-y  (floor (+ (elt vpivot 1)    (elt rev 0)))))
         (with-check-matrix-borders (res rev-x rev-y)
           (setf (pixel@ res x y) (pixel@ object rev-x rev-y)))))
      res))

(defmethod rotate-pixmap-90-degree-cw ((object pixmap) fill-value pivot)
  "Rotate a pixmap 90° clockwise"
  (let* ((w        (floor (width object)))
         (h        (floor (height object)))
         (res      (make-pixmap-frame w h fill-value)))
    (declare (fixnum w h))
    (loop-matrix (res x y)
       (let* ((point  (vec2 x y))
              (vpivot (vec2  (elt pivot 0) (elt pivot 1)))
              (rev    (vec2- point vpivot))
              (rev-x  (floor (+ (elt vpivot 0)    (- (elt rev 1)))))
              (rev-y  (floor (+ (elt vpivot 1) -1 (elt rev 0)))))
         (with-check-matrix-borders (res rev-x rev-y)
           (setf (pixel@ res x y) (pixel@ object rev-x rev-y)))))
      res))

(defun repeat-periodic-coord (val max)
  (if (< val 0)
      (+ max
         (* max
            (- (/ val max)
               (truncate (/ val max)))))
      (+ (mod (floor val) max)
         (- val
            (truncate val)))))

(defun bilinear-interpolation (matrix x y
                               &key (interpolate-fn #'interpolate)
                                 (behaivour-on-border-fn #'repeat-periodic-coord))
  ;; a          b
  ;; +----------+
  ;; |          |
  ;; |          |
  ;; +----------+
  ;; d          c
  (let* ((actual-x  x)
         (actual-y  y)
         (floor-x   (floor (funcall behaivour-on-border-fn
                                    (floor actual-x) (width matrix))))
         (floor-y   (floor (funcall behaivour-on-border-fn
                                    (floor actual-y) (height matrix))))
         (ceiling-x (ceiling (funcall behaivour-on-border-fn
                                      (ceiling actual-x) (width matrix))))
         (ceiling-y (ceiling (funcall behaivour-on-border-fn
                                      (ceiling actual-y) (height matrix))))
         (dx        (- actual-x (floor x)))
         (dy        (- actual-y (floor y)))
         (a         (pixel@ matrix floor-x floor-y))
         (b         (pixel@ matrix floor-x ceiling-y))
         (c         (pixel@ matrix ceiling-x ceiling-y))
         (d         (pixel@ matrix ceiling-x floor-y))
         (inter-x1  (funcall interpolate-fn dx a d))
         (inter-x2  (funcall interpolate-fn dx b c))
         (inter-y   (funcall interpolate-fn dy inter-x1 inter-x2)))
    inter-y))

(defmethod rotate-pixmap ((object pixmap) angle
                          &key
                            (fill-value  (ubvec4 0 0 0 0))
                            (pivot (vec2 (->f (/ (width object)  2))
                                         (->f (/ (height object) 2))))
                            (repeat nil)
                            (rounding-fn #'round))
  "Rotate a  pixmap about an  arbitrary angle and around  an arbitrary
  pivot (the center  of the image by default), The  'void' part of the
  image  vill be  filled with  'fill-value' (a  'nodgui.ubvec4:ubvec4'
  vector) or  with repeated parial clone  clones of the same  image if
  'repeat' is non nil"
  (cond
    ;; using  the next tree functions because  the  usual  approach below  for
    ;; rotating did not worked for me
    ((epsilon= (->f angle) 90.0)
     (rotate-pixmap-90-degree-ccw object fill-value pivot))
    ((epsilon= (->f angle) -90.0)
     (rotate-pixmap-90-degree-cw object fill-value pivot))
    ((epsilon= (abs (->f angle)) 180.0)
     (rotate-pixmap-180-degree object fill-value pivot))
    (repeat
        (rotate-pixmap-w-repeat object angle :fill-value fill-value :pivot pivot
                                :rounding-fn rounding-fn))
    (t
     (let ((res (make-pixmap-frame (width object) (height object) fill-value))
           (act-angle (->f (deg->rad angle))))
       (loop-matrix (res x y)
         (let* ((new-pixel (vec2+ (vec2-rotate  (vec2+ (vec2 x y)
                                                       (vec2-negate pivot))
                                                act-angle)
                                  (vec2 (elt pivot 0)
                                        (elt pivot 1))))
                (floor-x   (floor   (vec2-x new-pixel)))
                (floor-y   (floor   (vec2-y new-pixel)))
                (ceiling-x (ceiling (vec2-x new-pixel)))
                (ceiling-y (ceiling (vec2-y new-pixel))))
           (with-check-matrix-borders (object floor-x floor-y)
             (with-check-matrix-borders (object ceiling-x ceiling-y)
               (let* ((px (bilinear-interpolation object
                                                  (elt new-pixel 0)
                                                  (elt new-pixel 1)
                                                  :interpolate-fn #'interpolate)))
                 (setf (pixel@ res x y)
                       (if rounding-fn
                           (round-all px :rounding-function rounding-fn)
                           px)))))))
         res))))

(defmethod rotate-pixmap-w-repeat ((object pixmap) angle
                                   &key (fill-value 0)
                                     (pivot (list (/ (width object) 2)
                                                  (/ (height object) 2)))
                                     (rounding-fn #'round))
  (let ((res       (make-pixmap-frame (width object) (height object) fill-value))
        (act-angle (->f (deg->rad angle))))
    (loop-matrix (res x y)
       (let* ((new-pixel (vec2+ (vec2-rotate (vec2+ (vec2 x y)
                                                    (vec2 (- (first pivot))
                                                          (- (second pivot))))
                                             act-angle)
                                (vec2 (first pivot)
                                      (second pivot)))))
         (let* ((px  (bilinear-interpolation object
                                             (elt new-pixel 0)
                                             (elt new-pixel 1)
                                             :interpolate-fn #'interpolate
                                             :behaivour-on-border-fn #'repeat-periodic-coord)))
           (setf (pixel@ res x y)
                 (if rounding-fn
                     (round-all px :rounding-function rounding-fn)
                     px)))))
    res))


(defmethod sync-data-to-bits ((object pixmap))
  "Fill 'bits' slot of this pixmap  with the contents of 'data' slots"
  (with-accessors ((data data) (bits bits) (depth depth)) object
    (when (/= (length data) (* depth (length bits)))
      (setf bits (make-bits-array (* (length data) depth))))
    (loop for pixel-count from 0 below (length data) do
         (loop for channel-count from 0 below depth do
              (setf (elt bits (+ (* pixel-count depth) channel-count))
                    (elt (elt data pixel-count) channel-count))))
    object))

(defmethod sync-bits-to-data ((object pixmap))
  "Fill 'data' slot of this pixmap  with the contents of 'bits' slots"
  (with-accessors ((data   data)
                   (bits   bits)
                   (depth  depth)
                   (width  width)
                   (height height)) object
    (let ((pixel (make-fresh-ubvec4))
          (data-size (* width height)))
      (if (/= (length data) (* depth (length bits)))
          (progn
            (setf data (make-array-frame data-size +ubvec4-zero+ 'ubvec4 t))
            (loop for i from 0 below (length bits) by depth do
                 (loop for channel-count from 0 below depth do
                      (setf (elt pixel channel-count) (elt bits (+ i channel-count))))
                 (setf (elt data (truncate (/ i depth))) (alexandria:copy-array pixel))))
          (loop for i from 0 below (length bits) by depth do
               (loop for channel-count from 0 below depth do
                    (setf (elt pixel channel-count)
                          (elt bits (+ i channel-count))))
               (setf (elt data (truncate (/ i depth))) (alexandria:copy-array pixel)))))
    object))

(defmethod save-pixmap ((object pixmap) path)
  "Save bitmap in TARGA bitmap format"
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede :if-does-not-exist :create
                          :element-type +targa-stream-element-type+)
    (write-sequence (pixmap->tga-file object) stream))
  object)

(defmethod pixel@ ((object pixmap) x y)
  "Get the color of pixel at specified coordinate from 'data' slot"
  (declare (pixmap object))
  (declare (fixnum x y))
  (elt (the (simple-array * (*)) (data object))
       (+ (* (the fixnum (width object)) y) x)))

(defmethod (setf pixel@) (color (object pixmap) x y)
  "Set the color of pixel at the specified coordinate for 'data' slot"
  (declare (pixmap object))
  (declare (fixnum x y))
  (setf (elt (the (simple-array * (*)) (data object))
             (+ (* (the fixnum (width object)) y) x))
        color))

(declaim (inline %offset-bits))

(defun %offset-bits (w x y)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum w x y))
  (the fixnum (* 4 (+ (the fixnum (* w y)) x))))

(defmethod bits-pixel@ ((object pixmap) x y)
  "Get the color of pixel at specified coordinate from 'bits' slot"
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (fixnum x y))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (ubvec4 (elt bits      offset)
              (elt bits (+ 1 offset))
              (elt bits (+ 2 offset))
              (elt bits (+ 3 offset))))))

(defmethod (setf bits-pixel@) (color (object pixmap) x y)
  "Set the color of pixel at specified coordinate for 'bits' slot"
  (declare (fixnum x y))
  (declare (ubvec4 color))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array fixnum) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (setf (elt bits      offset)  (elt color 0)
            (elt bits (+ 1 offset)) (elt color 1)
            (elt bits (+ 2 offset)) (elt color 2)
            (elt bits (+ 3 offset)) (elt color 3)))
    object))

(defmethod (setf alpha-bits@) (alpha-value (object pixmap) x y)
  "Set  the alpha  component  for  the 'bits'  slotys  only; value  is
an (unsigned-byte 8)"
  (declare (fixnum x y))
  (declare ((unsigned-byte 8) alpha-value))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (setf (elt bits (+ 3 offset)) alpha-value))
    object))

(defclass pixmap-file (pixmap)
  ((magic-number
    :initform ""
    :accessor magic-number
    :initarg :magic-number
    :documentation "The number that identify this file format")
   (errors
    :initform nil
    :accessor errors
    :initarg :errors))
   (:documentation "A file that contain a pixmap"))

(defun slurp-pixmap (type file)
  "Makes a instance of pixmap-derived  class type with contents loaded
from file: 'file'"
  (let ((px (make-instance type)))
    (pixmap-load px file)
    px))

(defgeneric pixmap-load (object file)
  (:documentation "load a fixmap form file 'file'"))

(alexandria:define-constant +targa-img-rgba-rle+                  10 :test 'equalp)

(alexandria:define-constant +targa-img-rgba+                       2 :test 'equalp)

(alexandria:define-constant +targa-img-header-size+               18 :test 'equalp)

(alexandria:define-constant +targa-img-scanline-topleft+           2 :test 'equalp)

(alexandria:define-constant +targa-img-scanline-bottomleft+        0 :test 'equalp)

(alexandria:define-constant +targa-img-signature+ "TRUEVISION-XFILE" :test 'equalp)

(define-offset-size nodgui.pixmap
    targa-img (id-len 0 1) (type 2 1) (spec 8 10)
    (id +targa-img-header-size+) (colormap-spec 3 5))

(defclass tga (pixmap-file)
  ()
  (:documentation "A file in TARGA bitmap format"))

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (pixmap-load object path)))

(defgeneric rearrange-scanline-by-pixmap-origin (object origin))

(defgeneric load-from-stream (object stream))

(define-parse-header-chunk (image-id-len +targa-img-id-len-offset+
                                              +targa-img-id-len-size+ tga nil))

(define-parse-header-chunk (image-type +targa-img-type-offset+
                                            +targa-img-type-size+ tga nil))

(define-parse-header-chunk (image-specs +targa-img-spec-offset+
                                            +targa-img-spec-size+ tga nil))

(define-parse-header-chunk (colormap-specs +targa-img-colormap-spec-offset+
                                            +targa-img-colormap-spec-size+ tga nil))

(defun load-rearrange-raw-packet (bgra data)
  (loop for i from 0 below (length bgra) by 4 do
       (let ((b (elt bgra i))
             (g (elt bgra (+ i 1)))
             (r (elt bgra (+ i 2)))
             (a (elt bgra (+ i 3))))
         (vector-push-extend r data)
         (vector-push-extend g data)
         (vector-push-extend b data)
         (vector-push-extend a data))))

(defmethod load-from-stream ((object tga) (stream stream))
  (let ((type (first (parse-image-type object stream))))
    (if (or (= type +targa-img-rgba+)
            (= type +targa-img-rgba-rle+))
        (let* ((id-len (first (parse-image-id-len object stream)))
               (colormap-specs (parse-colormap-specs object stream))
               (colormap-len (byte->int (subseq colormap-specs 2 4)))
               (img-specs (parse-image-specs object stream))
               ;;(x-origin  (byte->int (subseq img-specs 0 2)))
               ;;(y-origin  (byte->int (subseq img-specs 2 4)))
               (width     (byte->int (subseq img-specs 4 6)))
               (height    (byte->int (subseq img-specs 6 8)))
               (depth     (/ (byte->int (subseq img-specs 8 9)) 8))
               (img-descr (first (subseq img-specs 9 10)))
               (alpha-size (boole boole-and img-descr #x0e))
               (scanline-origin (ash (boole boole-and img-descr #x30) -4))
               (scanline-offset (+ +targa-img-header-size+ id-len colormap-len))
               (scanline-size (* width height depth))
               (bgra (make-array-frame (* width depth))))
          (if (/= 8 alpha-size)
              (push (format nil "Alpha bitsize should be 8 instead of ~x" alpha-size)
                    (errors object))
              (progn
                (setf (width object)  width
                      (height object) height
                      (depth object) depth)
                (file-position stream scanline-offset)
                (if (= type +targa-img-rgba+)
                    (progn
                      (loop for i from 0 below height do
                           (read-sequence bgra stream)
                           (load-rearrange-raw-packet bgra (bits object))))
                    (loop for i from 0 below (/ scanline-size depth) do
                         (let* ((packet-head (read-byte stream))
                                (packet-type (boole boole-and packet-head #x80))
                                (packet-count (boole boole-and packet-head #x7f)))
                           (if (= packet-type #x80) ;; rle packet
                               (let ((b (read-byte stream))
                                     (g (read-byte stream))
                                     (r (read-byte stream))
                                     (a (read-byte stream)))
                                 (loop for px from 0 to packet-count do
                                      (vector-push-extend r (bits object))
                                      (vector-push-extend g (bits object))
                                      (vector-push-extend b (bits object))
                                      (vector-push-extend a (bits object))))
                               (loop for px from 0 to packet-count do ;; raw packet
                                    (let ((b (read-byte stream))
                                          (g (read-byte stream))
                                          (r (read-byte stream))
                                          (a (read-byte stream)))
                                      (vector-push-extend r (bits object))
                                      (vector-push-extend g (bits object))
                                      (vector-push-extend b (bits object))
                                      (vector-push-extend a (bits object)))))
                           (incf i packet-count))))
                (rearrange-scanline-by-pixmap-origin object scanline-origin)
                (sync-bits-to-data object)
                object)))
        (push "Image type not supported: only rgba and compressed rgba allowed."
              (errors object)))))

(defmethod pixmap-load ((object tga) (file string))
  (with-open-file (stream file :element-type +targa-stream-element-type+
                          :if-does-not-exist :error)
    (load-from-stream object stream)))

(defmethod rearrange-scanline-by-pixmap-origin ((object tga) origin)
  (with-accessors ((height height) (bits bits) (width width)
                   (depth depth)) object
    (macrolet ((swap-bits (a b)
                 `(rotatef (elt bits ,a) (elt bits ,b))))
      (cond
        ((= origin +targa-img-scanline-topleft+)
         t)
        ((= origin +targa-img-scanline-bottomleft+)
         (let ((scanline-length (* depth width)))
           (loop for y from 0 below (floor (/ height 2)) do
                (loop for x from 0 below scanline-length do
                     (let ((sup (+ x (* y scanline-length)))
                           (inf (+ (* (- (1- height) y) scanline-length) x)))
                       (swap-bits sup inf))))))
        (t
         (push "scanline origin not-supported, top-left or bottom-left only are allowed"
               (errors object))
         nil)))))

(defun rle-encode-line (line depth &optional (res '()))
  (if (and line (> (length line) 0))
      (let* ((pix (elt line 0))
             (rearranged-pixel (vector (elt pix 2)   ; b
                                       (elt pix 1)   ; r
                                       (elt pix 0)   ; g
                                       2))
             (max-packet-count (do* ((ct 1 (1+ ct)))
                                    ((not (and (< ct (length line))
                                               (< ct 127)
                                               (equalp (elt line ct) pix)))
                                     ct))))

        (when (= depth 4)
          (setf (elt rearranged-pixel 3) (elt pix 3)))
        (rle-encode-line (subseq line max-packet-count)
                         depth
                         (push (list max-packet-count rearranged-pixel) res)))
      (reverse res)))

(defun targa-rle-scanline (line depth)
  (let ((encoded (rle-encode-line line depth)))
    (alexandria:flatten
     (mapcar #'(lambda (a)
                 (list (boole boole-ior #x80 (boole boole-and (1- (first a)) #x7f))
                       (second a)))
             encoded))))

(defmethod pixmap->tga-file ((object pixmap))
  (assert (= 4 (depth object)))
  (labels ((write-bytes (vector bytes)
             (loop for i in bytes do (vector-push-extend i vector))
             vector))
  (let ((results (make-array-frame 0 +targa-stream-element-type+)))
    (vector-push-extend #x0 results) ; image id has zero length
    (vector-push-extend #x0 results) ; no colormap
    (vector-push-extend #x0a results) ; we use only truecolor image also we want them compressed
    (write-bytes results '(#x00 #x00 #x00 #x00 #x00)) ; color map specification
                                                      ; all zero for true color image
    (setf results (write-bytes results '(#x00 #x00))) ; x-origin
    (setf results (write-bytes results '(#x00 #x00))) ; y-origin
    (setf results (write-bytes results (int16->bytes (width object))))
    (setf results (write-bytes results (int16->bytes (height object))))
    (vector-push-extend #x20 results) ; 32 bpp plus alpha channel
    (vector-push-extend #x28 results) ; image origin top-left alpha channel, important
    (loop for i from 0 below (length (data object)) by (width object) do
         (let ((line (targa-rle-scanline (subseq (data object) i (+ i (width object)))
                                         (depth object))))
           (loop for p in line do
                (etypecase p
                    (number (vector-push-extend p results))
                    (vector (loop for j across p do (vector-push-extend j results)))
                    (list (loop for j in p do (vector-push-extend j results)))))))
    ;; footer
    (loop repeat 8 do
         (vector-push-extend #x0 results))
    (loop for i across +targa-img-signature+ do
         (vector-push-extend (char-code i) results))
    (vector-push-extend (char-code #\.) results)
    (vector-push-extend #x0 results)
    results)))

(defclass jpeg (pixmap-file)
  ()
  (:documentation "A pixmap stored in JPG format"))

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (pixmap-load object path)))

(defmethod pixmap-load ((object jpeg) (file string))
  (with-open-file (stream file
                          :element-type      +jpeg-stream-element-type+
                          :if-does-not-exist :error)
    (load-from-stream object stream)))

(defmethod load-from-stream ((object jpeg) (stream stream))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (multiple-value-bind (raw-data image-h image-w)
        (cl-jpeg:decode-stream stream)
      (let ((new-data (make-array-frame (* image-w image-h) (ubvec4 0 0 0 0) 'ubvec4 t)))
        (loop ; add alpha channel and swap BGR -> RGB
           for i from 0 below (length raw-data) by 3
           for j from 0 below (length new-data) by 1 do
             (setf (elt new-data j)
                   (ubvec4 (elt raw-data (+ i 2))
                           (elt raw-data (+ i 1))
                           (elt raw-data i))))
      (setf data   new-data
            width  image-w
            height image-h)
      (sync-data-to-bits object)
      object))))
