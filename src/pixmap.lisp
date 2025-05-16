;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2021 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.pixmap)

(a:define-constant +red-channel+   0 :test #'=)

(a:define-constant +green-channel+ 1 :test #'=)

(a:define-constant +blue-channel+  2 :test #'=)

(a:define-constant +alpha-channel+ 3 :test #'=)

(a:define-constant +targa-stream-element-type+ '(unsigned-byte 8) :test 'equalp)

#-nodgui-lite
(a:define-constant +jpeg-stream-element-type+  '(unsigned-byte 8) :test 'equalp)

(defun buffer-sizes->static-vector-size (width height)
  (declare (fixnum width height))
  #.nodgui.config:default-optimization
  (nodgui.typed-operations:f* width height))

(defun make-buffer (width height)
  (make-buffer-elements (buffer-sizes->static-vector-size width height)))

(defun make-buffer-elements (element-count)
  (static-vectors:make-static-vector element-count
                                     :element-type '(unsigned-byte 32)
                                     :initial-element #x000000ff))

(defun free-buffer-memory (buffer)
  (static-vectors:free-static-vector buffer))

(u:definline copy-buffer-row-ffi-pointer (source-buffer-pointer
                                    destination
                                    source-buffer-width
                                    destination-buffer-width
                                    x-source
                                    y-source
                                    x-destination
                                    y-destination
                                    pixels-count)
  (declare (fixnum x-source
                   y-source
                   x-destination
                   y-destination
                   pixels-count
                   source-buffer-width
                   destination-buffer-width))
  (declare ((simple-array (unsigned-byte 32)) destination))
  #+sbcl (declare (sb-sys:system-area-pointer source-buffer-pointer))
  #.nodgui.config:default-optimization
  (let ((offset-source      (to:f+ x-source      (the fixnum
                                                      (to:f* y-source
                                                        source-buffer-width))))
        (offset-destination (to:f+ x-destination (the fixnum
                                                      (to:f* y-destination
                                                        destination-buffer-width)))))
    (u:copy-ffi-vector (cffi:inc-pointer source-buffer-pointer (* offset-source 4))
                       (cffi:inc-pointer (static-vectors:static-vector-pointer destination)
                                         (* offset-destination 4))
                       (to:f* pixels-count 4))))

(defun copy-buffer-row (source
                        destination
                        source-buffer-width
                        destination-buffer-width
                        x-source
                        y-source
                        x-destination
                        y-destination
                        pixels-count)
  (declare (fixnum x-source
                   y-source
                   x-destination
                   y-destination
                   pixels-count
                   source-buffer-width
                   destination-buffer-width))
  (declare ((simple-array (unsigned-byte 32)) source destination))
  #.nodgui.config:default-optimization
  (copy-buffer-row-ffi-pointer #+sbcl (the sb-sys:system-area-pointer
                                           (static-vectors:static-vector-pointer source))
                               #-sbcl (static-vectors:static-vector-pointer source)
                               destination
                               source-buffer-width
                               destination-buffer-width
                               x-source
                               y-source
                               x-destination
                               y-destination
                               pixels-count))

(defmacro with-buffer ((buffer width height) &body body)
  `(let ((,buffer nil))
     (unwind-protect
          (progn
            (setf ,buffer (make-buffer ,width ,height))
            ,@body)
       (free-buffer-memory ,buffer))))

(u:definline extract-red-component (color)
  (declare (fixnum color))
  #.nodgui.config:default-optimization
  (logand (ash color -24) #xff))

(u:definline extract-blue-component (color)
  (declare (fixnum color))
  #.nodgui.config:default-optimization
  (logand (ash color -8) #xff))

(u:definline extract-green-component (color)
  (declare (fixnum color))
  #.nodgui.config:default-optimization
  (logand (ash color -16) #xff))

(u:definline extract-alpha-component (color)
  (declare (fixnum color))
  #.nodgui.config:default-optimization
  (logand color #xff))

(defun not-inline-assemble-color (r g b &optional (a 255))
  (declare (notinline assemble-color))
  (assemble-color r g b a))

(define-compiler-macro assemble-color (&whole form r g b &optional (a 255))
  (let ((low-funname 'not-inline-assemble-color))
    (if (and (every #'constantp (list r g b a))
             (every #'numberp (list r g b a)))
        (funcall (symbol-function low-funname) r g b a)
        form)))

(u:definline assemble-color (r g b &optional (a 255))
  (declare ((unsigned-byte 8) r g b a))
  #.nodgui.config:default-optimization
  (logior (logand a #xff)
          (the fixnum (ash b 8))
          (the fixnum (ash g 16))
          (the fixnum (ash r 24))))

(u:definline set-color-alpha-channel (color alpha-channel)
  (logior (logand color #x00)
          alpha-channel))

(defun make-bits-array (pixmap width height)
  (let ((buffer (make-buffer width height)))
    (setf (slot-value pixmap 'bits) buffer)
    (tg:finalize pixmap
                 (lambda () (free-buffer-memory buffer)))))

(defun make-bits-array-elements (pixmap size)
  (let ((buffer (make-buffer-elements size)))
    (setf (slot-value pixmap 'bits) buffer)
    (tg:finalize pixmap
                 (lambda () (free-buffer-memory buffer)))))

(defclass pixmap ()
  ((data
    :initarg :data
    :initform (u:make-array-frame 0)
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
    :initform nil
    :accessor bits
    :initarg :bits
    :documentation "This  is the same  of 'data'  slots but as  a flat
    vector (ex: #(RRGGBBAA R1R1G1G1B1B1A1A1 ...)) Notes that this must
    be a simple-array of  (unsigned-byte 32), use 'make-bits-array' to
    ensure that this vector has the correct type."))
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
                 :data   (u:make-array-frame (* h w) bg 'ubvec4 t)))

(defun make-pixmap (w h &optional (bg (ubvec4 0 0 0 0)))
  "Instantiate a pixmap object of width 'w' and height 'h' filled with
 enough copies of the color bg."
  (assert (ubvec4p bg))
  (make-instance 'pixmap
                 :depth  4
                 :height h
                 :width  w
                 :data   (u:make-fresh-array (* h w) bg 'ubvec4 t)))

(defun copy-pixmap (object)
  "Make a copy of a pixmap"
  (make-instance 'pixmap
                 :depth  (depth object)
                 :width  (width object)
                 :height (height object)
                 :data   (a:copy-array (data object))))

(defun buffer->pixmap (buffer buffer-width buffer-height)
  (let ((pixmap (make-instance 'pixmap
                               :depth 4
                               :height buffer-height
                               :width  buffer-width
                               :bits   buffer)))
    (sync-bits-to-data pixmap)
    pixmap))

(defgeneric pixel@ (object x y))

(defgeneric (setf pixel@) (colorlist object x y))

(defgeneric bits-pixel@ (object x y))

(defgeneric (setf bits-pixel@) (color object x y))

(defgeneric (setf alpha-bits@) (alpha-value object x y))

(defgeneric sync-data-to-bits (object))

(defgeneric sync-bits-to-data (object))

(defgeneric pixmap->tga-file (object))

(defgeneric save-pixmap (object path))

(defgeneric encode-base64 (object))

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

(defgeneric to-grayscale (object))

(defgeneric to-disabled (object))

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
           (round (to:dlerp weight (coerce c1 'single-float) (coerce c2 'single-float))))
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
         (setf (pixel@ res x y) (u:round-all inter-y))))
    (change-class res (class-of object))
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
    (change-class res (class-of object))
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
    (change-class res (class-of object))
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
    (change-class res (class-of object))
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
    (change-class res (class-of object))
    res))

(defun repeat-periodic-coord (val max)
  (let ((fract  (- (abs val)
                   (truncate (abs val)))))
    (if (< val 0)
        (+ (1- max)
           (- (rem (floor (abs val)) max))
           (- fract))
        (+ (rem (floor val) max)
           fract))))

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
                            (pivot (vec2 (u:->f (/ (width object)  2))
                                         (u:->f (/ (height object) 2))))
                            (repeat nil)
                            (rounding-fn #'round))
  "Rotate a  pixmap about an  arbitrary angle and around  an arbitrary
  pivot (the center  of the image by default), The  'void' part of the
  image  will be  filled with  'fill-value' (a  'nodgui.ubvec4:ubvec4'
  vector) or with repeated parial clone  of the same image if `repeat'
  is non nil"
  (cond
    ;; using  the next tree functions because  the  usual  approach below  for
    ;; rotating did not worked for me
    ((u:epsilon= (u:->f angle) 90.0)
     (rotate-pixmap-90-degree-ccw object fill-value pivot))
    ((u:epsilon= (u:->f angle) -90.0)
     (rotate-pixmap-90-degree-cw object fill-value pivot))
    ((u:epsilon= (abs (u:->f angle)) 180.0)
     (rotate-pixmap-180-degree object fill-value pivot))
    (repeat
        (rotate-pixmap-w-repeat object angle :fill-value fill-value :pivot pivot
                                :rounding-fn rounding-fn))
    (t
     (let ((res (make-pixmap-frame (width object) (height object) fill-value))
           (act-angle (u:->f (u:deg->rad angle))))
       (loop-matrix (res x y)
         (let* ((new-pixel (vec2+ (vec2-rotate (vec2+ (vec2 x y)
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
                           (u:round-all px :rounding-function rounding-fn)
                           px)))))))
       (change-class res (class-of object))
       res))))

(defmethod rotate-pixmap-w-repeat ((object pixmap) angle
                                   &key (fill-value 0)
                                     (pivot (list (/ (width object) 2)
                                                  (/ (height object) 2)))
                                     (rounding-fn #'round))
  (let ((res       (make-pixmap-frame (width object) (height object) fill-value))
        (act-angle (u:->f (u:deg->rad angle))))
    (loop-matrix (res x y)
       (let* ((new-pixel (vec2+ (vec2-rotate (vec2+ (vec2 x y)
                                                    (vec2 (- (vec2-x pivot))
                                                          (- (vec2-y pivot))))
                                             act-angle)
                                (vec2 (vec2-x pivot)
                                      (vec2-y pivot)))))
         (let* ((px  (bilinear-interpolation object
                                             (elt new-pixel 0)
                                             (elt new-pixel 1)
                                             :interpolate-fn #'interpolate
                                             :behaivour-on-border-fn #'repeat-periodic-coord)))
           (setf (pixel@ res x y)
                 (if rounding-fn
                     (u:round-all px :rounding-function rounding-fn)
                     px)))))
    (change-class res (class-of object))
    res))

(defmethod sync-data-to-bits ((object pixmap))
  "Fill 'bits' slot of this pixmap  with the contents of 'data' slots"
  (with-accessors ((data data)
                   (bits bits)
                   (depth depth)
                   (width width)
                   (height height)) object
    (when (/= (length data)
              (length bits))
      (make-bits-array object width height))
    (loop for pixel-count from 0 below (length data) do
      (let* ((color-list (loop for channel-count from 0 below depth
                               collect
                               (elt (elt data pixel-count) channel-count)))
             (pixel (apply #'assemble-color color-list)))
        (setf (elt bits pixel-count) pixel)))
    object))

(defmethod sync-bits-to-data ((object pixmap))
  "Fill 'data' slot of this pixmap  with the contents of 'bits' slots"
  (with-accessors ((data   data)
                   (bits   bits)
                   (depth  depth)
                   (width  width)
                   (height height)) object
    (let ((data-size (* width height)))
      (when (/= (length data)
                (length bits))
        (setf data (u:make-array-frame data-size +ubvec4-zero+ 'ubvec4 t)))
      (loop for i from 0 below (length bits) do
        (let* ((pixel (elt bits i))
               (r     (extract-red-component   pixel))
               (g     (extract-green-component pixel))
               (b     (extract-blue-component  pixel))
               (a     (extract-alpha-component pixel)))
          (setf (elt data i) (ubvec4 r g b a)))))
    object))

(defmethod save-pixmap ((object pixmap) path)
  "Save bitmap in TARGA bitmap format"
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede :if-does-not-exist :create
                          :element-type +targa-stream-element-type+)
    (write-sequence (pixmap->tga-file object) stream))
  object)

(defmethod save-pixmap ((object pixmap) (path stream))
  "Save bitmap in TARGA bitmap format"
  (with-accessors ((bits bits)
                   (width  width)
                   (height height)) object
    (write-sequence (pixmap->tga-file object) path)
    object))

(defmethod encode-base64 ((object pixmap))
  (let ((pixmap-as-vector (flexi-streams:with-output-to-sequence (stream)
                            (save-pixmap object stream))))
    (nodgui.base64:encode pixmap-as-vector)))

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
  #.nodgui.config:default-optimization
  (declare (fixnum w x y))
  (the fixnum (+ (the fixnum (* w y)) x)))

(defmethod bits-pixel@ ((object pixmap) x y)
  "Get the color of pixel at specified coordinate from 'bits' slot"
  #.nodgui.config:default-optimization
  (declare (fixnum x y))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array (unsigned-byte 32)) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (elt bits offset))))

(defmethod (setf bits-pixel@) (color (object pixmap) x y)
  "Set the color of pixel at specified coordinate for 'bits' slot"
  #.nodgui.config:default-optimization
  (declare (fixnum x y))
  (declare (ubvec4 color))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (declare ((simple-array (unsigned-byte 32)) bits))
    (let* ((offset (%offset-bits width x y)))
      (declare (fixnum offset))
      (setf (elt bits offset)
            (assemble-color (elt color +red-channel+)
                            (elt color +green-channel+)
                            (elt color +blue-channel+)
                            (elt color +alpha-channel+))))
    object))

(defmethod (setf alpha-bits@) (alpha-value (object pixmap) x y)
  "Set  the alpha  component  for  the 'bits'  slot  only; value  is
an (unsigned-byte 8)"
  #.nodgui.config:default-optimization
  (declare (fixnum x y))
  (declare ((unsigned-byte 8) alpha-value))
  (with-accessors ((bits  bits)
                   (width width)) object
    (declare (fixnum width))
    (let* ((offset (%offset-bits width x y))
           (color  (elt bits offset)))
      (declare (fixnum offset))
      (declare ((unsigned-byte 32) color))
      (declare (dynamic-extent offset color))
      (setf (elt bits offset)
            (set-color-alpha-channel color alpha-value))
      object)))

(defun pixel-to-grayscale (pixel)
  (let ((value (floor (to:d+ (to:d* 0.2126f0 (to:d (elt pixel +red-channel+)))
                             (to:d* 0.7152f0 (to:d (elt pixel +green-channel+)))
                             (to:d* 0.0722f0 (to:d (elt pixel +blue-channel+)))))))
    (ubvec4 value value value (elt pixel +alpha-channel+))))

(defmethod to-grayscale ((object pixmap))
  (assert (or (= 4 (the fixnum (depth object)))
              (= 3 (the fixnum (depth object)))))
  (loop-matrix (object x y)
    (let ((grayscale-pixel (pixel-to-grayscale (pixel@ object x y))))
      (setf (pixel@ object x y) grayscale-pixel)))
  object)

(defmethod to-disabled ((object pixmap))
  (to-grayscale object)
  (loop with offset = 0
        for y fixnum from 0 below (height object) do
          (loop for x fixnum from 0 below (width object) do
            (let* ((shifted-x (rem (+ x offset)
                                  (width object))))
              (when (= (rem shifted-x 2)
                       0)
                  (let ((pixel (pixel@ object x y)))
                    (setf (elt pixel +alpha-channel+) 0)
                    (setf (pixel@ object x y) pixel)))))
          (incf offset 1))
  object)

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

(a:define-constant +targa-img-rgba-rle+                  10 :test 'equalp)

(a:define-constant +targa-img-rgba+                       2 :test 'equalp)

(a:define-constant +targa-img-header-size+               18 :test 'equalp)

(a:define-constant +targa-img-scanline-topleft+           2 :test 'equalp)

(a:define-constant +targa-img-scanline-bottomleft+        0 :test 'equalp)

(a:define-constant +targa-img-signature+ "TRUEVISION-XFILE" :test 'equalp)

(u:define-offset-size nodgui.pixmap
  targa-img (id-len 0 1) (type 2 1) (spec 8 10)
  (id +targa-img-header-size+) (colormap-spec 3 5))

(defclass tga (pixmap-file)
  ()
  (:documentation "A file in TARGA bitmap format"))

(a:define-constant +targa-footer-offset+ 26 :test #'=)

(defun file-tga-p (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((size (file-length stream)))
      (when (> size +targa-footer-offset+)
        (let ((footer (make-list +targa-footer-offset+)))
          (file-position stream (- size +targa-footer-offset+))
          (read-sequence footer stream)
          (let ((signature (subseq footer
                                   8
                                   (+ 8 (length nodgui.pixmap::+targa-img-signature+)))))
            (string= nodgui.pixmap::+targa-img-signature+
                     (coerce (mapcar #'code-char signature) 'string))))))))

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (pixmap-load object path)))

(defgeneric rearrange-scanline-by-pixmap-origin (object origin))

(defgeneric load-from-stream (object stream))

(defgeneric load-from-vector (object data))

(u:define-parse-header-chunk (image-id-len +targa-img-id-len-offset+
                              +targa-img-id-len-size+ tga nil))

(u:define-parse-header-chunk (image-type +targa-img-type-offset+
                              +targa-img-type-size+ tga nil))

(u:define-parse-header-chunk (image-specs +targa-img-spec-offset+
                              +targa-img-spec-size+ tga nil))

(u:define-parse-header-chunk (colormap-specs +targa-img-colormap-spec-offset+
                              +targa-img-colormap-spec-size+ tga nil))

(defun load-rearrange-raw-packet (offset bgra bits)
  (loop for data-ct from 0 below (length bgra) by 4
        for bits-ct from 0 do
          (let ((b (elt bgra data-ct))
                (g (elt bgra (+ data-ct 1)))
                (r (elt bgra (+ data-ct 2)))
                (a (elt bgra (+ data-ct 3))))
            (setf (elt bits (+ offset bits-ct))
                  (assemble-color r g b a)))))

(defmethod load-from-stream ((object tga) (stream stream))
  (with-accessors ((bits bits)) object
    (let ((type (first (parse-image-type object stream))))
      (if (or (= type +targa-img-rgba+)
              (= type +targa-img-rgba-rle+))
          (let* ((id-len          (first (parse-image-id-len object stream)))
                 (colormap-specs  (parse-colormap-specs object stream))
                 (colormap-len    (u:byte->int (subseq colormap-specs 2 4)))
                 (img-specs       (parse-image-specs object stream))
                 ;;(x-origin  (u:byte->int (subseq img-specs 0 2)))
                 ;;(y-origin  (u:byte->int (subseq img-specs 2 4)))
                 (width           (u:byte->int (subseq img-specs 4 6)))
                 (height          (u:byte->int (subseq img-specs 6 8)))
                 (depth           (/ (u:byte->int (subseq img-specs 8 9))
                                     8))
                 (img-descr       (first (subseq img-specs 9 10)))
                 (alpha-size      (boole boole-and img-descr #x0e))
                 (scanline-origin (ash (boole boole-and img-descr #x30) -4))
                 (scanline-offset (+ +targa-img-header-size+ id-len colormap-len))
                 (scanlines-size  (* width height depth))
                 (bgra-scanline   (u:make-array-frame (* width depth))))
            (if (/= 8 alpha-size)
                (push (format nil "Alpha bitsize should be 8 instead of ~x" alpha-size)
                      (errors object))
                (progn
                  (setf (width object)  width
                        (height object) height
                        (depth object) depth)
                  (make-bits-array object (width object) (height object))
                  (file-position stream scanline-offset)
                  (let ((bits-offset-count 0))
                    (if (= type +targa-img-rgba+) ;; no compression RLE
                        (loop for i from 0 below height do
                          (read-sequence bgra-scanline stream)
                          (load-rearrange-raw-packet (* i width) bgra-scanline (bits object)))
                        (loop for i from 0 below (/ scanlines-size depth) do
                          (let* ((packet-head (read-byte stream))
                                 (packet-type (boole boole-and packet-head #x80))
                                 (packet-count (boole boole-and packet-head #x7f)))
                            (if (= packet-type #x80) ;; rle packet
                                (let ((b (read-byte stream))
                                      (g (read-byte stream))
                                      (r (read-byte stream))
                                      (a (read-byte stream)))
                                  (loop for px from 0 to packet-count do
                                    (setf (elt bits (+ bits-offset-count px))
                                          (assemble-color r g b a)))
                                  (incf bits-offset-count (1+ packet-count)))
                                (progn
                                  (loop for px from 0 to packet-count
                                        do ;; raw packet
                                           (let ((b (read-byte stream))
                                                 (g (read-byte stream))
                                                 (r (read-byte stream))
                                                 (a (read-byte stream)))
                                             (setf (elt bits (+ bits-offset-count px))
                                                   (assemble-color r g b a))))
                                  (incf bits-offset-count (1+ packet-count))))
                            (incf i packet-count)))))
                  (rearrange-scanline-by-pixmap-origin object scanline-origin)
                  (sync-bits-to-data object)
                  object)))
          (push "Image type not supported: only rgba and compressed rgba allowed."
                (errors object))))))

(defmethod pixmap-load ((object tga) (file string))
  (with-open-file (stream file :element-type +targa-stream-element-type+
                               :if-does-not-exist :error)
    (load-from-stream object stream)))

(defmethod rearrange-scanline-by-pixmap-origin ((object tga) origin)
  (with-accessors ((height height)
                   (bits bits)
                   (width width)
                   (depth depth)) object
    (macrolet ((swap-bits (a b)
                 `(rotatef (elt bits ,a) (elt bits ,b))))
      (cond
        ((= origin +targa-img-scanline-topleft+)
         t)
        ((= origin +targa-img-scanline-bottomleft+)
         (let ((scanline-length width))
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
  (let ((results (u:make-array-frame 0 +targa-stream-element-type+)))
    (vector-push-extend #x0 results) ; image id has zero length
    (vector-push-extend #x0 results) ; no colormap
    (vector-push-extend #x0a results) ; we use only truecolor image also we want them compressed
    (write-bytes results '(#x00 #x00 #x00 #x00 #x00)) ; color map specification
                                                      ; all zero for true color image
    (setf results (write-bytes results '(#x00 #x00))) ; x-origin
    (setf results (write-bytes results '(#x00 #x00))) ; y-origin
    (setf results (write-bytes results (u:int16->bytes (width object))))
    (setf results (write-bytes results (u:int16->bytes (height object))))
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

#-nodgui-lite
(defclass jpeg (pixmap-file)
  ()
  (:documentation "A pixmap stored in JPG format"))

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (pixmap-load object path)))

(defun fill-bits-rgb (pixmap uncompressed-data image-w image-h)
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) pixmap
    (let ((new-data (u:make-array-frame (* image-w image-h) (ubvec4 0 0 0 0) 'ubvec4 t)))
          (loop
            for i from 0 below (length uncompressed-data) by 3
            for j from 0 below (length new-data) by 1 do
              (setf (elt new-data j)
                    (ubvec4 (elt uncompressed-data    i)
                            (elt uncompressed-data (+ i 1))
                            (elt uncompressed-data (+ i 2))
                            255)))
      (setf data   new-data
            width  image-w
            height image-h)
      (sync-data-to-bits pixmap)
      pixmap)))

#-nodgui-lite
(defmethod pixmap-load ((object jpeg) (file string))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (jpeg-turbo:with-decompressor (jpeg-handle)
      (multiple-value-bind (image-w image-h)
          (jpeg-turbo:decompress-header jpeg-handle file)
        (let ((uncompressed-data (jpeg-turbo:decompress jpeg-handle file)))
          (fill-bits-rgb object uncompressed-data image-w image-h))))))

#-nodgui-lite
(defmethod load-from-stream ((object jpeg) (stream stream))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (let ((raw-data (u:slurp-stream-into-array stream)))
      (load-from-vector object raw-data))))

#-nodgui-lite
(defmethod load-from-vector ((object jpeg) (stream vector))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (jpeg-turbo:with-decompressor (jpeg-handle)
      (multiple-value-bind (image-w image-h)
          (jpeg-turbo:decompress-header-from-octets jpeg-handle stream)
        (let ((uncompressed-data (jpeg-turbo:decompress-from-octets jpeg-handle stream)))
          (fill-bits-rgb object uncompressed-data image-w image-h))))))

#-nodgui-lite
(defmethod save-pixmap ((object jpeg) path)
  "Save bitmap in JPG bitmap format"
  (with-accessors ((bits bits)
                   (width  width)
                   (height height)) object
    (jpeg-turbo:with-compressor (jpeg-handle)
      (jpeg-turbo:compress jpeg-handle path bits width height :rgba))
    object))

#-nodgui-lite
(defmethod save-pixmap ((object jpeg) (path stream))
  "Save bitmap in JPG bitmap format"
  (with-accessors ((bits bits)
                   (width  width)
                   (height height)) object
    (jpeg-turbo:with-compressor (jpeg-handle)
      (let ((jpg-as-vector (jpeg-turbo:compress-to-octets jpeg-handle
                                                          bits
                                                          width
                                                          height
                                                          :rgba)))
        (write-sequence jpg-as-vector path)))
    object))

(defclass png (pixmap-file)
  ()
  (:documentation "A pixmap stored in PNG format"))

(defmethod initialize-instance :after ((object tga) &key (path nil) &allow-other-keys)
  (when path
    (pixmap-load object path)))

(defmethod pixmap-load ((object png) (file string))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
      (load-from-stream object stream))))

(defun fill-bits-rgba (pixmap uncompressed-data image-w image-h)
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) pixmap
    (let ((new-data (u:make-array-frame (* image-w image-h) (ubvec4 0 0 0 0) 'ubvec4 t)))
      (loop
        for i from 0 below (length uncompressed-data) by 4
        for j from 0 below (length new-data) by 1 do
          (setf (elt new-data j)
                (ubvec4 (elt uncompressed-data    i)
                        (elt uncompressed-data (+ i 1))
                        (elt uncompressed-data (+ i 2))
                        (elt uncompressed-data (+ i 3)))))
      (setf data   new-data
            width  image-w
            height image-h)
      (sync-data-to-bits pixmap)
      pixmap)))

(defmethod load-from-stream ((object png) (stream stream))
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (let* ((png-decoded  (pngload:load-stream stream :flatten t))
           (image-width  (pngload:width png-decoded))
           (image-height (pngload:height png-decoded))
           (data         (pngload:data png-decoded)))
      (cond
        ((eq (pngload:color-type png-decoded) :truecolour)
         (fill-bits-rgb object data image-width image-height))
        ((or (eq (pngload:color-type png-decoded) :truecolour-alpha)
             (eq (pngload:color-type png-decoded) :indexed-colour))
         (fill-bits-rgba object data image-width image-height))
        (t
         (error "Unsupported color space ~s" (pngload:color-type png-decoded)))))))

(defmethod load-from-vector ((object png) (data vector))
  (flexi-streams:with-input-from-sequence (stream data)
    (load-from-stream object stream)))

(defmethod save-pixmap ((object png) path)
  "Save bitmap in PNG bitmap format"
  (with-accessors ((bits bits)
                   (width  width)
                   (height height)) object
    (with-open-file (stream
                     path
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
      (save-pixmap object stream)
      object)))

(defmethod save-pixmap ((object png) (path stream))
  "Save bitmap in PNG bitmap format"
  (with-accessors ((data   data)
                   (width  width)
                   (height height)) object
    (let ((png (make-instance 'zpng:pixel-streamed-png
                              :color-type :truecolor-alpha
                              :width      width
                              :height     height)))
      (zpng:start-png png path)
      (loop for pixel across data do
        (zpng:write-pixel pixel png))
      (zpng:finish-png png)
      object)))

(alexandria:define-constant +file-matrix-buff-size+    2048               :test '=)

(alexandria:define-constant +file-matrix-element-type+ '(unsigned-byte 32) :test 'equalp)

(defclass file-matrix (pixmap)
  ((file-path
    :initform ""
    :initarg  :file-path
    :accessor file-path)
   (stream-handle
    :initform nil
    :initarg  :stream-handle
    :accessor stream-handle)
   (block-size
    :initform 4
    :initarg  :block-size
    :accessor block-size
    :type     integer)))

(defmethod print-object ((object file-matrix) stream)
  (with-accessors ((file-path     file-path)
                   (block-size    block-size)
                   (stream-handle stream-handle)
                   (width         width)
                   (height        height)) object
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~aX~a bs: ~a stream: ~a" width height block-size stream-handle))))

(defun calc-file-matrix-size-elements (fm)
  (with-accessors ((block-size    block-size)
                   (stream-handle stream-handle)
                   (width         width)
                   (height        height)) fm
    (let ((size (* width height block-size)))
      (multiple-value-bind (block-num remainder)
          (floor (/ size +file-matrix-buff-size+))
        (values block-num ;; number of +file-matrix-buff-size+
                (* remainder +file-matrix-buff-size+)))))) ; number of elements (32 bits)!

(defun fm-vector-type-fn (fm)
  (with-accessors ((block-size block-size)) fm
    (cond
      ((= block-size 1)
       #'(lambda () (vector 0)))
      ((= block-size 2)
        #'(lambda () (vector 0 0)))
      ((= block-size 4)
       #'(lambda () (ubvec4 0 0 0 0)))
      (t
       (error "Only 1, 2 or 4 blocksize allowed")))))

(defun fill-file-matrix-size (fm)
  (with-accessors ((block-size    block-size)
                   (stream-handle stream-handle)
                   (width         width)
                   (height        height)) fm
      (multiple-value-bind (block-counts elements-left)
          (calc-file-matrix-size-elements fm)
        (let ((buff (u:make-array-frame +file-matrix-buff-size+
                                        0
                                        +file-matrix-element-type+
                                        t)))
          (loop repeat block-counts do
               (write-sequence buff stream-handle))
          (loop repeat elements-left do
               (write-byte 0 stream-handle))))
      (finish-output stream-handle)
      (file-position stream-handle 0))
  fm)

(defun make-file-matrix (file width height &key (fill nil) (block-sz 4))
  (let ((res (make-instance 'file-matrix
                            :width  width
                            :height height)))
    (with-accessors ((file-path     file-path)
                     (block-size    block-size)
                     (stream-handle stream-handle)
                     (width         width)
                     (height        height)) res
      (if (u:file-exists-p file) ; file exists
          (let ((new-stream (open file
                                  :direction         :io
                                  :if-exists         :overwrite
                                  :if-does-not-exist :create
                                  :element-type      +file-matrix-element-type+)))
            (setf stream-handle new-stream)
            (setf file-path     file)
            (setf block-size    block-sz)
            (when fill                           ; fill the file with 0's
              (fill-file-matrix-size res))
            (file-position stream-handle 0)
            res)
          (progn                                 ; create and fill a new file
            (u:create-file file)
            (make-file-matrix file
                              width
                              height
                              :fill     t
                              :block-sz block-sz))))))

(defun close-file-matrix (fm)
  (with-accessors ((stream-handle stream-handle)) fm
    (when stream-handle
      (finish-output stream-handle)
      (setf stream-handle nil)))
  fm)

(defun calc-offset (block-size width x y)
  (* block-size (+ (* width y) x)))

(defmethod pixel@ ((object file-matrix) x y)
  (with-accessors ((block-size    block-size)
                   (stream-handle stream-handle)
                   (width         width)
                   (height        height)) object
    (let ((res    (funcall (fm-vector-type-fn object)))
          (offset (calc-offset block-size width x y)))
      (loop for i from 0 below block-size do
           (file-position stream-handle (+ offset i))
           (setf (elt res i)
                 (read-byte stream-handle)))
      res)))

(defmethod (setf pixel@) (color (object file-matrix) x y)
  (with-accessors ((block-size    block-size)
                   (stream-handle stream-handle)
                   (width         width)
                   (height        height)) object
    (let ((offset (calc-offset block-size width x y)))
      (file-position stream-handle offset)
      (write-sequence color stream-handle)
      object)))

(defmethod sync-data-to-bits ((object file-matrix))
  "Fill 'bits' slot of this pixmap  with the contents of 'data' slots"
  (multiple-value-bind (block-count elements-left)
      (calc-file-matrix-size-elements object)
    (with-accessors ((bits          bits)
                     (stream-handle stream-handle)) object
      (make-bits-array-elements object
                                (+ (* block-count
                                      +file-matrix-buff-size+)
                                   elements-left))
      (file-position stream-handle 0)
      (loop for bytes-offset from 0 below elements-left do
        (setf (elt bits bytes-offset)
              (read-byte stream-handle)))
      object)))

(defmacro with-file-matrix ((name file w h &key (block-size 4)) &body body)
  `(let ((,name (make-file-matrix ,file ,w ,h :block-sz ,block-size)))
     (unwind-protect
          (progn ,@body)
       (close-file-matrix ,name))))
