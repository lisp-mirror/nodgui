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

(defclass photo-image (tkobject)
  ((data
    :accessor data
    :initform nil
    :initarg :data)
   (file
    :accessor file
    :initform nil
    :initarg :file)))

(defmethod widget-path ((photo photo-image))
  (name photo))

(defmethod initialize-instance :after ((object photo-image) &key &allow-other-keys)
  (with-accessors ((data data)) object
    (setf (name object) (create-name))
    (format-wish "image create photo ~a" (name object))
    (when data
      (with-read-data ()
        (if (stringp data)
            (format-wish (tclize `(senddatastring [ ,(name object) " "
                                                  put {+ ,data } ])))
            (format-wish (tclize `(senddatastring [ ,(name object) " " put
                                                  ;; no  need to  escape
                                                  ;; because      base64
                                                  ;; encoded   data  can
                                                  ;; not        contains
                                                  ;; problematic
                                                  ;; characters
                                                  ,(nodgui.base64:encode data)
                                                  ]))))))))

(defgeneric make-image (data &optional w h channels))

(defun make-image-data (data-bits w h channels &key (column-offset channels))
  " (make-image-data #( 1  2  3   4  5  6  7  8  9
                      10 11 12  13 14 15 16 17 18)
                    3 2 3)
   ===> [list [list #010203 #040506 #070809 ] [list #0A0B0C #0D0E0F #101112 ] ]"
  (let ((*suppress-newline-for-tcl-statements* t)
        (raw-width                             (the fixnum (* column-offset w)))
        (row-list                              (make-fresh-array (* w (+ (* channels 2)
                                                                         2))
                                                                 #\a
                                                                 'character
                                                                 nil)))
    (declare (fixnum raw-width))
    (flet ((make-row (r)
             (declare (fixnum r))
             (setf (fill-pointer row-list) 0)
             (with-output-to-string (stream row-list)
               (loop
                  for c fixnum from 0 below raw-width by column-offset do
                    (write-char #\# stream)
                    (loop for i fixnum from 0 below channels do
                         (let ((pix-color (elt data-bits
                                               (+ (* r w
                                                     column-offset)
                                                  c i))))
                           (format stream "~2,'0x" pix-color)))
                    (write-char #\Space stream))
               row-list)))
      (tclize `([list
                ,(loop for r from 0 below h collect
                      (tclize `([list ,(make-row r) ])
                              :sanitize nil))
                ])
              :sanitize nil))))

(defmethod make-image ((object string) &optional (w nil) (h nil) (channels 3))
  "If object is a valid local file path is threated as a  pathname, otherwise the
string must  be base64 encoded image.  Note in both cases  only PNG or
GIF are supported but if tk-img is used more format are available!"
  (declare (ignore w h channels))
  (if (file-exists-p object)
      (make-image (namestring->pathname object))
      (make-instance 'photo-image
                     :data object)))

(defparameter *use-tk-for-decoding-png* nil
  "Set this variable to non null to force decoding the PNGs with TK and
repscts the alpha channnel" )

(defmethod make-image ((object vector) &optional (w nil) (h nil) (channels 3))
  (flet ((make-image-in-tcl ()
           (with-read-data (nil)
             (let ((*max-line-length* nil)
                   (res (make-instance 'photo-image))
                   (data-bits (make-image-data object
                                               w
                                               h
                                               channels
                                               :column-offset channels)))
               (with-send-batch
                   (format-wish (tclize `(senddatastring [ ,(sanitize (name res)) " "
                                                         put
                                                         ,data-bits
                                                         ])
                                        :sanitize nil)))
               (read-data)
               res))))
    (cond
      ((or *tkimg-loaded-p*
           (gifp object))
       (make-instance 'photo-image
                      :data (nodgui.base64:encode object)))
      ((pngp object)
       (if *use-tk-for-decoding-png*
           (make-instance 'photo-image
                          :data (nodgui.base64:encode object))
           (make-image (load-from-vector (make-instance 'png) object))))
      ((jpgp object)
       (make-image (load-from-vector (make-instance 'jpeg) object)))
      (t
       (make-image-in-tcl)))))

(defun bits-pixmap->bits-octets (bits-pixmap width height channels)
  (let ((bytes (make-fresh-array (* width height channels) #x00 '(unsigned-byte 8) t)))
    (loop for pixmap-index from 0 below (* width height)
          for octet-index from 0 by channels do
      (let* ((pixel (elt bits-pixmap pixmap-index))
             (r     (nodgui.pixels-canvas:extract-red-component   pixel))
             (g     (nodgui.pixels-canvas:extract-green-component pixel))
             (b     (nodgui.pixels-canvas:extract-blue-component  pixel)))
        (setf (elt bytes    octet-index)    r
              (elt bytes (+ octet-index 1)) g
              (elt bytes (+ octet-index 2)) b)))
    bytes))

(defmethod make-image ((object pixmap) &optional (w nil) (h nil) (channels 4))
  (declare (ignore w h channels))
  (with-read-data (nil)
    (sync-data-to-bits object)
    (let* ((res (make-instance 'photo-image)))
      (let ((*max-line-length* nil)
            (data-bits (make-image-data (bits-pixmap->bits-octets (bits object)
                                                                  (width  object)
                                                                  (height object)
                                                                  3)
                                        (width  object)
                                        (height object)
                                        3
                                        :column-offset 3)))
        (with-send-batch
            (format-wish (tclize `(senddatastring [ ,(sanitize (name res)) " "
                                                  put
                                                  ,data-bits
                                                  ])
                                 :sanitize nil)))
        (read-data)
        res))))

(defmethod make-image ((object pathname) &optional (w nil) (h nil) (channels nil))
  (declare (ignore w h channels))
  (with-open-file (stream object :element-type '(unsigned-byte 8))
    (cond
      ((file-tga-p object)
       (let ((pixmap (slurp-pixmap 'tga (file-namestring object))))
         (make-image pixmap)))
      ((file-jpg-p object)
       (let ((pixmap (slurp-pixmap 'jpeg (file-namestring object))))
         (make-image pixmap)))
      ((file-png-p object)
       (let ((pixmap (slurp-pixmap 'png (file-namestring object))))
         (make-image pixmap)))
      (t
       (let ((data (nodgui.utils:read-into-array stream (file-length stream))))
         (make-image data))))))

(defgeneric image-load (p filename))

(defmethod image-load ((p photo-image) filename)
  (format-wish "~A read {~A} -shrink" (name p) filename)
  p)

(defgeneric image-scale (object scale-x &optional scale-y))

(defmethod image-scale ((object photo-image) (scale-x fixnum) &optional (scale-y scale-x))
  "Scale the  image, if both  scaling factors are positive  numbers this
function  will  upscale  the  image, a  downscale  will  be  performed
otherwise"
  (assert (/= 0 scale-x))
  (assert (/= 0 scale-y))
  (let ((new-image (make-instance 'photo-image)))
    (with-no-emitted-newline
      (format-wish (tclize
                    `(,(name new-image) " " copy " " ,(name object) " "
                      ,(if (or (< scale-x 0)
                               (< scale-y 0))
                           (tclize `(-subsample))
                           (tclize `(-zoom)))
                      ,(abs scale-x) ,(abs scale-y)))))
    (setf (name object) (name new-image))
    object))
