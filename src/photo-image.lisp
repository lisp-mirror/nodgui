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

(defmethod initialize-instance :after ((p photo-image) &key &allow-other-keys)
  (setf (name p) (create-name))
  (format-wish "image create photo ~a" (name p)))

(defun make-image-data (data w h channels &key (column-offset channels))
 " (make-image-data #( 1  2  3   4  5  6  7  8  9
                      10 11 12  13 14 15 16 17 18)
                    3 2 3)
   ===> [list [list #010203 #040506 #070809 ] [list #0A0B0C #0D0E0F #101112 ] ]"
 (let ((*suppress-newline-for-tcl-statements* t)
        (raw-width                            (* column-offset w)))
    (tclize `([list
              ,(loop for r from 0 below h collect
                    (tclize `([list ,(loop
                                        for c from 0 below raw-width by column-offset
                                        collect
                                          (let ((res "#"))
                                            (loop for i from 0 below channels do
                                                 (let ((pix-color (elt data
                                                                       (+ (* r w
                                                                             column-offset)
                                                                          c i))))
                                                   (setf res (strcat res
                                                                     (format nil "~2,'0x"
                                                                             pix-color)))))
                                            (tclize (strcat res " "))))
                                    ])))
              ]))))

(defgeneric make-image (data &optional w h channels))

(defmethod make-image ((data vector) &optional (w nil) (h nil) (channels 3))
  (let* ((res (make-instance 'photo-image)))
    (when data
      (cond
        ((or (pngp data)
             (gifp data))
         (let ((encoded (nodgui.base64:encode data)))
           (format-wish (format nil (tcl-str (~a put {~a}))
                                (name res)
                                encoded))))
        (t ; raw image data
         (let ((*max-line-length* nil))
           (with-atomic
               (format-wish (tclize `(senddatastring [ ,(name res) " "
                                                     put
                                                     ,(make-image-data data w h channels)
                                                     ]))))))))
    res))

(defmethod make-image ((object pixmap) &optional (w nil) (h nil) (channels 4))
  (declare (ignore w h channels))
  (sync-data-to-bits object)
  (let* ((res (make-instance 'photo-image)))
    (let ((*max-line-length* nil))
      (with-atomic
          (format-wish (tclize `(senddatastring [ ,(name res) " "
                                                put
                                                ,(make-image-data (bits   object)
                                                                  (width  object)
                                                                  (height object)
                                                                  3
                                                                  :column-offset 4)
                                                ])))))
    res))

(defgeneric image-load (p filename))

(defmethod image-load((p photo-image) filename)
  (send-wish (format nil "~A read {~A} -shrink" (name p) filename))
  p)
