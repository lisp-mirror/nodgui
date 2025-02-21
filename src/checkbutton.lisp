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

(defargs check-button (widget)
  cbcommand
  compound
  image
  offvalue
  onvalue
  state
  textvariable
  underline
  variable
  width)

(defwrapper check-button (widget tkvariable) () "ttk::checkbutton")

(defmethod (setf command) (val (check-button check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
               (name check-button) (name check-button))
  val)

(defmethod value ((v check-button))
  (with-read-data (nil)
    (format-wish "global ~a; senddata $~a" (name v) (name v))
    (if (equal 1 (read-data))
        t
        nil)))

(defmethod (setf value) (val (v check-button))
    (when (and (numberp val)
               (= val 0))
      (warn (strcat "Use of 0 for check-button values will be treated as true,"
                    " so the checkbutton state will be \"on\"")
            val))
    (format-wish "global ~a; set ~a {~a}" (name v) (name v) (if val 1 0))
    val)

(defargs classic-check-button (widget)
  activebackground
  activeforeground
  anchor
  background
  bitmap
  borderwidth
  compound
  disabledforeground
  font
  foreground
  highlightbackground
  highlightcolor
  highlightthickness
  image
  justify
  padx
  pady
  relief
  textvariable
  underline
  wraplength
  command
  height
  indicatoron
  offrelief
  offvalue
  onvalue
  overrelief
  selectcolor
  selectimage
  state
  tristateimage
  tristatevalue
  variable
  width)

(defwrapper classic-check-button (widget tkvariable) () "checkbutton")

(defmethod (setf command) (val (check-button classic-check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
               (name check-button) (name check-button))
  val)

(defmethod value ((v classic-check-button))
  (with-read-data (nil)
    (format-wish "global ~a; senddata $~a" (name v) (name v))
    (if (equal 1 (read-data))
        t
        nil)))

(defmethod (setf value) (val (v classic-check-button))
    (when (and (numberp val)
               (= val 0))
      (warn (strcat "Use of 0 for check-button values will be treated as true,"
                    " so the checkbutton state will be \"on\"")
            val))
    (format-wish "global ~a; set ~a {~a}" (name v) (name v) (if val 1 0))
    val)

(defclass image-check-button (check-button)
  ((image-selected
    :initform nil
    :initarg  :image-selected
    :accessor image-selected)
   (image-unselected
    :initform nil
    :initarg  :image-unselected
    :accessor image-unselected)))

(defun image-check-button-state->image (button state)
  (if state
      (configure button :image (image-selected button))
      (configure button :image (image-unselected button))))

(defun build-image-check-button-command (button command)
  (lambda (state)
    (image-check-button-state->image button state)
    (when command
      (funcall command state))))

(defmethod initialize-instance :after ((object image-check-button) &key &allow-other-keys)
  (with-accessors ((image-selected   image-selected)
                   (image-unselected image-unselected)) object
    (setf (command object)
          (build-image-check-button-command object (command object)))
    object))

(defmethod (setf command) (command-fn (object image-check-button))
  (add-callback (name object)
                (build-image-check-button-command object command-fn))
  (format-wish "~a configure -command {callbackval ~a $~a}"
                   (widget-path object)
                   (name object)
                   (name object))
  command-fn)

(defmethod (setf value) :after (val (object image-check-button))
  (image-check-button-state->image object val)
  val)
