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

(define-constant +reference-text-width-char+ "0" :test #'string=
                 :documentation
                 "According to the documentation this is the reference size for char width")

(defargs text ()
  autoseparators
  background
  borderwidth
  cursor
  exportselection
  font
  foreground
  height
  highlightbackground
  highlightcolor
  highlightthickness
  insertbackground
  insertborderwidth
  insertofftime
  insertontime
  insertwidth
  maxundo
  padx
  pady
  relief
  selectbackground
  selectborderwidth
  selectforeground
  setgrid
  spacing1
  spacing2
  spacing3
  state
  tabs
  takefocus
  undo
  width
  wrap
  xscrollcommand
  yscrollcommand)

;;; text widget

(defwrapper text (widget)
  ((xscroll
    :accessor xscroll
    :initarg  :xscroll
    :initform nil)
   (yscroll
    :accessor yscroll
    :initarg  :yscroll
    :initform nil))
  "text")

(defmethod cursor-index ((text text))
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (let* ((index (split-sequence (read-data) ".")))
    (values (parse-integer  (first index))
            (parse-integer  (second index)))))

(defun make-text (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'text
                 :master  master
                 :width   width
                 :height  height
                 :xscroll xscroll
                 :yscroll yscroll))


(defgeneric clear-text (txt))

(defgeneric append-text (txt text &rest tags))

(defgeneric search-all-text (text pattern))

(defgeneric search-next-text (text pattern))

(defgeneric save-text (txt filename &key if-exists if-does-not-exist))

(defgeneric load-text (txt filename))

(defmethod append-text ((txt text) text &rest tags)
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path txt) text tags)
  txt)

(defmethod insert-object ((txt text) obj)
  (format-wish "~a window create end -window ~a" (widget-path txt) (widget-path obj))
  txt)

(defun append-newline (text)
  (append-text text (coerce '(#\Linefeed) 'string)))

(defmethod clear-text ((txt text))
  (format-wish "~A delete 0.0 end" (widget-path txt))
  txt)

(defmethod see ((txt text) pos)
  (format-wish "~a see {~(~a~)}" (widget-path txt) pos)
  txt)

(defmethod search-all-text ((txt text) pattern)
  (format-wish "searchall ~a {~a}" (widget-path txt) pattern)
  txt)

(defmethod search-next-text ((txt text) pattern)
  (format-wish "searchnext ~a {~a}" (widget-path txt) pattern)
  txt)

(defmethod tag-configure ((txt text) tag option value &rest others)
  (format-wish "~a tag configure {~a}~{ {-~(~a~)} {~a}~}"
               (widget-path txt)
               (if (stringp tag)
                   tag
                   (format nil "~(~a~)" tag))
               (mapcar #'down (list* option value others)))
  txt)


(defmethod tag-bind ((object text) tag event fun &key (exclusive nil))
  "bind fun to event of the tag of the text widget object"
  (declare (ignore exclusive))
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a tag bind {~a} {~a} {callback ~A}" (widget-path object) tag event name))
  object)

(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end]" (widget-path text))
  (read-data))

(defmethod (setf text) (val (text text))
  (format-wish "~A delete 0.0 end;~A insert end {~A}"
               (widget-path text) (widget-path text) val)
  val)

(defmethod save-text ((txt text) filename
                      &key
                        (if-exists         :supersede)
                        (if-does-not-exist :create))
  "save the content of the text widget into the file <filename>"
  (let ((data (text txt)))
    (with-open-file (stream filename
                            :direction         :output
                            :if-exists         if-exists
                            :if-does-not-exist if-does-not-exist)
      (write-sequence data stream)))
  txt)

(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
  (setf (text txt) (alexandria:read-file-into-string filename))
  txt)

;;; scrolled-text

(defclass scrolled-text (frame)
  ((inner-text
    :initform nil
    :initarg  :inner-text
    :accessor inner-text)
   (hscroll
    :initform nil
    :initarg  :hscroll
    :accessor hscroll)
   (vscroll
    :initform nil
    :initarg  :vscroll
    :accessor vscroll)))

(defun make-scrolled-text (master)
  (make-instance 'scrolled-text :master master))

(defmethod initialize-instance :after ((object scrolled-text)
                                       &key (use-horizontal-scrolling-p t)
                                         &allow-other-keys)
  (with-accessors ((inner-text inner-text)
                   (hscroll    hscroll)
                   (vscroll    vscroll)) object

    (setf vscroll (make-scrollbar object))
    (setf inner-text  (make-text object
                                 :xscroll hscroll
                                 :yscroll vscroll))
    (grid inner-text  0 0 :sticky :news)

    (grid vscroll 0 1     :sticky :ns)
    (grid-columnconfigure object 0 :weight 1)
    (grid-columnconfigure object 1 :weight 0)
    (grid-rowconfigure    object 0 :weight 1)
    (grid-rowconfigure    object 1 :weight 0)
    (when use-horizontal-scrolling-p
      (setf hscroll (make-scrollbar object :orientation "horizontal"))
      (grid hscroll 1 0     :sticky :we)
      (configure hscroll
                 "command"
                 (concatenate 'string (widget-path inner-text) " xview")))
    (configure inner-text
               "xscrollcommand"
               (concatenate 'string (widget-path hscroll) " set"))
    (configure vscroll
               "command"
               (concatenate 'string (widget-path inner-text) " yview"))
    (configure inner-text
               "yscrollcommand"
               (concatenate 'string (widget-path vscroll) " set"))))

(defmacro with-inner-text ((text-slot scrolled-text) &body body)
  "Syntatic sugar to  access the text slot of a scrolled-text

  `(with-accessors ((,text-slot inner-text)) ,scrolled-text
     ,@body))"
  `(with-accessors ((,text-slot inner-text)) ,scrolled-text
     ,@body))

(defmethod append-text ((txt scrolled-text) text &rest tags )
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}"
               (widget-path (inner-text txt))
               text
               tags)
  txt)

(defmethod text ((self scrolled-text))
  (text (inner-text self)))

(defmethod (setf text) (new-text (self scrolled-text))
  (setf (text (inner-text self)) new-text))

(defgeneric insert-object (txt object))

(defmethod insert-object ((txt scrolled-text) obj)
  (format-wish "~a window create end -window ~a"
               (widget-path (inner-text txt))
               (widget-path obj))
  txt)

(defgeneric see (txt pos)
  (:documentation "Makes sure the widget is visible"))

(defmethod see ((txt scrolled-text) pos)
  (format-wish "~a see {~(~a~)}" (widget-path (inner-text txt)) pos)
  txt)

(defgeneric fit-words-to-text-widget (object text font))

(defmethod fit-words-to-text-widget ((object scrolled-text) text font)
  "This method will split 'text' into words  o and will fits it to the
width  of  the  object  this   method  specializes,  words  are  never
splitted (i.e. no hyphenation)"
  (with-inner-text (text-widget object)
    (let* ((words (split-words text))
           (width-widget (* (width text-widget)
                            (font-measure font
                                          "0"
                                          :display-of text-widget))))
      (labels ((text-screen-width (text)
                 (font-measure font
                               (subseq text 0 (1- (length text)))
                               :display-of text-widget))
               (maybe-add-space (text add-space-p)
                 (if add-space-p
                     text
                     (append text (list " "))))
               (find-last-world-pos (l)
                 (position-if (lambda (a) (cl-ppcre:scan "^\\P{White_Space}+$" a))
                              l
                              :from-end t))
               (wordp (a)
                 (cl-ppcre:scan "^\\P{White_Space}+$" a))
               (exceed-limits-p (s)
                 (>= (text-screen-width (join-with-strings s ""))
                     width-widget))
               (last-word (s)
                 (elt s (find-last-world-pos s)))
               (append-new-line (l)
                 (append l (list (format nil "~%"))))
               (%subdivide (text-to-split splitted-so-far)
                 (if text-to-split
                     (multiple-value-bind (splitted rest-to-split)
                         (do* ((first-iteration-p     t  nil)
                               (others text-to-split  (rest others))
                               (single-word-p (<= (length others) 1)
                                              (<= (length others) 1))
                               (so-far (maybe-add-space (list (first others)) single-word-p)
                                       (maybe-add-space (append so-far (list (first others)))
                                                        single-word-p)))
                              ((and (not first-iteration-p)
                                    (or (exceed-limits-p so-far)
                                        (null others)))
                               (values
                                (if (exceed-limits-p so-far)
                                    (if (null others)
                                        (append-new-line so-far)
                                        (append-new-line (subseq so-far 0
                                                                 (find-last-world-pos so-far))))
                                    so-far)
                                (if (exceed-limits-p so-far)
                                    (append (list (last-word so-far))
                                            others
                                            (list " "))
                                    others))))
                       (%subdivide (remove-if-not #'wordp (rest rest-to-split))
                                   (append splitted-so-far splitted)))
                     splitted-so-far)))
        (join-with-strings (%subdivide words '()) "")))))
