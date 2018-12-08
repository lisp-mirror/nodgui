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
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil))
  "text")

(defmethod cursor-index ((text text))
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (let* ((index (split-sequence (read-data) ".")))
        (values (parse-integer  (first index))
                (parse-integer  (second index)))))

(defun make-text (master &key (width nil) (height nil))
  (make-instance 'text :master master :width width :height height))

(defmethod append-text ((txt text) text &rest tags)
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path txt) (tkescape text) tags)
  txt)

(defmethod insert-object ((txt text) obj)
  (format-wish "~a window create end -window ~a" (widget-path txt) (widget-path obj))
  txt)

(defun append-newline (text)
  (append-text text (coerce '(#\Linefeed) 'string)))

(defgeneric clear-text (txt))

(defmethod clear-text ((txt text))
  (format-wish "~A delete 0.0 end" (widget-path txt))
  txt)

(defmethod see ((txt text) pos)
  (format-wish "~a see ~(~a~)" (widget-path txt) pos)
  txt)

(defgeneric search-all-text (text pattern))

(defmethod search-all-text ((txt text) pattern)
  (format-wish "searchall ~a ~a" (widget-path txt) pattern)
  txt)

(defgeneric search-next-text (text pattern))

(defmethod search-next-text ((txt text) pattern)
  (format-wish "searchnext ~a ~a" (widget-path txt) pattern)
  txt)

(defgeneric tag-configure (txt tag option value &rest others))

(defmethod tag-configure ((txt text) tag option value &rest others)
  (format-wish "~a tag configure ~a~{ -~(~a~) {~/nodgui::down/}~}" (widget-path txt)
               (if (stringp tag)
                   tag
                 (format nil "~(~a~)" tag))
               (list* option value others))
  txt)

(defgeneric tag-bind (txt tag event fun))

(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a tag bind ~a ~a {callback ~A}" (widget-path txt) tag event name)
    )
  txt)

(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end]" (widget-path text))
  (read-data))

(defmethod (setf text) (val (text text))
  (format-wish "~A delete 0.0 end;~A insert end {~A}" (widget-path text) (widget-path text) val)
  val)

(defgeneric save-text (txt filename))

(defmethod save-text ((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (format-wish "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (widget-path txt))
  (read-line (wish-stream *wish*))
  txt)

(defgeneric load-text (txt filename))

(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
;  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"asdf\"" filename (widget-path txt) (widget-path txt))
  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"(:DATA asdf)\"" filename (widget-path txt) (widget-path txt))
  (read-data))
