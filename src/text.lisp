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

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

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
  (let* ((raw-index (read-data))
         (index (split-sequence raw-index ".")))
    (values (parse-integer (first index))
            (parse-integer (second index))
            raw-index)))

(defun make-text (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'text
                 :master  master
                 :width   width
                 :height  height
                 :xscroll xscroll
                 :yscroll yscroll))

(defgeneric clear-text (txt))

(defgeneric append-text (txt text &rest tags))

(defgeneric append-line (txt text &rest tags))

(defgeneric search-all-text (text pattern))

(defgeneric search-next-text (text pattern))

(defgeneric save-text (txt filename &key if-exists if-does-not-exist))

(defgeneric load-text (txt filename))

(defgeneric insert-object (txt object))

(defgeneric insert-window (txt object &optional coordinates))

(defgeneric insert-image (object image &optional coordinates))

(defgeneric insert-text (object text &optional coordinates))

(defgeneric delete-in-range (object from-index &optional to-index))

(defgeneric replace-in-range (object string from-index &optional to-index))

(defgeneric tag-create (object tag-name from-index &rest other-indices))

(defgeneric tag-delete (object &rest tag-names))

(defgeneric tag-lower (object tag-name &optional before-tag))

(defgeneric tag-raise (object tag-name &optional on-top-of-tag))

(defgeneric highlight-text (object from-index &key tag-name to-index))

(defun make-indices-xy (x y)
  (format nil "@~a,~a" x y))

(defun make-indices-linechar (line-number char-index)
  (format nil "~a.~a" line-number char-index))

(defun make-indices-tag (tagname &key (firstp nil) (endp nil))
  (assert (not (and firstp endp)))
  (let ((modifier (cond
                    (firstp
                     ".first")
                    (endp
                     ".last")
                    (t
                     nil))))
    (format nil "~a~@[~a~]" tagname modifier)))

(defun make-indices-end ()
  "end")

(defun make-indices-start ()
  (parse-indices '(:line 1 :char 0)))

(defun make-index-modifier (count units &optional (submodifier nil))
  (assert (member units
                  '("chars" "indices" "lines" "linestart" "lineend" "wordstart" "wordend")
                  :test #'string-equal))
  (assert (or (null submodifier)
              (member submodifier '("display" "any") :test #'string-equal)))
  (format nil "~@d~@[~( ~a~)~] ~(~a~)" count submodifier units))

(defun make-indices (indices &rest modifiers)
  (apply #'join-with-strings (append (list indices) modifiers) (list " ")))

(defmethod append-text ((txt text) text &rest tags)
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path txt) text tags)
  txt)

(defmethod append-line ((object text) text &rest tags)
  (let ((new-text (strcat text (string #\Newline))))
    (apply #'append-text object new-text tags)
    object))

(defun p-sign (indices-specification)
  (let ((token (first indices-specification)))
    (cond
      ((not (symbolp token))
       nil)
      ((string-equal token "+")
       1)
      ((string-equal token "-")
       -1)
      (t
       nil))))

(defun p-coordinate-number (coordinates-spec)
  (assert (numberp (second coordinates-spec)))
  (second coordinates-spec))

(defun p-x (coordinates-spec)
  (assert (eq (first coordinates-spec)
              :x))
  (p-coordinate-number coordinates-spec))

(defun p-y (coordinates-spec)
  (assert (eq (first coordinates-spec)
              :y))
  (p-coordinate-number coordinates-spec))

(defun p-row-number (coordinates-spec)
  (assert (or (eq (first coordinates-spec)
                  :line)
              (eq (first coordinates-spec)
                  :row)))
  (let ((line-number (p-coordinate-number coordinates-spec)))
    (assert (> line-number 0))
    line-number))

(defun p-column-number (coordinates-spec)
  (assert (eq (first coordinates-spec)
              :char))
  (let ((column-number (if (eq (second coordinates-spec)
                               :end)
                           (make-indices-end)
                           (p-coordinate-number coordinates-spec))))
    (assert (or (eq column-number (make-indices-end))
                (>= column-number 0)))
    column-number))

(defun p-tag (coordinates-spec)
  (let ((tag-name (second coordinates-spec))
        (endp     (string-equal (third coordinates-spec) "last"))
        (firstp   (string-equal (third coordinates-spec) "first")))
    (assert (<= (length coordinates-spec) 3))
    (when (third coordinates-spec)
      (assert (or endp firstp)))
    (values tag-name firstp endp)))

(defun p-coordinates (coordinates-spec)
  (let ((look-ahead (first coordinates-spec)))
    (cond
      ((string-equal look-ahead "x")
       (let ((x (p-x coordinates-spec))
             (y (p-y (subseq coordinates-spec 2))))
         (make-indices-xy x y)))
      ((string-equal look-ahead "y")
       (let ((y (p-y coordinates-spec))
             (x (p-x (subseq coordinates-spec 2))))
         (make-indices-xy x y)))
      ((string-equal look-ahead "tag")
       (multiple-value-bind (tag-name firstp endp)
           (p-tag coordinates-spec)
         (make-indices-tag tag-name :firstp firstp :endp endp)))
      ((string-equal look-ahead "mark")
       (second coordinates-spec))
      ((string-equal look-ahead "line")
       (let ((row    (p-row-number coordinates-spec))
             (column (p-column-number (subseq coordinates-spec 2))))
         (make-indices-linechar row column)))
      ((string-equal look-ahead "char")
       (let ((column (p-column-number coordinates-spec))
             (row    (p-row-number (subseq coordinates-spec 2))))
         (make-indices-linechar row column)))
      (t
       (error (format nil
                      "Unknown token: ~s, expected one of (:x, :y, :tag or :mark)"
                      look-ahead))))))

(defun p-coordinates-offset (modifier-specification)
  (p-coordinate-number (list :dummy (first modifier-specification))))

(defun p-modifier (modifier-specification sign)
  (let ((offset      (* sign (p-coordinates-offset modifier-specification)))
        (units       (second modifier-specification))
        (submodifier (third modifier-specification)))
    (make-index-modifier offset units submodifier)))

(defgeneric parse-indices (indices-specification))

(defmethod parse-indices ((indices-specification list))
  (let ((sign (p-sign indices-specification)))
    (if (null sign)
        (p-coordinates indices-specification)
        (let ((index    (p-coordinates (second indices-specification)))
              (modifier (p-modifier    (subseq indices-specification 2) sign)))
          (make-indices index modifier)))))

(defmethod parse-indices ((indices-specification string))
  indices-specification)

(defmethod insert-object ((txt text) obj)
  (format-wish "~a window create end -window ~a" (widget-path txt) (widget-path obj))
  txt)

(defun raw-coordinates (text-widget)
  (nth-value 2 (cursor-index text-widget)))

(defmethod insert-window ((txt text) obj &optional (coordinates (raw-coordinates txt)))
  (format-wish "~a window create ~a -window ~a"
               (widget-path txt)
               (parse-indices coordinates)
               (widget-path obj))
  txt)

(defun insert-image* (text-widget image-resource coordinates)
  (let ((image-object (make-image image-resource)))
    (insert-image text-widget image-object coordinates)))

(defmethod insert-image ((object text) (image-resource pathname)
                         &optional (coordinates (raw-coordinates object)))
  (insert-image* object image-resource coordinates))

(defmethod insert-image ((object text) (image-resource vector)
                         &optional (coordinates (raw-coordinates object)))
  (insert-image* object image-resource coordinates))

(defmethod insert-image ((object text) (image-resource string)
                         &optional (coordinates (raw-coordinates object)))
  (insert-image* object image-resource coordinates))

(defmethod insert-image ((object text) (image-resource photo-image)
                         &optional (coordinates (raw-coordinates object)))
  (format-wish (tclize `(senddata [ ,(widget-path object)      " "
                                    image create
                                    {+ ,(parse-indices coordinates) }
                                    -image ,(name image-resource)
                                    ])))
  (read-data))

(defmethod insert-text ((object text) string &optional (coordinates (raw-coordinates object)))
  (format-wish (tclize `(,(widget-path object) " "
                         insert
                         {+ ,(parse-indices coordinates) }
                         {+ ,string }))))

(defmethod delete-in-range ((object text) from-index &optional (to-index nil))
  (format-wish (tclize `(,(widget-path object) " "
                         delete
                         {+ ,(parse-indices from-index) }
                         ,(empty-string-if-nil to-index
                                            `(  {+ ,(parse-indices to-index) }))))))

(defmethod replace-in-range ((object text) string from-index
                             &optional (to-index (make-indices-end)))
  (format-wish (tclize `(,(widget-path object) " "
                         replace
                         {+ ,(parse-indices from-index) } " "
                         {+ ,(parse-indices to-index) } " "
                         string))))

(defun append-newline (text-widget &rest tags )
  (apply #'append-line text-widget "" tags))

(defmethod clear-text ((txt text))
  (format-wish "~a delete ~a ~a"
               (make-indices-start)
               (make-indices-end)
               (widget-path txt))
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

(defmethod tag-create ((object text) tag-name from-index &rest other-indices)
  (format-wish "~a tag add {~a} {~a} ~{{~a} ~}"
               (widget-path object)
               tag-name
               (parse-indices from-index)
               (mapcar #'parse-indices other-indices))
  tag-name)

(defmethod tag-delete ((object text) &rest tag-names)
  (assert (not (null tag-names)))
  (format-wish "~a tag delete {~a} ~{{~a} ~}" (widget-path object) tag-names)
  object)

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

(defmethod tag-raise ((object text) tag-name &optional (on-top-of-tag nil))
  (format-wish (tclize `(,(widget-path object) " "
                         tag raise
                         {+ ,tag-name }
                         ,(empty-string-if-nil on-top-of-tag
                                               on-top-of-tag)))))

(defmethod tag-lower ((object text) tag-name &optional (before-tag nil))
  (format-wish (tclize `(,(widget-path object) " "
                         tag lower
                         {+ ,tag-name }
                         ,(empty-string-if-nil before-tag
                                               before-tag)))))


(defmethod highlight-text ((object text) from-index
                           &key
                             (tag-name nil)
                             (to-index (raw-coordinates object)))
  (let ((highlight-foreground (cget object :highlightcolor))
        (highlight-background (cget object :highlightbackground))
        (tag-name             (or tag-name (create-name "tag"))))
    (tag-create object tag-name from-index to-index)
    (tag-configure object
                   tag-name
                   :foreground highlight-foreground
                   :background highlight-background)
    tag-name))

(defgeneric make-text-tag-button (object
                                  tag-name
                                  button-1-callback
                                  &key
                                    button-2-callback
                                    button-3-callback
                                    cursor-over
                                    cursor-outside))

(defmethod make-text-tag-button ((object text)
                                 tag-name
                                 button-1-callback
                                 &key
                                   (button-2-callback nil)
                                   (button-3-callback nil)
                                   (cursor-over :hand2)
                                   (cursor-outside (cget object :cursor)))
  (tag-bind object
            tag-name
            #$<ButtonPress-1>$
            button-1-callback)
  (when button-2-callback
    (tag-bind object
              tag-name
              #$<ButtonPress-2>$
              button-2-callback))
  (when button-3-callback
    (tag-bind object
              tag-name
              #$<ButtonPress-3>$
              button-3-callback))
  (tag-bind object
            tag-name
            #$<Enter>$
            (lambda ()
              (configure-mouse-pointer object cursor-over)))
  (tag-bind object
            tag-name
            #$<Leave>$
            (lambda ()
              (configure-mouse-pointer object cursor-outside))))

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
    (setf inner-text (make-text object
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

(defmethod configure ((object scrolled-text) option value &rest others)
  (apply #'configure (inner-text object) option value others))

(defmethod cget ((object scrolled-text) option)
  (cget (inner-text object) option))

(defmethod append-text ((txt scrolled-text) text &rest tags )
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}"
               (widget-path (inner-text txt))
               text
               tags)
  txt)

(defmethod append-line ((object scrolled-text) text &rest tags)
  (with-inner-text (text-widget object)
    (apply #'append-line text-widget text tags)))

(defmethod text ((self scrolled-text))
  (text (inner-text self)))

(defmethod (setf text) (new-text (self scrolled-text))
  (setf (text (inner-text self)) new-text))

(defmethod insert-object ((txt scrolled-text) obj)
  (format-wish "~a window create end -window ~a"
               (widget-path (inner-text txt))
               (widget-path obj))
  txt)

(defmethod see ((txt scrolled-text) pos)
  (format-wish "~a see {~(~a~)}" (widget-path (inner-text txt)) pos)
  txt)

(defmethod insert-window ((object scrolled-text) obj
                          &optional (coordinates (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (insert-window text-widget coordinates)))

(defmethod insert-image ((object scrolled-text) image-resource
                         &optional (coordinates (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (insert-image text-widget image-resource coordinates)))

(defmethod insert-text ((object scrolled-text) string &optional (coordinates (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (insert-text text-widget string coordinates)))

(defmethod delete-in-range ((object scrolled-text) from-index &optional (to-index nil))
  (with-inner-text (text-widget object)
    (delete-in-range text-widget from-index to-index)))

(defmethod replace-in-range ((object scrolled-text) string from-index
                             &optional (to-index (make-indices-end)))
  (with-inner-text (text-widget object)
    (replace-in-range text-widget string from-index to-index)))

(defmethod tag-create ((object scrolled-text) tag-name from-index &rest other-indices)
  (with-inner-text (text-widget object)
    (apply #'tag-create text-widget tag-name from-index other-indices)))

(defmethod tag-delete ((object scrolled-text) &rest tag-names)
  (assert (not (null tag-names)))
  (with-inner-text (text-widget object)
    (apply #'tag-delete text-widget tag-names)))

(defmethod tag-configure ((object scrolled-text) tag-name option value &rest others)
  (with-inner-text (text-widget object)
    (apply #'tag-configure text-widget tag-name option value others)
    object))

(defmethod tag-bind ((object scrolled-text) tag-name event fun &key (exclusive nil))
  (with-inner-text (text-widget object)
    (tag-bind text-widget tag-name event fun :exclusive exclusive)))

(defmethod tag-raise ((object scrolled-text) tag-name &optional on-top-of-tag)
  (with-inner-text (text-widget object)
    (tag-raise text-widget tag-name on-top-of-tag)))

(defmethod tag-lower ((object scrolled-text) tag-name &optional before-tag)
  (with-inner-text (text-widget object)
    (tag-lower text-widget tag-name before-tag)))

(defmethod make-text-tag-button ((object scrolled-text)
                                 tag-name
                                 button-1-callback
                                 &key
                                   (button-2-callback nil)
                                   (button-3-callback nil)
                                   (cursor-over :hand2)
                                   (cursor-outside (cget object :cursor)))
  (with-inner-text (text-widget object)
    (make-text-tag-button text-widget
                          tag-name
                          button-1-callback
                          :button-2-callback button-2-callback
                          :button-3-callback button-3-callback
                          :cursor-over       cursor-over
                          :cursor-outside    cursor-outside)))


(defmethod highlight-text ((object scrolled-text) from-index
                           &key
                             (tag-name nil)
                             (to-index (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (highlight-text text-widget from-index :tag-name tag-name :to-index to-index)))

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
