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

(a:define-constant +reference-text-width-char+ "0"
  :test          #'string=
  :documentation "According to the documentation this is the reference size for char width")

(a:define-constant +text-selection-tag+ "sel"
  :test          #'string=
  :documentation "According to the documentation this is name of the tag qith selected text")

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

(defun parse-line-char-index (raw-index)
  (let ((index (split-sequence raw-index ".")))
    (values (parse-integer (first index))
            (parse-integer (second index)))))

(defmethod cursor-index ((text text))
  (with-read-data (nil)
    (format-wish "senddatastring [~a index insert]" (widget-path text))
    (let ((raw-index (read-data)))
      (multiple-value-bind (lines chars)
          (parse-line-char-index raw-index)
        (values lines chars raw-index)))))

(defun make-text (master &key
                           (width nil)
                           (height nil)
                           (xscroll nil)
                           (yscroll nil)
                           (cursor (find-cursor :xterm)))
  (make-instance 'text
                 :cursor  cursor
                 :master  master
                 :width   width
                 :height  height
                 :xscroll xscroll
                 :yscroll yscroll))

(defgeneric maximum-lines-number (object))

(defgeneric sync-text-metrics (object))

(defgeneric clear-text (txt))

(defgeneric text-in-range (object start-index &optional end-index))

(defgeneric append-text (txt text &rest tags))

(defgeneric append-line (object text &rest tags))

(defgeneric search-all-text (text pattern &key start-index end-index case-insensitive accum))

(defgeneric search-regexp (object pattern start-index
                           &key end-index case-insensitive forward tag-matching-region))

(defgeneric save-text (txt filename &key if-exists if-does-not-exist))

(defgeneric load-text (txt filename))

(defgeneric insert-object (txt object))

(defgeneric insert-window (txt object &optional coordinates))

(defgeneric insert-image (object image &optional coordinates))

(defgeneric insert-text (object text &optional coordinates))

(defgeneric delete-in-range (object start-index &optional end-index))

(defgeneric replace-in-range (object string start-index &optional end-index))

(defgeneric line-info (object &optional index))

(defgeneric tag-create (object tag-name start-index &rest other-indices))

(defgeneric tag-delete (object &rest tag-names))

(defgeneric tag-ranges (object tag-name))

(defgeneric highlight-text (object start-index &key tag-name end-index))

(defgeneric highlight-text-line (object line-index &key tag-name))

(defgeneric make-text-tag-button (object
                                  tag-name
                                  button-1-callback
                                  &key
                                    button-2-callback
                                    button-3-callback
                                    over-callback
                                    leave-callback
                                    cursor-over
                                    cursor-outside
                                    other-bindings))

(defgeneric make-link-button (object
                              from
                              to
                              font
                              foreground
                              background
                              button-1-callback
                              &key
                                tag-prefix
                                button-2-callback
                                button-3-callback
                                over-callback
                                leave-callback
                                cursor-over
                                cursor-outside
                                other-bindings))

(defgeneric move-cursor-to (object index))

(defgeneric move-cursor-to-last-line (object))

(defgeneric move-cursor-to-last-visible-line (object))

(defgeneric move-cursor-to-first-visible-line (object))

(defgeneric move-cursor-next-char (object))

(defgeneric move-cursor-previous-char (object))

(defgeneric move-cursor-next-line (object))

(defgeneric move-cursor-previous-line (object))

(defgeneric width-in-chars (object))

(defgeneric height-in-chars (object))

(defgeneric scroll-until-line-on-top (object line-index &optional column-index))

(defgeneric scroll-to (object coordinates))

(defgeneric index->line-char-coordinates (object index))

(defgeneric selected-text (object))

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
  (let* ((error-coordinates-message
           "Unknown coordinates spec: ~s, expected the keyword :end or a list of coordinates")
        (look-ahead (cond
                      ((listp coordinates-spec)
                       (first coordinates-spec))
                      ((eq coordinates-spec :end)
                       (make-indices-end))
                      (t
                       (error (format nil error-coordinates-message coordinates-spec))))))
    (cond
      ((string-equal look-ahead (make-indices-end))
       look-ahead)
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
  (with-read-data ()
    (format-wish (tclize `(senddata [ ,(widget-path object)      " "
                                    image create
                                    {+ ,(parse-indices coordinates) }
                                    -image ,(name image-resource)
                                    ])))))

(defmethod insert-image ((object text) (image-resource pixmap)
                         &optional (coordinates (raw-coordinates object)))
  (insert-image* object image-resource coordinates))

(defmethod insert-text ((object text) string &optional (coordinates (raw-coordinates object)))
  (format-wish (tclize `(,(widget-path object) " "
                         insert
                         {+ ,(parse-indices coordinates) }
                         {+ ,(make-bypass-escape :data string) }))))


(defmethod delete-in-range ((object text) start-index &optional (end-index nil))
  (format-wish (tclize `(,(widget-path object) " "
                         delete
                         {+ ,(parse-indices start-index) }
                         ,(empty-string-if-nil end-index
                                            `(  {+ ,(parse-indices end-index) }))))))

(defmethod replace-in-range ((object text) string start-index
                             &optional (end-index (make-indices-end)))
  (format-wish (tclize `(,(widget-path object) " "
                         replace
                         {+ ,(parse-indices start-index) } " "
                         {+ ,(parse-indices end-index) } " "
                         \"+ string \"))))

(defmethod line-info ((object text) &optional (coordinates (raw-coordinates object)))
  "Returns a plist of line information, keys are: :x, :y, :w, :h, :baseline."
  (with-read-data (nil)
    (format-wish (tclize `(senddatastrings [ ,(widget-path object) " "
                                           dlineinfo
                                           {+ ,(parse-indices coordinates) }])))
    (let ((raw-data (read-data)))
      (if raw-data
          (let ((number-data (mapcar #'parse-integer raw-data)))
            (list :x        (elt number-data 0)
                  :y        (elt number-data 1)
                  :w        (elt number-data 2)
                  :h        (elt number-data 3)
                  :baseline (elt number-data 4)))
          nil))))

(defun append-newline (text-widget &rest tags )
  (apply #'append-line text-widget "" tags))

(defmethod sync-text-metrics ((object text))
  (format-wish "~a sync" (widget-path object)))

(defmethod maximum-lines-number ((object text))
  (with-read-data (nil)
    (format-wish (tclize `(senddatastring [ ,(widget-path object) " "
                                          index
                                          ,(make-indices-end)
                                          ])))
    (let ((raw-index (read-data)))
      (1- (nth-value 0 (parse-line-char-index raw-index))))))

(defmethod clear-text ((txt text))
  (format-wish "~a delete ~a ~a"
               (widget-path txt)
               (make-indices-start)
               (make-indices-end))
  txt)

(defmethod text-in-range ((object text) start-index &optional (end-index (make-indices-end)))
  (with-read-data ()
    (format-wish (tclize `(senddatastring [ ,(widget-path object) " "
                                          get
                                          {+ ,(parse-indices start-index) }
                                          {+ ,(parse-indices end-index) }
                                          ])))))

(defmethod see ((object text) pos)
  (format-wish "~a see {~a}" (widget-path object) (parse-indices pos))
  object)

(defstruct match
  (start)
  (end)
  (string)
  (tag-name))

(defmethod search-all-text ((object text) pattern
                            &key
                              (start-index      (make-indices-start))
                              (end-index        (make-indices-end))
                              (case-insensitive t)
                              (accum            '()))
  (multiple-value-bind (re-start-index re-end-index tag-name x y z)
      (ignore-errors
       (search-regexp object
                      pattern
                      start-index
                      :tag-matching-region t
                      :case-insensitive    case-insensitive
                      :end-index           end-index))
    (declare (ignore x y z))
    (if tag-name
        (let ((results (make-match :start    re-start-index
                                   :end      re-end-index
                                   :string   (text-in-range object re-start-index re-end-index)
                                   :tag-name tag-name)))
          (search-all-text object
                           pattern
                           :start-index       re-end-index
                           :end-index         end-index
                           :case-insensitive  case-insensitive
                           :accum             (push results accum)))
        accum)))

(a:define-constant +text-tag-prefix-search-results+ "tgre" :test #'string=)

(defmethod search-regexp ((object text) pattern start-index
                          &key
                            (end-index (make-indices-end))
                            (case-insensitive t)
                            (forward          t)
                            (tag-matching-region nil))
  (with-read-data (nil)
    (let ((count-variable-name (create-name "rect")))
      (with-send-wish-atomic (stream)
        (format-for-wish stream
                         "global ~a; set ~a {};"
                         count-variable-name
                         count-variable-name)
        (format-for-wish stream
                         (tclize `(,(format nil "global ~a; set ~a {};"
                                          count-variable-name count-variable-name)
                                 senddatastring [ ,(widget-path object) " "
                                        search
                                        -regexp
                                        ,(if forward
                                             '-forwards
                                             '-backwards)
                                        -nolinestop
                                        -count ,count-variable-name  " "
                                        ,(empty-string-if-nil case-insensitive
                                                              '-nocase)
                                        --
                                        \"+ ,pattern \"
                                        {+ ,(parse-indices start-index) }
                                        {+ ,(parse-indices end-index) }
                                        ])
                                 :sanitize t)))
      (let ((indices (read-data)))
        (when (not (string-empty-p indices))
          (format-wish "global ~a; senddata $~a"
                       count-variable-name count-variable-name)
          (let ((size (read-data)))
            (multiple-value-bind (lines chars)
                (parse-line-char-index indices)
              (let ((re-start-index `(:line ,lines :char ,chars))
                    (re-end-index  `(+ (:line ,lines :char ,chars)
                                       ,size :chars)))
                (if tag-matching-region
                    (let ((tag-name  (create-name +text-tag-prefix-search-results+)))
                      (tag-create object tag-name re-start-index re-end-index)
                      (values re-start-index re-end-index tag-name lines chars size))
                    (values re-start-index re-end-index nil lines chars size))))))))))

(defmethod tag-create ((object text) tag-name start-index &rest other-indices)
  (format-wish "~a tag add {~a} {~a} ~{{~a} ~}"
               (widget-path object)
               tag-name
               (parse-indices start-index)
               (mapcar #'parse-indices other-indices))
  tag-name)

(defmethod tag-delete ((object text) &rest tag-names)
  (assert (not (null tag-names)))
  (format-wish "~a tag delete ~{{~a} ~}" (widget-path object) tag-names)
  object)

(defmethod tag-configure ((object text) tag option value &rest others)
  (format-wish "~a tag configure {~a}~{ {-~(~a~)} {~a}~}"
               (widget-path object)
               (if (stringp tag)
                   tag
                   (format nil "~(~a~)" tag))
               (mapcar #'down (list* option value others)))
  object)

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
                         \"+ ,tag-name \"
                         ,(empty-string-if-nil on-top-of-tag
                                               on-top-of-tag)))))

(defmethod tag-ranges ((object text) tag-name)
  (with-read-data (nil)
    (format-wish (tclize `(senddatastring [ ,(widget-path object) " "
                                          tag ranges
                                          \"+ ,tag-name \"
                                          ])))
    (let ((indices (split-words (read-data))))
      (loop for line-char in indices
            collect
            (multiple-value-bind (line char)
                (parse-line-char-index line-char)
              `(:line ,line :char ,char))))))

(defmethod tag-lower ((object text) tag-name &optional (before-tag nil))
  (format-wish (tclize `(,(widget-path object) " "
                         tag lower
                         \"+ ,tag-name \"
                         ,(empty-string-if-nil before-tag
                                               before-tag)))))


(defmethod highlight-text ((object text) start-index
                           &key
                             (tag-name (create-tag-name))
                             (end-index (raw-coordinates object)))
  (let ((highlight-foreground (cget object :highlightcolor))
        (highlight-background (cget object :highlightbackground))
        (tag-name             (or tag-name (create-tag-name))))
    (tag-create object tag-name start-index end-index)
    (tag-configure object
                   tag-name
                   :foreground highlight-foreground
                   :background highlight-background)
    tag-name))

(defmethod highlight-text-line ((object text) line-index &key (tag-name (create-tag-name)))
  (highlight-text object
                  (raw-coordinates object)
                  :end-index `(:line ,line-index :char :end)
                  :tag-name  tag-name))

(defmethod move-cursor-to ((object text) index)
  (let ((parsed-index (parse-indices index)))
    (format-wish (tclize `(,(widget-path object) " "
                           mark set insert
                           {+ ,parsed-index })))
    (values object index)))

(defmethod move-cursor-to-last-line ((object text))
  (move-cursor-to object (make-indices-end))
  (let ((last-line (cursor-index object)))
    (move-cursor-to object `(:line ,last-line :char 0))))

(defmethod move-cursor-to-last-visible-line ((object text))
  (let* ((height (window-height object))
         (column (nth-value 1 (cursor-index object)))
         (index  `(:line ,height :char ,column)))
    (move-cursor-to object index)))

(defmethod move-cursor-to-first-visible-line ((object text))
  (let* ((column (nth-value 1
                            (cursor-index object)))
         (index  `(+ (:x  0 :y 0)
                     ,column :chars)))
    (move-cursor-to object index)))

(defmethod move-cursor-next-char ((object text))
  (multiple-value-bind (lines chars)
      (cursor-index object)
    (move-cursor-to object `(+ (:line ,lines :char ,chars) 1 :chars))))

(defmethod move-cursor-previous-char ((object text))
  (multiple-value-bind (lines chars)
      (cursor-index object)
    (when (> chars 0)
      (move-cursor-to object `(- (:line ,lines :char ,chars) 1 :chars)))))

(defmethod move-cursor-next-line ((object text))
  (multiple-value-bind (lines chars)
      (cursor-index object)
    (move-cursor-to object `(+ (:line ,lines :char ,chars) 1 :lines))))

(defmethod move-cursor-previous-line ((object text))
    (multiple-value-bind (lines chars)
      (cursor-index object)
      (move-cursor-to object `(- (:line ,lines :char ,chars) 1 :lines))))

(defmethod width-in-chars ((object text))
  (let* ((width-in-pixel  (window-width object))
         (font            (cget object :font))
         (zero-char-width (font-measure font "0")))
    (round (/ width-in-pixel zero-char-width))))

(defmethod height-in-chars ((object text))
  (let* ((height-in-pixel (window-height object))
         (font            (cget object :font))
         (font-metrics    (font-metrics font))
         (font-linespace  (getf font-metrics :linespace)))
    (round (/ height-in-pixel font-linespace))))

(defmethod make-text-tag-button ((object text)
                                 tag-name
                                 button-1-callback
                                 &key
                                   (button-2-callback nil)
                                   (button-3-callback nil)
                                   (cursor-over :hand2)
                                   (over-callback nil)
                                   (leave-callback nil)
                                   (cursor-outside (cget object :cursor))
                                   other-bindings)
  (let ((actual-cursor-outside (or (find-cursor cursor-outside)
                                   +standard-cursor+))
        (actual-cursor-over    (or (find-cursor cursor-over)
                                   +standard-cursor+)))
    (loop for (event . callback) in other-bindings do
      (tag-bind object tag-name event callback))
    (tag-bind object
              tag-name
              #$<ButtonPress-1>$
              (lambda ()
                (configure-mouse-pointer object actual-cursor-outside)
                (funcall button-1-callback)))
    (when button-2-callback
      (tag-bind object
                tag-name
                #$<ButtonPress-2>$
                (lambda ()
                  (configure-mouse-pointer object actual-cursor-outside)
                  (funcall button-2-callback))))
    (when button-3-callback
      (tag-bind object
                tag-name
                #$<ButtonPress-3>$
                (lambda ()
                  (configure-mouse-pointer object actual-cursor-outside)
                  (funcall button-3-callback))
                :exclusive t))
    (tag-bind object
              tag-name
              #$<Motion>$
              (lambda ()
                (configure-mouse-pointer object actual-cursor-over)
                (when over-callback
                  (funcall over-callback))))
    (tag-bind object
              tag-name
              #$<Leave>$
              (lambda ()
                (configure-mouse-pointer object actual-cursor-outside)
                (when leave-callback
                  (funcall leave-callback))))))

(defmethod make-link-button ((object text)
                             from
                             to
                             font
                             foreground
                             background
                             button-1-callback
                             &key
                               (tag-prefix "link")
                               button-2-callback
                               button-3-callback
                               leave-callback
                               over-callback
                               (cursor-over :hand2)
                               (cursor-outside (cget object :cursor))
                               other-bindings)
  (let ((tag-link (tag-create object (create-name tag-prefix) from to)))
    (tag-configure object tag-link :font font :foreground foreground :background background)
    (make-text-tag-button object
                          tag-link
                          button-1-callback
                          :button-2-callback button-2-callback
                          :button-3-callback button-3-callback
                          :over-callback     over-callback
                          :leave-callback    leave-callback
                          :cursor-over       cursor-over
                          :cursor-outside    cursor-outside
                          :other-bindings    other-bindings)
    tag-link))

(defmethod text ((text text))
  (text-in-range text (make-indices-start) (make-indices-end)))

(defmethod (setf text) (val (text text))
  (format-wish "~a delete 0.0 end; ~a insert end \"~a\""
               (widget-path text)
               (widget-path text)
               val)
  val)

(defmethod save-text ((object text) filename
                      &key
                        (if-exists         :supersede)
                        (if-does-not-exist :create))
  "save the content of the text widget into the file <filename>"
  (let ((data (text object)))
    (with-open-file (stream filename
                            :direction         :output
                            :if-exists         if-exists
                            :if-does-not-exist if-does-not-exist)
      (write-sequence data stream)))
  object)

(a:define-constant +newline-as-string+ (format nil "~%") :test #'string=)

(defmethod load-text ((object text) file-path)
  "load the content of the file `file-path'"
  (if (file-exists-p file-path)
      (with-open-file (stream file-path :direction :input :if-does-not-exist :error)
        (loop for line = (read-line stream nil nil nil)
              while line
              do
                 (append-text object (strcat line +newline-as-string+))))
      (error "File ~a does not exists" file-path))
  object)

(defmethod scroll-until-line-on-top ((object text) line-index &optional (column-index 0))
  (let ((indices (parse-indices `(:line ,line-index :char ,column-index))))
    (format-wish (tclize `(,(widget-path object) " " yview {+ ,indices })))
    object))

(defmethod scroll-to ((object text) coordinates)
  (let ((indices (parse-indices coordinates)))
    (format-wish (tclize `(,(widget-path object) " " yview {+ ,indices })))
    object))

(defmethod index->line-char-coordinates ((object text) coordinates)
  (with-read-data (nil)
    (format-wish (tclize `(senddatastring [ ,(widget-path object)      " "
                                          index
                                          {+ ,(parse-indices coordinates) }
                                          ])))
    (let ((raw-index (read-data)))
      (multiple-value-bind (lines chars)
          (parse-line-char-index raw-index)
        (values lines chars raw-index)))))

(defmethod selected-text ((object text))
  (let ((ranges (tag-ranges object +text-selection-tag+)))
    (text-in-range object
                   (first ranges)
                   (second ranges))))

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

(defun set-scrolled-text-read-only-with-cursor (text-widget)
  (bind (inner-text text-widget)
        #$<KeyPress>$
        (lambda (event)
          (let ((keycode (event-char event)))
            (cond
              ((string= keycode nodgui.event-symbols:+up+)
               (move-cursor-previous-line text-widget)
               (see text-widget (nth-value 2 (cursor-index text-widget))))
              ((string= keycode nodgui.event-symbols:+down+)
               (move-cursor-next-line text-widget)
               (see text-widget (nth-value 2 (cursor-index text-widget))))
              ((string= keycode nodgui.event-symbols:+left+)
               (move-cursor-previous-char text-widget)
               (see text-widget (nth-value 2 (cursor-index text-widget))))
              ((string= keycode nodgui.event-symbols:+right+)
               (move-cursor-next-char text-widget)
               (see text-widget (nth-value 2 (cursor-index text-widget)))))))
        :exclusive t))

(defun scroll-text-read-only-up (text-widget)
  (format-wish (tclize `(,(widget-path text-widget) " " yview "scroll -1 units")))
  text-widget)

(defun scroll-text-read-only-down (text-widget)
  (format-wish (tclize `(,(widget-path text-widget) " " yview "scroll 1 units")))
  text-widget)

(defun scroll-text-read-only-next-page (text-widget)
  (format-wish (tclize `(,(widget-path text-widget) " " yview "scroll 1 pages")))
  text-widget)

(defun scroll-text-read-only-previous-page (text-widget)
  (format-wish (tclize `(,(widget-path text-widget) " " yview "scroll -1 pages")))
  text-widget)

(defun scroll-text-read-only-go-start-text (text-widget)
  (move-cursor-to text-widget `(:line 1 :char 0))
  (scroll-until-line-on-top text-widget 1))

(defun scroll-text-read-only-go-end-text (text-widget)
  (let* ((last-line-number (maximum-lines-number text-widget))
         (index            `(:line ,last-line-number :char 0)))
    (move-cursor-to text-widget index)
    (see text-widget index)))

(defun set-scrolled-text-read-only (text-widget)
  (bind (inner-text text-widget)
        #$<KeyPress-Up>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-up (inner-text text-widget)))
        :exclusive t)
  (bind (inner-text text-widget)
        #$<KeyPress-Down>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-down (inner-text text-widget)))
        :exclusive t)
  (bind (inner-text text-widget)
        #$<KeyPress-Next>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-next-page (inner-text text-widget)))
        :exclusive t)
  (bind (inner-text text-widget)
        #$<KeyPress-Prior>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-previous-page (inner-text text-widget)))
        :exclusive t)
  (bind (inner-text text-widget)
        #$<KeyPress-Home>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-go-start-text text-widget))
        :exclusive t)
  (bind (inner-text text-widget)
        #$<KeyPress-End>$
        (lambda (e)
          (declare (ignore e))
          (scroll-text-read-only-go-end-text text-widget))
        :exclusive t)
  (bind (inner-text text-widget) #$<<PasteSelection>>$
        (lambda (e)
          (declare (ignore e))  t)
        :exclusive t
        :append nil)
  (bind (inner-text text-widget) #$<KeyPress>$
        (lambda (e)
          (declare (ignore e)) t)
        :exclusive t
        :append t))

(defmethod initialize-instance :after ((object scrolled-text)
                                       &key
                                         autoseparators
                                         background
                                         borderwidth
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
                                         wrap
                                         (cursor                     (find-cursor :xterm))
                                         (use-horizontal-scrolling-p t)
                                         (read-only nil)
                                         &allow-other-keys)
  (with-accessors ((inner-text inner-text)
                   (hscroll    hscroll)
                   (vscroll    vscroll)) object
    (setf vscroll (make-scrollbar object))
    (setf inner-text (make-instance 'text
                                    :autoseparators      autoseparators
                                    :background          background
                                    :borderwidth         borderwidth
                                    :exportselection     exportselection
                                    :font                font
                                    :foreground          foreground
                                    :height              height
                                    :highlightbackground highlightbackground
                                    :highlightcolor      highlightcolor
                                    :highlightthickness  highlightthickness
                                    :insertbackground    insertbackground
                                    :insertborderwidth   insertborderwidth
                                    :insertofftime       insertofftime
                                    :insertontime        insertontime
                                    :insertwidth         insertwidth
                                    :maxundo             maxundo
                                    :padx                padx
                                    :pady                pady
                                    :selectbackground    selectbackground
                                    :selectborderwidth   selectborderwidth
                                    :selectforeground    selectforeground
                                    :setgrid             setgrid
                                    :spacing1            spacing1
                                    :spacing2            spacing2
                                    :spacing3            spacing3
                                    :state               state
                                    :tabs                tabs
                                    :takefocus           takefocus
                                    :undo                undo
                                    :wrap                wrap
                                    :master              object
                                    :cursor              cursor
                                    :xscroll             hscroll
                                    :yscroll             vscroll))
    (grid inner-text  0 0 :sticky :news)
    (grid vscroll     0 1 :sticky :ns)
    (grid-columnconfigure object 0 :weight 1)
    (grid-columnconfigure object 1 :weight 0)
    (grid-rowconfigure    object 0 :weight 1)
    (when use-horizontal-scrolling-p
      (setf hscroll (make-scrollbar object :orientation "horizontal"))
      (grid hscroll 1 0 :sticky :we)
      (grid-rowconfigure object 1 :weight 0)
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
               (concatenate 'string (widget-path vscroll) " set"))
    (move-cursor-to inner-text (make-indices-start))
    (cond
      ((eq read-only :with-cursor)
       (set-scrolled-text-read-only-with-cursor object))
      (read-only
       (set-scrolled-text-read-only object)))))

(defmacro with-inner-text ((text-slot scrolled-text) &body body)
  "Syntatic sugar to  access the text slot of a scrolled-text

  `(with-accessors ((,text-slot inner-text)) ,scrolled-text
     ,@body))"
  `(with-accessors ((,text-slot inner-text)) ,scrolled-text
     ,@body))

(defmethod cursor-index ((object scrolled-text))
  (with-inner-text (text-widget object)
    (cursor-index text-widget)))

(defmethod configure ((object scrolled-text) option value &rest others)
  (apply #'configure (inner-text object) option value others))

(defmethod cget ((object scrolled-text) option &key (query-container nil))
  (if query-container
      (call-next-method)
      (cget (inner-text object) option)))

(defmethod clear-text ((object scrolled-text))
  (with-inner-text (text-widget object)
    (clear-text text-widget)
    object))

(defmethod text-in-range ((object scrolled-text) start-index
                          &optional (end-index (make-indices-end)))
  (with-inner-text (text-widget object)
    (text-in-range text-widget start-index end-index)))

(defmethod save-text ((object scrolled-text) filename
                      &key
                        (if-exists         :supersede)
                        (if-does-not-exist :create))
  (with-inner-text (text-widget object)
    (save-text text-widget
               filename
               :if-exists if-exists
               :if-does-not-exist if-does-not-exist)))

(defmethod load-text ((object scrolled-text) file-path)
  (with-inner-text (text-widget object)
    (load-text text-widget file-path)))

(defmethod sync-text-metrics ((object scrolled-text))
  (with-inner-text (text-widget object)
    (sync-text-metrics text-widget))
  object)

(defmethod maximum-lines-number ((object scrolled-text))
  (with-inner-text (text-widget object)
    (maximum-lines-number text-widget)))

(defmethod append-text ((object scrolled-text) text &rest tags )
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}"
               (widget-path (inner-text object))
               text
               tags)
  object)

(defmethod append-line ((object scrolled-text) text &rest tags)
  (with-inner-text (text-widget object)
    (apply #'append-line text-widget text tags)))

(defmethod text ((self scrolled-text))
  (text (inner-text self)))

(defmethod (setf text) (new-text (self scrolled-text))
  (setf (text (inner-text self)) new-text))

(defmethod insert-object ((object scrolled-text) obj)
  (format-wish "~a window create end -window ~a"
               (widget-path (inner-text object))
               (widget-path obj))
  object)

(defmethod see ((object scrolled-text) pos)
  (with-inner-text (text-widget object)
    (see text-widget pos)
    object))

(defmethod scroll-until-line-on-top ((object scrolled-text) line-index &optional (column-index 0))
  (with-inner-text (text-widget object)
    (scroll-until-line-on-top text-widget line-index column-index)
    object))

(defmethod scroll-to ((object scrolled-text) coordinates)
  (with-inner-text (text-widget object)
    (scroll-to text-widget coordinates)
    object))

(defmethod index->line-char-coordinates ((object scrolled-text) coordinates)
  (with-inner-text (text-widget object)
    (index->line-char-coordinates text-widget coordinates)))

(defmethod selected-text ((object scrolled-text))
  (with-inner-text (text-widget object)
    (selected-text text-widget)))

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

(defmethod delete-in-range ((object scrolled-text) start-index &optional (end-index nil))
  (with-inner-text (text-widget object)
    (delete-in-range text-widget start-index end-index)))

(defmethod replace-in-range ((object scrolled-text) string start-index
                             &optional (end-index (make-indices-end)))
  (with-inner-text (text-widget object)
    (replace-in-range text-widget string start-index end-index)))

(defmethod line-info ((object scrolled-text) &optional (coordinates (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (line-info text-widget coordinates)))

(defmethod tag-create ((object scrolled-text) tag-name start-index &rest other-indices)
  (with-inner-text (text-widget object)
    (apply #'tag-create text-widget tag-name start-index other-indices)))

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

(defmethod tag-ranges ((object scrolled-text) tag-name)
  (with-inner-text (text-widget object)
    (tag-ranges text-widget tag-name)))

(defmethod tag-lower ((object scrolled-text) tag-name &optional before-tag)
  (with-inner-text (text-widget object)
    (tag-lower text-widget tag-name before-tag)))

(defmethod make-text-tag-button ((object scrolled-text)
                                 tag-name
                                 button-1-callback
                                 &key
                                   (button-2-callback nil)
                                   (button-3-callback nil)
                                   (over-callback nil)
                                   (leave-callback nil)
                                   (cursor-over :hand2)
                                   (cursor-outside (cget object :cursor))
                                   other-bindings)
  (with-inner-text (text-widget object)
    (make-text-tag-button text-widget
                          tag-name
                          button-1-callback
                          :button-2-callback button-2-callback
                          :button-3-callback button-3-callback
                          :over-callback     over-callback
                          :leave-callback    leave-callback
                          :cursor-over       cursor-over
                          :cursor-outside    cursor-outside
                          :other-bindings    other-bindings)))

(defmethod make-link-button ((object scrolled-text)
                             from
                             to
                             font
                             foreground
                             background
                             button-1-callback
                             &key
                               (tag-prefix "link")
                               button-2-callback
                               button-3-callback
                               over-callback
                               leave-callback
                               (cursor-over :hand2)
                               (cursor-outside (cget object :cursor))
                               other-bindings)
  (with-inner-text (text-widget object)
    (make-link-button text-widget
                      from
                      to
                      font
                      foreground
                      background
                      button-1-callback
                      :tag-prefix        tag-prefix
                      :button-2-callback button-2-callback
                      :button-3-callback button-3-callback
                      :cursor-over       cursor-over
                      :cursor-outside    cursor-outside
                      :over-callback     over-callback
                      :leave-callback    leave-callback
                      :other-bindings    other-bindings)))

(defmethod highlight-text ((object scrolled-text) start-index
                           &key
                             (tag-name nil)
                             (end-index (raw-coordinates object)))
  (with-inner-text (text-widget object)
    (highlight-text text-widget start-index :tag-name tag-name :end-index end-index)))

(defmethod highlight-text-line ((object scrolled-text) line-index &key (tag-name (create-tag-name)))
  (with-inner-text (text-widget object)
    (highlight-text-line text-widget line-index :tag-name tag-name)))

(defmethod move-cursor-to ((object scrolled-text) index)
  (with-inner-text (text-widget object)
    (move-cursor-to text-widget index)))

(defmethod move-cursor-to-last-line ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-to-last-line text-widget)))

(defmethod move-cursor-to-last-visible-line ((object scrolled-text))
  "Note:  there is  a bad  heuristic involved  in this  function, column
position  could be  wrong  if  the latest  visible  line  uses a  font
different from the default (expecially if  such a font is smaller than
the size of the default one"
  (with-inner-text (text-widget object)
    (let* ((height               (window-height text-widget))
           (font-metrics         (font-metrics (cget object :font)))
           (font-approx-baseline (getf font-metrics :descent))
           (column (nth-value 1 (cursor-index text-widget)))
           (index  `(+ (:x 0
                        :y ,(- height font-approx-baseline))
                       ,column :chars)))
      (move-cursor-to text-widget index))))

(defmethod move-cursor-to-first-visible-line ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-to-first-visible-line text-widget)))

(defmethod move-cursor-next-char ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-next-char text-widget)))

(defmethod move-cursor-previous-char ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-previous-char text-widget)))

(defmethod move-cursor-next-line ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-next-line text-widget)))

(defmethod move-cursor-previous-line ((object scrolled-text))
  (with-inner-text (text-widget object)
    (move-cursor-previous-line text-widget)))

(defmethod width-in-chars ((object scrolled-text))
  (with-inner-text (text-widget object)
    (width-in-chars text-widget)))

(defmethod height-in-chars ((object scrolled-text))
  (with-inner-text (text-widget object)
    (height-in-chars text-widget)))

(defmethod search-regexp ((object scrolled-text) pattern start-index
                         &key
                           (end-index (make-indices-end))
                           (case-insensitive t)
                           (forward          t)
                           (tag-matching-region nil))
  (with-inner-text (text-widget object)
    (search-regexp text-widget pattern start-index
                   :end-index           end-index
                   :case-insensitive    case-insensitive
                   :forward             forward
                   :tag-matching-region tag-matching-region)))

(defmethod search-all-text ((object scrolled-text) pattern
                            &key
                              (start-index      (make-indices-start))
                              (end-index        (make-indices-end))
                              (case-insensitive t)
                              (accum            '()))
  (with-inner-text (text-widget object)
    (search-all-text text-widget
                     pattern
                     :start-index      start-index
                     :end-index        end-index
                     :case-insensitive case-insensitive
                     :accum            accum)))

(defun visible-portion (scrollbar)
  (with-read-data (nil)
    (format-wish "senddata \"([~a get])\"" (widget-path scrollbar))
    (let ((raw (read-data)))
      (list :start (first raw)
            :end   (second raw)))))

(defgeneric y-visible-portion (object))

(defmethod y-visible-portion ((object scrolled-text))
  "Return a plist list of normalized (from 0 to 1) portion of currently visible document.
 e.g: (:start 0.0 :end 0.5)"
  (visible-portion (vscroll object)))

(defgeneric x-visible-portion (object))

(defmethod x-visible-portion ((object scrolled-text))
  "Return a plist list of normalized (from 0 to 1) portion of currently visible document.
 e.g: (:start 0.0 :end 0.5)"
  (visible-portion (hscroll object)))

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
