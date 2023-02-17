;; This software is Copyright
;; (c) 2004 Peter Herth <herth@peter-herth.de>
;; (c) 2018 cage

;; Peter Herth grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui.mw)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

;;;; mixin class for widget construction
;;;; for widgets inheriting from redraw-on-resize the generic function
;;;; redraw is called, whenever the widget is resized (e.g. by window resize)

(defgeneric redraw (widget))

(defclass redraw-on-resize () ())

(defmethod initialize-instance :after ((r redraw-on-resize) &key)
  (bind r #$<Configure>$ (lambda (evt)
                           (declare (ignore evt))
                           (redraw r))))

;;;; history entry widget
;;;;
;;;; Entry widget with history of all text entered.

(defclass history-entry (entry)
  ((history
    :initform  '()
    :initarg   :history
    :accessor history)
   (history-pos
    :accessor  history-pos
    :initform -1)
   (keepinput
    :initform nil
    :initarg  :keepinput
    :accessor keepinput)
   (compare-history-candidate-predicate
    :initform #'string=
    :initarg  :compare-history-candidate-predicate
    :accessor compare-history-candidate-predicate
    :documentation "An new candidate is added to the history only if
                    (funcall compare-history-candidate-predicate candidate old-entry) is
                    nil for all elements in history"))
  (:documentation  "An entry  widget keeping  the history  of previous
    input (which can be browsed through  with cursor up and down), the
    input  can  be also  autocompleted  pressing  the TAB  key.   When
    autocompletion items  are shown  the user  can press  the function
    key,  from F1  to  F10, to  complete the  text  with the  proposed
    completion corresponding to the number of the function key."))

(defgeneric add-history (entry txt))

(defgeneric clear-history (entry))

(defmethod add-history ((entry history-entry) (txt string))
  (with-accessors ((compare-history-candidate-predicate compare-history-candidate-predicate))
      entry
    (when (> (length txt) 0)
      (pushnew txt (history entry) :test compare-history-candidate-predicate))
    (setf (history-pos entry) -1)))

(defmethod clear-history ((entry history-entry))
  (setf (history entry) nil)
  (setf (history-pos entry) -1))

(defun left-parens-ornament ()
  (try-unicode "MEDIUM_LEFT_PARENTHESIS_ORNAMENT" #\( ))

(defun right-parens-ornament ()
  (try-unicode "MEDIUM_RIGHT_PARENTHESIS_ORNAMENT" #\)))

(defmacro bind-quick-choice (entry history saved-text-entry event-type position)
  `(bind ,entry ,event-type
         (lambda (event)
           (declare (ignore event))
           (let* ((sorted-history (sort (copy-list ,history)
                                        (lambda (a b) (> (length a) (length b)))))
                  (candidates     (remove-if-not (lambda (a)
                                                   (scan (strcat "^"
                                                                 ,saved-text-entry)
                                                         a))
                                                 sorted-history)))
             (when (< ,position  (length candidates))
               (setf text-entry (elt candidates ,position))
               (set-selection    entry 0 0)
               (set-cursor-index entry :end))))))

(defmacro bind-quick-choices (entry history saved-text-entry &rest event-types-and-positions)
  `(progn
     ,@(loop for (event-type . position) in event-types-and-positions collect
            `(bind-quick-choice ,entry ,history ,saved-text-entry ,event-type ,position))))

(defmethod initialize-instance :after ((entry history-entry) &key command &allow-other-keys)
  (with-accessors ((history    history)
                   (text-entry text)) entry
    (let ((saved-text-entry ""))
      (bind entry #$<KeyPress-Return>$
            (lambda (event)
              (declare (ignore event))
              (let ((txt (text entry)))
                (add-history entry txt)
                (if (keepinput entry)
                    (entry-select entry 0 "end")
                    (setf (text entry) ""))
                (nodgui::callback (nodgui::name entry) (list txt)))))
      (bind entry #$<KeyPress-Up>$
            (lambda (event)
              (declare (ignore event))
              (when (< (history-pos entry) (1- (length (history entry))))
                (incf (history-pos entry))
                (let ((val (nth (history-pos entry) (history entry))))
                  (when val
                    (setf (text entry) val))))))
      (bind entry #$<KeyPress-Down>$
            (lambda (event)
              (declare (ignore event))
              (if (>= (history-pos entry) 0)
                  (progn
                    (decf (history-pos entry))
                    (if (>= (history-pos entry) 0)
                        (setf (text entry) (nth (history-pos entry) (history entry)))
                        (setf (text entry) "")))
                  (progn
                    (setf (text entry) "")))))
      (bind entry #$<KeyPress-Tab>$
            (lambda (event)
              (declare (ignore event))
              (flet ((format-candidates (candidates)
                       (let ((res ""))
                         (loop
                            for i from 1
                            for candidate in candidates do
                              (setf res
                                    (strcat res
                                            (format nil
                                                    "~a:~a ~a ~a"
                                                    i
                                                    (left-parens-ornament)
                                                    candidate
                                                    (right-parens-ornament)))))
                         res)))
                (when history
                  (a:when-let* ((sorted-history (sort (copy-list history)
                                                      (lambda (a b) (> (length a) (length b)))))
                                (candidates     (remove-if-not (lambda (a)
                                                                 (scan (strcat "^" text-entry)
                                                                       a))
                                                               sorted-history))
                                (prefix         (apply #'common-prefix candidates))
                                (new-text       (if (> (length candidates) 1)
                                                    (strcat prefix " "
                                                            (format-candidates candidates))
                                                    prefix)))
                    (setf saved-text-entry text-entry)
                    (setf text-entry new-text)
                    (set-cursor-index entry (length prefix))
                    (set-selection    entry (length prefix) :end)))))
            :exclusive t)
      (bind-quick-choices entry
                          history
                          saved-text-entry
                          (#$<F1>$  . 0)
                          (#$<F2>$  . 1)
                          (#$<F3>$  . 2)
                          (#$<F4>$  . 3)
                          (#$<F5>$  . 4)
                          (#$<F6>$  . 5)
                          (#$<F7>$  . 6)
                          (#$<F8>$  . 7)
                          (#$<F9>$  . 8)
                          (#$<F10>$ . 9))
      (when command
        (setf (command entry) command)))))

(defmethod (setf command) (val (entry history-entry))
  (nodgui::add-callback (nodgui::name entry) val))

;;; tree list widget

(defclass treelist (frame)
  ((depth
    :reader  depth
    :initarg :depth
    :initform 3
    :documentation "number of listboxes to display")
   (listbox
    :accessor      listbox
    :initform      nil
    :documentation "array with the displayed listboxes")
   (data
    :accessor data
    :initarg  :data
    :initform nil
    :documentation "root  node to  be displayed  (its children
            fill the first box)")
   (entries
    :accessor entries
    :documentation  "array  of  the  lists  displayed  in  the
            listbox")
   (offset
    :accessor offset
    :initform 0
    :documentation  "index   difference  between   data  depth
            position and listbox position")
   (selection
    :accessor selection
    :initform nil
    :documentation "list of selected values"))
  (:documentation "A widget to display  a tree-like structure by a set
  of listboxes."))

(defclass tree-entry ()
  ((nodes
    :accessor nodes
    :initform nil
    :initarg  :nodes)
   (index
    :accessor index
    :initform nil
    :initarg  :index)
   (parent-node
    :accessor parent-node
    :initform nil
    :initarg  :parent-node)
   (selected-node
    :accessor selected-node
    :initform nil
    :initarg  :selected-node)))

(defmethod initialize-instance :after ((tree treelist)
                                       &key listwidth listheight
                                       (background :white) horizontal-scrollbar)
  (setf (listbox tree) (make-array (depth tree)))
  (setf (entries tree) (make-array (depth tree) :adjustable t :fill-pointer 0))
  (dotimes (i (depth tree))
    (let ((nr i)
          (sb (make-instance 'scrolled-listbox
                             :master tree
                             :width  listwidth
                             :height listheight )))
      (unless horizontal-scrollbar
        (grid-forget (nodgui::hscroll sb)))
      (setf (aref (listbox tree) nr) (listbox sb))
      (configure (listbox sb)
                 :background        background
                 :selectforeground :white
                 :selectbackground :blue)
      (pack sb :side :left :expand t :fill :both)
      (bind (aref (listbox tree) nr) #$<<ListboxSelect>>$
            (lambda (event)
              (declare (ignore event))
              (treelist-listbox-select tree nr)))))
  (when (data tree)
    (treelist-set-root-node tree (data tree))))

(defgeneric treelist-set-root-node (tree node))

(defgeneric treelist-clearlist (tree index))

(defgeneric treelist-setlist (tree parent-node nr))

(defgeneric treelist-listbox-select (tree nr))

(defmethod treelist-set-root-node ((tree treelist) node)
  (setf (data tree) node)
  (treelist-setlist tree node 0))

(defmethod treelist-clearlist ((tree treelist) index)
  (when (< index (depth tree))
    (setf (aref (entries tree) index) nil)
    (listbox-delete (aref (listbox tree) index))
    (treelist-clearlist tree (1+ index))))

(defmethod treelist-setlist ((tree treelist) parent-node nr)
  (when (< nr (depth tree))
    (treelist-clearlist tree nr)
    (let ((entry (make-instance 'tree-entry
                                :nodes (treelist-children tree parent-node)
                                :index nr
                                :parent-node parent-node)))
      (setf (aref (entries tree) nr) entry)
      (listbox-append (aref (listbox tree) nr)
                      (mapcar (lambda (node)
                                (treelist-name tree node))
                              (nodes entry))))))

(defmethod treelist-listbox-select ((tree treelist) nr)
  (let* ((listbox (aref (listbox tree) nr))
         (entry   (aref (entries tree) nr)))
    (when entry
      (let* ((oldsel (selected-node entry))
             (sel    (first (listbox-get-selection-index listbox))))
        (when oldsel
          (listbox-configure listbox oldsel :background :white :foreground :black))
        (setf (selected-node (aref (entries tree) nr)) sel)
        (when sel
          (listbox-configure listbox sel :background :blue :foreground :white)
          (let* ((entry (aref (entries tree) nr))
                 (selected-node (nth sel (nodes entry))))
            (listbox-configure listbox sel :background :blue :foreground :white)
            (treelist-select tree selected-node)
            (treelist-setlist tree selected-node (1+ nr))))))))

(defgeneric treelist-select (tree node)
  (:documentation "callback for selecting a tree node"))

(defmethod treelist-select (tree node)
    (declare (ignore tree node)))

(defgeneric treelist-children (tree node)
  (:documentation "list of children for a node in a tree"))

(defmethod treelist-children (tree node)
  (declare (ignore tree node))
  nil)

(defgeneric treelist-has-children (tree node)
  (:documentation "is non-nil, if the node has children"))

(defmethod treelist-has-children (tree node)
  (treelist-children tree node))

(defgeneric treelist-name (tree node)
  (:documentation "String to display in the tree list for a node"))

(defmethod treelist-name (tree (node string))
  (declare (ignore tree)))

;;; demo tree widget

(defparameter *tree*
  '(nil
    ("BMW"
     ("3er"
      "318"
      "320"
      "325")
     ("5er"
      "520"
      "530"
      "535"
      "M5"))
    ("Mercedes"
     ("A-Klasse"
      "A 160"
      "A 180")
     ("C-Klasse"
      "C 200"
      "C 250")
     ("S-Klasse"
      "400 S"
      "500 S"
      "600 S"))
    ("VW"
     ("Golf"
      ("TDI"
       "1.8"
       "2.0"
       "16 V")
      "GTI"))))

(defclass demo-tree (treelist)
  ())

(defmethod treelist-name ((tree demo-tree) (node list))
  (car node))

(defmethod treelist-children ((tree demo-tree) (node list))
  (rest node))

(defmethod treelist-name ((tree demo-tree) (node string))
  node)

(defmethod treelist-children ((tree demo-tree) (node string))
  nil)

(defun treelist-test ()
  (with-nodgui ()
    (pack (make-instance 'demo-tree :data *tree*) :expand t :fill :both)))

;;;; tooltip widget

(defclass tooltip (toplevel)
  ((label
    :accessor tooltip-label
    :initarg :label)
   (popup-time
    :accessor popup-time
    :initform 500
    :initarg :popup-time)))

(defparameter *tooltip-afterid* nil)

(defmethod initialize-instance :after ((tooltip tooltip) &key)
  (withdraw tooltip)
  (setf (tooltip-label tooltip)
        (make-instance 'label
                       :text       ""
                       :background :yellow3
                       :master     tooltip
                       :justify    :left))
  (set-wm-overrideredirect tooltip 1)
  (pack (tooltip-label tooltip) :side :left :expand t :fill :both))

(defgeneric show (tooltip text x y))

(defgeneric popup-tooltip (tooltip))

(defgeneric cancel-tooltip (tooltip))

(defgeneric register-tooltip (tooltip widget content))

(defmethod show ((tooltip tooltip) text x y)
  (let ((txt (typecase text
               (function
                (with-output-to-string (s)
                  (funcall text s)))
               (string
                text)
               (t
                (to-s text)))))
    (when (and txt (> (length txt) 0))
      (setf (text (tooltip-label tooltip)) txt)
      (set-geometry-xy tooltip (truncate x)  (truncate y))
      (normalize tooltip)
      (raise tooltip))))

(defmethod popup-tooltip ((tooltip tooltip))
  (normalize tooltip)
  (raise tooltip))

(defgeneric schedule-tooltip (tooltip text x y time))

(defmethod schedule-tooltip (tooltip text x y time)
  (cancel-tooltip tooltip)
  (setf *tooltip-afterid*
        (after time (lambda ()
                      (show tooltip text x y)))))

(defmethod cancel-tooltip ((tooltip tooltip))
  (when *tooltip-afterid*
    (after-cancel *tooltip-afterid*)
    (setf *tooltip-afterid* nil)))

(defmethod clear ((tooltip tooltip))
  (withdraw tooltip))

(defmethod register-tooltip ((tooltip tooltip) (widget widget) content)
  (bind widget #$<Leave>$ (lambda (event)
                            (declare (ignore event))
                            (clear tooltip)
                            (cancel-tooltip tooltip))
        :append t)
  (bind widget #$<Motion>$ (lambda (event)
                             (clear tooltip)
                             (cancel-tooltip tooltip)
                             (schedule-tooltip tooltip
                                               content
                                               (+ (event-root-x event) 10)
                                               (- (event-root-y event) 10)
                                               (popup-time tooltip)))
        :append t)
  widget)

(defmethod configure ((tooltip tooltip) option value &rest others)
  (apply #'configure (tooltip-label tooltip) option value others))

(defun tooltip-test ()
  (with-nodgui ()
    (let ((b       (make-instance 'button :text "Tooltip"))
          (tooltip (make-instance 'tooltip)))
      (pack b)
      (configure tooltip :borderwidth 1 :relief :solid)
      (register-tooltip tooltip b (lambda (s) (format s "~d" (random 10000)))))))

;;;; graphical tree widget

(defclass gtree (canvas)
  ((data
    :accessor data
    :initform nil
    :initarg :data))
  (:documentation "Render a tree.
   Data should be tree in a form of cons cell like:
   '(a (b (d (h)
             (i))
          (e (j)
             (k)))
       (c (f)
          (g)))"))

(defgeneric render-tree (g d x y))

(defmethod render-tree ((g gtree) data x y)
  (let ((h 0))
    (when (gtree-content g data)
      (if (gtree-children g data)
        (dolist (c (gtree-children g data))
          (incf h (render-tree g c (+ x 100) (+ y h))))
        (incf h 30))
      (let* ((c (gtree-render-node g (gtree-content g data)))
             (w (create-window g x (+ y (truncate h 2)) c)))
        (declare (ignore w))))
    h))

(defmethod initialize-instance :after ((g gtree) &key)
  (render-tree g (data g) 0 0))

(defgeneric gtree-children (gtree node))

(defgeneric gtree-content (gtree node))

(defgeneric gtree-render-node (gtree node))

(defclass gtree-demo (gtree) ())

(defmethod gtree-children ((d gtree-demo) (node list))
  (rest node))

(defmethod gtree-content ((d gtree-demo) (node list))
  (first node))

(defmethod gtree-render-node ((d gtree-demo) node )
  (make-instance 'label
                 :master d
                 :text node
                 :relief :raised
                 :background :grey
                 :width 10))

(defun gtree-demo ()
  (with-nodgui ()
    (let* ((tree (make-instance 'gtree-demo
                                :data '(a (b (d (h)
                                              (i))
                                           (e (j)
                                            (k)))
                                        (c (f)
                                         (g))))))
      (pack tree :side :left :expand t :fill :both)
      (format t "data: ~s~%" (data tree))
      (force-output))))

;;; list-select box widget

(defclass list-select (listbox)
  ((data
    :accessor data
    :initarg :data
    :initform nil)))

(defgeneric list-select-display (select item))

(defmethod list-select-display ((select list-select) item)
  (format nil "~a" item))

(defgeneric selected-elements (select))

(defmethod selected-elements ((select list-select))
  (let ((selection (listbox-get-selection select)))
    (when selection
      (mapcar (lambda (index)
                (nth index (data select)))
              selection))))

(defmethod (setf data) :after (val (select list-select))
  (listbox-delete select)
  (listbox-append select
                  (mapcar (lambda (item)
                            (list-select-display select item))
                          (data select))))

;;; seachable-listbox

(defclass searchable-listbox (frame)
  ((scrolled-listbox
    :accessor scrolled-listbox
    :initform nil
    :initarg  :scrolled-listbox)
   (listbox
    :accessor listbox
    :initform nil
    :initarg  :listbox)
   (entry
    :accessor entry
    :initform nil
    :initarg  :entry)
   (data
    :accessor data
    :initform nil
    :initarg  :data
    :documentation "The items of this listbox")
   (key
    :accessor key
    :initform #'identity
    :initarg  :key
    :documentation "This function is applied to every element in 'data'
    before the actual match is performed")
   (remove-non-matching-p
    :accessor remove-non-matching-p
    :initform t
    :initarg :remove-non-matching-p
    :documentation  "If true  (generalized boolean)  non-matching item
    are removed from the listbox, if nil just unselected")
   (matching-fn
    :initform #'cl-ppcre:scan
    :initarg  :matching-fn
    :accessor matching-fn
    :documentation "The  filter function  for list entries,  if values
    nil the  entry is removed or  uselected depending of the  value of
    'remove-non-matching-p'.  The parameter ar the text of 'entry' and
    the value of the item.")
   (entry-label
    :initform "Search"
    :initarg  :entry-label
    :accessor entry-label
    :documentation  "The  label  near  the  text  entry  where  filter
    criteria is typed")
   (displayed
    :accessor displayed
    :initform nil
    :initarg :displayed))
   (:documentation "A listbox with an entry to filter its contents"))

(defgeneric get-searchable-listbox-data (lb))

(defgeneric update-search (lb string))

(defgeneric search-text (object))

(defmethod get-searchable-listbox-data ((lb searchable-listbox))
  (mapcar (key lb) (data lb)))

(defmethod selection ((lb searchable-listbox))
  (cond
    ((remove-non-matching-p lb))
    (t)))

(defmethod update-search ((lb searchable-listbox) searchstring)
  (with-accessors ((listbox     listbox)
                   (matching-fn matching-fn)) lb
    (let ((data    (get-searchable-listbox-data lb)))
      (cond
        ((= (length searchstring) 0)
         (cond
           ((remove-non-matching-p lb)
            (listbox-delete listbox)
            (listbox-append listbox data))
           (t
            (listbox-select listbox nil))))
        (t
         (let ((results (remove-if-not (lambda (item)
                                         (nodgui.conditions:with-default-on-error (t)
                                             (funcall matching-fn searchstring item)))
                                       (data lb))))
         (cond
           ((remove-non-matching-p lb)
            (listbox-delete listbox)
            (when results
              (listbox-append listbox results)))
           (t
            (let ((indexes (mapcar (lambda (item) (position item (data lb) :test #'string=))
                                   results)))
              (listbox-select listbox nil)
              (dolist (index indexes)
                (when index
                  (listbox-select listbox index)))
              (when (car indexes)
                (see listbox (car indexes))))))))))))

(defmethod initialize-instance :after ((lb searchable-listbox)
                                       &key
                                         (select-mode      :browse)
                                         (placeholder-text nil)
                                         (export-selection   nil)
                                         &allow-other-keys)
  (with-accessors ((entry-label entry-label)) lb
    (let* ((scrolled (make-instance 'scrolled-listbox
                                    :master           lb
                                    :export-selection export-selection
                                    :select-mode      select-mode))
           (listbox (listbox scrolled))
           (fsearch (make-instance 'frame :master lb))
           (label (make-instance 'label :master fsearch :text entry-label))
           (entry (make-instance 'entry :master fsearch)))
      (pack scrolled :side :top :fill :both :expand t)
      (pack fsearch  :side :top :fill :x)
      (pack label    :side :left)
      (pack entry    :side :left :fill :x :expand t)
      (setf (scrolled-listbox lb) scrolled
            (listbox          lb) listbox
            (entry            lb) entry)
      (listbox-append listbox (data lb))
      (when placeholder-text
        (setf (text entry) placeholder-text))
      (bind entry #$<KeyPress>$ (lambda (event)
                                  (declare (ignore event))
                                  (update-search lb (text entry)))))))

(defmethod search-text ((object searchable-listbox))
  (text (entry object)))

(defmethod listbox-clear ((object searchable-listbox) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)
                   (data    data)) object
    (listbox-clear listbox start end)
    object))

(defmethod listbox-delete ((object searchable-listbox) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)
                   (data    data)) object
    (listbox-delete listbox start end)
    (setf data nil)
    object))

(defmethod listbox-append ((object searchable-listbox) (values list))
  (with-accessors ((listbox listbox)
                   (data    data)) object
    (listbox-append listbox values)
    (setf data (append data values))
    object))

(defmethod listbox-select ((object searchable-listbox) val)
  (listbox-select (listbox object) val)
  object)

;; TODO: use a macro to generate all those methods
(defmethod listbox-get-selection ((object searchable-listbox))
  (listbox-get-selection (listbox object)))

(defmethod listbox-get-selection-index ((object searchable-listbox))
  (listbox-get-selection-index (listbox object)))

(defmethod listbox-get-selection-value ((object searchable-listbox))
  (listbox-get-selection-value (listbox object)))

(defmethod listbox-select-mode ((object searchable-listbox) (mode symbol))
  (listbox-select-mode (listbox object) mode))

(defmethod listbox-export-selection ((object searchable-listbox) value)
  (listbox-export-selection (listbox object) value))

(defmethod listbox-values-in-range ((object searchable-listbox) &key (from 0) (to :end))
  (listbox-values-in-range (listbox object) :from from :to to))

(defmethod listbox-all-values ((object searchable-listbox))
  (listbox-all-values (listbox object)))

;;; autocomplete-listbox

(defclass autocomplete-listbox (searchable-listbox)
  ((autocomplete-function-hook
    :accessor autocomplete-function-hook
    :initform nil
    :initarg  :autocomplete-function-hook))
  (:documentation "Acts like 'searchable-listbox'  but the items added
  comes from  the results  from apply  'autocomplete-function-hook' to
  the content of  the text entry.  This function is  triggered after a
  character is inserted into the entry."))

(defun %autocomplete (listbox)
  (with-accessors ((autocomplete-function-hook autocomplete-function-hook)
                   (entry                      entry)
                   (master                     master)) listbox
    (listbox-delete listbox)
    (when autocomplete-function-hook
      (with-hourglass (master listbox entry)
        (listbox-append listbox (funcall autocomplete-function-hook
                                         (search-text listbox)))))))

(defmethod initialize-instance :after ((object autocomplete-listbox)
                                       &key (select-mode :browse) &allow-other-keys)
  (with-accessors ((listbox                    listbox)
                   (entry                      entry)
                   (autocomplete-function-hook autocomplete-function-hook)) object
    (listbox-select-mode listbox select-mode)
    (bind entry #$<KeyPress>$ (lambda (event)
                                (declare (ignore event))
                                (%autocomplete object)))))

(defgeneric launch-autocompletion (object))

(defmethod launch-autocompletion ((object autocomplete-listbox))
  (%autocomplete object))

;;; demos

(defun autocomplete-listbox-demo ()
  (flet ((autocomplete (text)
           (remove-if-not #'(lambda (a)
                              (nodgui.conditions:with-default-on-error (nil)
                                (and text
                                     (not (string-empty-p text))
                                     (cl-ppcre:scan text a))))
                          '("A" "B" "C" "a" "aa" "b" "c" "foo" "bar" "lisp"))))
  (with-nodgui ()
    (pack (make-instance 'autocomplete-listbox
                         :placeholder-text           "type a regexp"
                         :autocomplete-function-hook #'autocomplete
                         :remove-non-matching-p nil)
          :fill :both :expand t))))

(defun searchable-listbox-demo ()
  (with-nodgui ()
    (pack (make-instance 'searchable-listbox
                         :data (loop for i from 1 to 100
                                  collect (format nil "Nummer: ~d" i))
                         :matching-fn #'cl-ppcre:scan ; just to show the initarg
                         :remove-non-matching-p nil)
          :fill :both :expand t)))

(defclass list-select-demo-entry ()
  ((file
    :accessor file
    :initarg  :file
    :initform nil)
   (size
    :accessor size
    :initarg  :size
    :initform 0)))

(defmethod list-select-display ((ls list-select) (entry list-select-demo-entry))
  (format nil "~a ~d Bytes" (namestring (file entry)) (size entry)))

(defun make-list-select-demo (&optional (master nil))
  (let* ((f (make-instance 'frame :master master))
         (ls (make-instance 'list-select :master f :selectmode :multiple))
         (f2 (make-instance 'frame :master f))
         (lsize (make-instance 'label :master f2 :text "Total Size:"))
         (bsize (make-instance 'button
                               :text    "Calc"
                               :master  f2
                               :command (lambda ()
                                          (setf (text lsize)
                                                (format nil "Total Size: ~a"
                                                        (loop for e in (selected-elements ls)
                                                           summing (size e))))))))
    (pack ls :side :top :expand t :fill :both)
    (pack f2 :side :top :fill :x)
    (pack bsize :side :left)
    (pack lsize :side :left)
    (setf (data ls)
          (mapcar (lambda (p)
                    (make-instance 'list-select-demo-entry
                                   :file p
                                   :size (with-open-file (s p)
                                           (file-length s))))
                  (directory (make-pathname :name :wild :type :wild))))
    f))

(defun list-select-demo ()
  (with-nodgui ()
    (let ((f (make-list-select-demo)))
      (pack f :side :top :expand t :fill :both))))

(defun text-input-dialog (parent title message
                          &key
                            (text           nil)
                            (button-message "OK")
                            (padding-x         2)
                            (padding-y         2))
  "A trivial dialog that waits for a textual input from user"
  (let ((res nil))
    (with-modal-toplevel (toplevel :title title)
      (transient toplevel parent)
      (flet ((close-window-cb (entry)
               (lambda ()
                 (setf res (text entry))
                 (setf *break-mainloop* t))))
        (let* ((label  (make-instance 'label
                                      :master toplevel
                                      :text   message))
               (entry  (make-instance 'entry
                                      :text   text
                                      :master toplevel))
               (button (make-instance 'button
                                      :text    button-message
                                      :command (close-window-cb entry)
                                      :master toplevel)))
          (bind entry #$<Return>$ (lambda (a)
                                    (declare (ignore a))
                                    (funcall (close-window-cb entry))))
          (grid label  0 0 :sticky :n :padx padding-x :pady padding-y)
          (grid entry  1 0 :sticky :n :padx padding-x :pady padding-y)
          (grid button 2 0 :sticky :n :padx padding-x :pady padding-y))))
    res))

(alexandria:define-constant +selection-mode-allow-double-click+ '(:single :browse) :test #'equalp)

(defun listbox-dialog (parent title message data
                       &key
                         (select-mode       :browse)
                         (key               #'identity)
                         (button-message    "OK")
                         (padding-x         2)
                         (padding-y         2))
  "A trivial dialog with a listbox that waits for user to select input"
  (let ((res nil))
    (with-modal-toplevel (toplevel :title title)
      (transient toplevel parent)
      (flet ((close-window-cb (sc-listbox)
               (lambda ()
                 (setf res (listbox-get-selection-value sc-listbox))
                 (setf *break-mainloop* t))))
        (let* ((label      (make-instance 'label
                                          :master toplevel
                                          :text   message))
               (sc-listbox (make-instance 'scrolled-listbox
                                          :select-mode select-mode
                                          :master      toplevel))
               (button     (make-instance 'button
                                          :text    button-message
                                          :command (close-window-cb sc-listbox)
                                          :master  toplevel)))
          (dolist (datum data)
            (listbox-append sc-listbox (funcall key datum)))
          (when (find select-mode +selection-mode-allow-double-click+)
            (bind (listbox sc-listbox) ; internal slot
                  #$<Double-1>$
                  (lambda (event)
                    (declare (ignore event))
                    (funcall (close-window-cb sc-listbox)))
                  :append    t))
          (grid label      0 0 :sticky :n :padx padding-x :pady padding-y)
          (grid sc-listbox 1 0 :sticky :news :padx padding-x :pady padding-y)
          (grid button     2 0 :sticky :n :padx padding-x :pady padding-y)
          (grid-rowconfigure    toplevel 1 :weight 1)
          (grid-columnconfigure toplevel 0 :weight 1))))
    res))

(a:define-constant +date-today-dom-wrapper+  "*" :test #'string=)

(defclass date-picker (frame)
  ((on-pressed-cb
    :initform #'identity
    :initarg  :on-pressed-cb
    :accessor on-pressed-cb
    :documentation  "When  a  day  button   is  pressed  the  date  is
    updated  (see slot  'date') and  this function  is called  with an
    istance of 'date-picker' as parameter")
   (universal-timestamp
    :initform (get-universal-time)
    :initarg  :universal-timestamp
    :accessor universal-timestamp
    :documentation "Universal time of the selected date")
   (months-name
    :initform '("January"
                "February"
                "March"
                "April"
                "May"
                "June"
                "July"
                "August"
                "September"
                "October"
                "November"
                "December")
    :initarg  :months-name
    :accessor months-name
    :documentation "List of labels for months names")
   (weekday-names
    :initform '("Mon"
                "Tue"
                "Wed"
                "Thu"
                "Fry"
                "Sat"
                "Sun")
    :initarg  :weekday-names
    :accessor weekday-names
    :documentation "List of labels for the days of the abbreviated week names")
   (current-month-entry
    :initform nil
    :initarg :current-month-entry
    :accessor current-month-entry)
   (current-year-entry
    :initform nil
    :initarg :current-year-entry
    :accessor current-year-entry)
   (all-days-buttons
    :initform '()
    :accessor all-days-buttons))
  (:documentation "A widget to choose a date"))

(defun time-as-list (univ-time)
  (multiple-value-list (decode-universal-time univ-time)))

(defmacro with-time-as-list ((decoded-time universal-time) &body body)
  `(let ((,decoded-time (time-as-list ,universal-time)))
     ,@body))

(defun date-format-month (date-object)
  (with-accessors ((universal-timestamp universal-timestamp)
                   (months-name         months-name)) date-object
    (with-time-as-list (decoded-time universal-timestamp)
      (elt months-name (1- (time-month-of decoded-time))))))

(defun date-format-year (date-object)
  (with-accessors ((universal-timestamp universal-timestamp)) date-object
    (with-time-as-list (decoded-time universal-timestamp)
      (time-year-of decoded-time))))

(defun date-format-week-day (date-object &optional (day nil))
  (with-accessors ((universal-timestamp universal-timestamp)
                   (weekday-names weekday-names)) date-object
    (with-time-as-list (decoded-time universal-timestamp)
      (if day
          (elt weekday-names (rem day 7))
          (elt weekday-names (time-day-of decoded-time))))))

(defun date-sundayp (index)
  (and (> index 0)
       (= (rem index 6) 0)))

(defun date-color-by-week (idx)
  (if (date-sundayp idx)
      "#ff0000"
      "#000000"))

(defun date-month-name->month-num (date-object name)
  (1+ (position name (months-name date-object) :test #'string=)))

(defun date-month-num->month-name (date-object idx)
  (elt (months-name date-object) (1- idx)))

(defun date-build-universal-time (year month day)
  (flet ((parse (n)
           (floor (if (numberp n) n (parse-integer n)))))
    (let ((month    (parse month))
          (year-num (parse year))
          (day-num  (parse day)))
      (encode-universal-time 0 0 0 day-num month year-num 0))))

(defun date-build-universal-time* (date-object day)
  (with-accessors ((current-year-entry  current-year-entry)
                   (current-month-entry current-month-entry)
                   (universal-timestamp universal-timestamp)) date-object
    (with-time-as-list (decoded universal-timestamp)
      (date-build-universal-time (time-year-of decoded)
                                 (time-month-of decoded)
                                 day))))

(defun date-refresh (date-object)
  (with-accessors ((current-year-entry  current-year-entry)
                   (current-month-entry current-month-entry)
                   (all-days-buttons    all-days-buttons)
                   (universal-timestamp universal-timestamp)
                   (on-pressed-cb       on-pressed-cb)) date-object
    (map nil #'destroy all-days-buttons)
    (with-time-as-list (decoded-now (get-universal-time))
      (with-time-as-list (decoded-current universal-timestamp)
        (let* ((decoded-time-this-month (time-as-list (date-build-universal-time* date-object 1)))
               (start-dow               (time-day-of  decoded-time-this-month)))
          (setf (text current-year-entry)  (time-year-of decoded-current))
          (setf (text current-month-entry)
                (date-month-num->month-name date-object
                                            (time-month-of decoded-current)))
          (let ((all-days (loop for i from 0 below 31 collect i)))
            (map nil
                 (lambda (dom)
                   (with-time-as-list (decoded-probe (date-build-universal-time* date-object
                                                                                 (1+ dom)))
                     (let ((current-month (time-month-of decoded-current)))
                       (when (= current-month (time-month-of decoded-probe))
                         (let* ((dom-button (make-instance 'button
                                                           :command
                                                           (lambda ()
                                                             (funcall on-pressed-cb
                                                                      (set-date date-object
                                                                                (1+ dom))))
                                                           :master date-object
                                                           :text   (time-date-of decoded-probe)))
                                (row         (floor (/ (+ start-dow
                                                          dom)
                                                       7)))
                                (col         (- (+ start-dow dom)
                                                (* 7 row))))
                           (when (= (time-date-of decoded-now)
                                    (time-date-of decoded-probe))
                             (setf (text dom-button) (wrap-with (text dom-button)
                                                                +date-today-dom-wrapper+)))
                           (grid dom-button (+ row 3) col :sticky :news)
                           (push dom-button all-days-buttons))))))
                 all-days))))))
    date-object)

(defun shift-date (date-object month-shift-fn year-shift-fn)
  (with-time-as-list (decoded-now (get-universal-time))
    (with-time-as-list (decoded (date-build-universal-time* date-object
                                                            (time-date-of decoded-now)))
      (let ((new (encode-universal-time 0 0 0
                                        (time-date-of           decoded-now)
                                        (funcall month-shift-fn (time-month-of decoded))
                                        (funcall year-shift-fn  (time-year-of  decoded))
                                        0)))
        (setf (universal-timestamp date-object) new)
        (date-refresh date-object)))))

(defun set-date (date-object day)
  (setf (universal-timestamp date-object) (date-build-universal-time* date-object day))
  date-object)

(defun add-a-month-clsr (date-object)
  (lambda ()
    (shift-date date-object
                #'(lambda (a)
                    (if (< a 12)
                        (1+ a)
                        1))
                #'identity)))

(defun subtract-a-month-clsr (date-object)
  (lambda ()
    (shift-date date-object
                #'(lambda (a)
                    (if (<= (1- a) 0)
                        12
                        (1- a)))
                #'identity)))

(defun add-a-year-clsr (date-object)
  (lambda ()
    (shift-date date-object #'identity #'1+)))

(defun subtract-a-year-clsr (date-object)
  (lambda ()
    (shift-date date-object
                #'identity
                #'(lambda (a)
                    (if (> a 1900)
                        (1- a)
                        a)))))

(defun date-jump-today (date-object)
  (lambda ()
    (setf (universal-timestamp date-object) (get-universal-time))
    (date-refresh date-object)))

(defun password-char-placeholder ()
  (bullet))

(defmethod initialize-instance :after ((object date-picker) &key &allow-other-keys)
  (with-accessors ((current-year-entry  current-year-entry)
                   (current-month-entry current-month-entry)
                   (all-days-buttons    all-days-buttons)) object
    (let* ((top-frame    (make-instance 'frame             :master object))
           (a-month-less (make-instance 'button
                                        :command (subtract-a-month-clsr object)
                                        :text    (left-arrow)
                                        :master  top-frame))
           (a-month-more (make-instance 'button
                                        :command (add-a-month-clsr object)
                                        :text    (right-arrow)
                                        :master  top-frame))
           (a-year-less  (make-instance 'button
                                        :command (subtract-a-year-clsr object)
                                        :text    (double-left-arrow)
                                        :master  top-frame))
           (a-year-more  (make-instance 'button
                                        :command (add-a-year-clsr object)
                                        :text    (double-right-arrow)
                                        :master  top-frame))
           (today        (make-instance 'button
                                        :command (date-jump-today object)
                                        :text    (big-dot)
                                        :master  top-frame)))
      (setf current-month-entry (make-instance 'entry
                                               :text   (date-format-month  object)
                                               :master top-frame))
      (setf current-year-entry (make-instance 'entry
                                               :text   (date-format-year object)
                                               :master top-frame))
      (grid a-year-less         0 0)
      (grid a-month-less        0 1)
      (grid today               0 2)
      (grid a-month-more        0 3)
      (grid a-year-more         0 4)
      (grid current-month-entry 0 5)
      (grid current-year-entry  0 6)
      (grid top-frame           0 0 :sticky :news :columnspan 7)
      (loop for col from 0 below 7 do
           (let ((weekday (make-instance 'label
                                         :foreground (date-color-by-week col)
                                         :master     object
                                         :text       (date-format-week-day object col))))
             (grid weekday 1 col :sticky :ns)))
      (date-refresh object)
      (grid-columnconfigure top-frame :all :weight 1)
      (grid-rowconfigure    top-frame :all :weight 1)
      (grid-columnconfigure object :all :weight 1)
      (grid-rowconfigure    object :all :weight 1)
      object)))

(defun date-picker-demo ()
  (let ((res nil))
    (with-modal-toplevel (toplevel)
      (set-geometry-wh toplevel 380 256)
      (let* ((widget (make-instance 'date-picker
                                    :master        toplevel
                                    :on-pressed-cb (lambda (a)
                                                     (setf res (universal-timestamp a))
                                                     (break-mainloop)))))
        (grid widget 0 0 :sticky :news)
        (grid-columnconfigure toplevel :all :weight 1)
        (grid-rowconfigure    toplevel :all :weight 1)))
    (and res
         (message-box (format nil
                              "chosen ~s~%"
                              (multiple-value-list (decode-universal-time res)))
                      "info"
                      :ok
                      "info"
                      :parent *tk*))))

(defclass password-entry (entry)
  ((secret-string
    :initform nil
    :initarg  :secret-string
    :accessor secret-string
    :documentation "The secret data kept by thi instance")
   (show-password
    :initform nil
    :initarg  :show-password
    :reader   show-password-p
    :writer   (setf show-password)
    :documentation "Show the password if double click on the entry? Default is nil")))

(defmethod initialize-instance :after ((object password-entry) &key &allow-other-keys)
  (with-accessors ((secret-string   secret-string)
                   (text            text)
                   (show-password-p show-password-p))
      (bind object #$<KeyPress>$
            (lambda (a) (declare (ignore a)))
            :exclusive t
            :append nil)
    (bind object #$<KeyRelease>$
          (lambda (a)
            (cond
              ((string= (event-char a) nodgui.event-symbols:+backspace+)
               (when (> (length secret-string) 0)
                 (setf secret-string (subseq secret-string 0 (1- (length secret-string))))
                 (setf text          (subseq text          0 (1- (length text))))))
              ((nodgui.event-symbols:keysym-printable-p (event-char-code a))
               (setf secret-string (strcat secret-string (event-char a)))
               (setf text          (strcat text          (password-char-placeholder))))))
          :exclusive t
          :append nil)
    (when show-password-p
      (bind object #$<Double-1>$
            (lambda (a)
              (declare (ignore a))
              (setf text secret-string))
            :exclusive t
            :append nil))))

(defun password-entry-demo ()
  (let ((res nil))
    (with-modal-toplevel (toplevel)
      (let* ((widget    (make-instance 'password-entry
                                       :show-password t
                                       :master        toplevel))
             (ok-button (make-instance 'button
                                       :text   "OK"
                                       :master toplevel
                                       :command (lambda ()
                                                  (setf res (secret-string widget))
                                                  (break-mainloop)))))
        (grid widget    0 0 :sticky :news)
        (grid ok-button 0 1 :sticky :news)))
    (and res
         (message-box (format nil "pssst: ~s" res)
                      "info"
                      :ok
                      "info"
                      :parent *tk*))))

(defclass progress-bar-star (canvas)
  ((star-num
    :initform 5
    :initarg :star-num
    :accessor star-num
    :documentation "The number of stars forming this bar")
   (stars
    :accessor stars
    :documentation "The single star-shaped item in canvas (as instance
    of bicolor-star)")
   (reached-color
    :initform "#FFFF00"
    :accessor reached-color
    :documentation "The color of star when reached")
   (not-reached-color
    :initform "#BEBEBE"
    :accessor not-reached-color
    :documentation "The color of star when not reached")
   (value
    :initform 0.0
    :initarg :value
    :reader   value
    :documentation "The status of the progress in [0.0, 1.0]")))

(defun colorize-progress-star (progress-widget)
  (with-lazy
      (with-accessors ((star-num          star-num)
                       (value             value)
                       (reached-color     reached-color)
                       (not-reached-color not-reached-color)
                       (stars             stars)) progress-widget
        (multiple-value-bind (full-colored partial-colored)
            (calc-color-stars progress-widget value)
          (loop for i from 0 below full-colored do
               (let* ((star         (elt stars i))
                      (left-handle  (nodgui.shapes:left-side-handle star))
                      (right-handle (nodgui.shapes:right-side-handle star)))
                 (item-configure progress-widget left-handle  :fill    reached-color)
                 (item-configure progress-widget left-handle  :outline reached-color)
                 (item-configure progress-widget right-handle :fill    reached-color)
                 (item-configure progress-widget right-handle :outline reached-color)))
          (when (>= partial-colored 0)
            (let* ((half-star    (elt stars partial-colored))
                   (left-handle  (nodgui.shapes:left-side-handle half-star)))
              (item-configure progress-widget left-handle :outline reached-color)
              (item-configure progress-widget left-handle :fill    reached-color)))))))

(defmethod (setf value) (new-value (object progress-bar-star))
  (clear-star-progress-star object)
  (setf (slot-value object 'value) (alexandria:clamp new-value 0.0 1.0))
  (colorize-progress-star object))

(defun progress-star-radius (canvas)
  (/ (width canvas)
     (* 2 (star-num canvas))))

(defun %make-progress-star (canvas left-color right-color bbox-fix)
  (let* ((radius (progress-star-radius canvas))
         (star   (nodgui.shapes:make-two-color-star canvas radius 0.5
                                                    left-color  left-color
                                                    right-color left-color
                                                    5
                                                    :outline-width 0)))
    (setf (nodgui.shapes:bbox-fix star) bbox-fix)
    star))

(defun clear-star-progress-star (bar)
  (with-lazy
      (loop for star in (stars bar) do
           (let* ((left-handle  (nodgui.shapes:left-side-handle star))
                  (right-handle (nodgui.shapes:right-side-handle star)))
             (item-configure bar left-handle  :fill    (not-reached-color bar))
             (item-configure bar left-handle  :outline (not-reached-color bar))
             (item-configure bar right-handle :fill    (not-reached-color bar))
             (item-configure bar right-handle :outline (not-reached-color bar))))))

(defun calc-color-stars (bar progress-value)
  (with-accessors ((star-num star-num)
                   (value    value)) bar
    (let* ((star-height         (* 2 (progress-star-radius bar)))
           (bar-width           (width bar))
           (filled-width        (* progress-value bar-width))
           (stars-colored-fract (/ filled-width star-height)))
      (multiple-value-bind (integer-part fractional-part)
          (floor stars-colored-fract)
        (let ((full-colored    integer-part)
              (partial-colored -1))
          (when (not (epsilon= fractional-part 0.0))
            (cond
              ((> fractional-part 0.5)
               (incf full-colored))
              (t
               (if (= full-colored 0)
                   (setf partial-colored 0)
                   (setf partial-colored full-colored)))))
          (values full-colored partial-colored))))))

(defmethod initialize-instance :after ((object progress-bar-star)
                                       &key (bbox-fix 0.95)  &allow-other-keys)
  (with-accessors ((star-num          star-num)
                   (value             value)
                   (reached-color     reached-color)
                   (not-reached-color not-reached-color)
                   (stars             stars)) object
    (setf stars (loop repeat star-num collect
                     (%make-progress-star object
                                          not-reached-color
                                          not-reached-color
                                          bbox-fix)))
    (loop
       for star in stars
       for x from 0 by (* 2 (progress-star-radius object)) do
         (nodgui.shapes:shape-move-to star x 0))
    (colorize-progress-star object)))

(defun star-progress-demo ()
  (with-modal-toplevel (toplevel)
    (let* ((widget (make-instance 'progress-bar-star
                                   :star-num 5
                                   :width    200
                                   :height   40
                                   :master toplevel))
           (scale  (make-instance 'scale
                                  :master  toplevel
                                  :form    0
                                  :to      100)))
      (setf (command scale) (lambda (a)
                              (declare (ignore a))
                              (let ((v (/ (value scale)
                                          100)))
                                (setf (value widget) v))))
      (grid widget 0 0 :sticky :news)
      (grid scale  1 0 :sticky :news))))

(defun message-with-timeout-callback (parent message timeout close-button-label
                                      expired-callback
                                      &rest label-options)
  "Create a window with a message that automatically close after a timeout
   - parent:             the new window will be placed on top of parent
   - message:            the label to display
   - timeout:            the timeout after the window is destroyed (in seconds)
   - close-button-label: the label of the button to close this window
   - expired-callback:   function with no parameters called after the timeout has expired
   - label-options:      the optional options for the message label (e.g. '(:font \"bold\"))
   Note: do not use the callback to modify widget in the same process that created this window."
  (let* ((button-clicked-p nil)
         (toplevel         (make-instance 'toplevel))
         (label            (apply #'make-instance
                                  'label
                                  (append (list :text   message)
                                          (list :master toplevel)
                                          label-options)))
         (progress-timeout (make-instance 'progressbar
                                          :master toplevel
                                          :initial-value 0.0))
         (ok-button        (make-instance 'button
                                          :text    close-button-label
                                          :master  toplevel
                                          :command (lambda ()
                                                     (setf button-clicked-p t)
                                                     (withdraw toplevel))))
         (wish-subprocess  *wish*)
         (exit-mainloop    *exit-mainloop*)
         (break-main-loop  *break-mainloop*))
    (transient toplevel parent)
    (grid label            0 0 :sticky :news)
    (grid progress-timeout 1 0 :sticky :news)
    (grid ok-button        2 0 :sticky :ns)
    (on-close toplevel (lambda () nil))
    (raise toplevel parent)
    (destructuring-bind (w-parent h-parent x-parent y-parent)
        (geometry parent)
      (destructuring-bind (w h x y)
          (geometry parent)
        (declare (ignore x y))
        (set-geometry-xy toplevel
                         (- (+ x-parent (/ w-parent 2))
                            (/ w 2))
                         (- (+ y-parent (/ h-parent 2))
                            (/ h 2)))))
    (bt:make-thread (lambda ()
                      (let ((*wish*           wish-subprocess)
                            (*break-mainloop* break-main-loop)
                            (*exit-mainloop*  exit-mainloop))
                        (loop for i from 0 below timeout do
                             (sleep 1)
                             (when (not button-clicked-p)
                               (setf (value progress-timeout)
                                     (* 100 (coerce (/ i timeout)
                                                    'single-float)))))
                        (when (not button-clicked-p)
                          (funcall expired-callback)
                          (withdraw toplevel)))))))

(defun message-with-timeout (parent message timeout close-button-label &rest label-options)
  "Create a window with a message that automatically close after a timeout
   - parent:             the new window will be placed on top of parent
   - message:            the label to display
   - timeout:            the timeout after the window is destroyed (in seconds)
   - close-button-label: the label of the button to close this window
   - label-options:      the optional options for the message label (e.g. '(:font \"bold\"))"
  (apply #'message-with-timeout-callback
         parent message
         timeout
         close-button-label
         (constantly t)
         label-options))

(defclass multifont-listbox (scrolled-text)
  ((selected-index
    :initform 0
    :initarg :selected-index
    :accessor selected-index)
   (selected-tag
    :initform (create-tag-name)
    :initarg :selected-tag
    :accessor selected-tag)
   (items
    :initform '()
    :initarg  :items
    :accessor items)))

(defun set-multifont-listbox-read-only (widget)
  (with-accessors ((selected-index selected-index)
                   (selected-tag   selected-tag)
                   (items          items)) widget
    (bind (inner-text widget)
          #$<KeyPress>$
          (lambda (event)
            (let ((keycode (event-char event))
                  (remap-indices-p nil))
              (cond
                ((string= keycode nodgui.event-symbols:+up+)
                 (setf remap-indices-p t)
                 (setf selected-index (max 0
                                           (1- selected-index))))
                ((string= keycode nodgui.event-symbols:+down+)
                 (setf remap-indices-p t)
                 (setf selected-index  (rem (1+ selected-index)
                                            (length items)))))
              (when remap-indices-p
                (let ((selected-line-index (1+ selected-index)))
                   (tag-delete widget selected-tag)
                   (move-cursor-to widget `(:line ,selected-line-index :char 0))
                   (setf selected-tag (highlight-text-line widget selected-line-index))
                   (see widget (raw-coordinates widget))))))
          :exclusive t)))

(defun sync-multifont-data (widget)
  (with-accessors ((items          items)
                   (selected-index selected-index)
                   (selected-tag   selected-tag)) widget
    (wait-complete-redraw)
    (let ((max-line-length (width-in-chars (inner-text widget))))
      (clear-text widget)
      (loop for item in items do
        (let ((padding (- max-line-length (length item))))
          (if (> padding 0)
              (append-line widget (strcat item (make-string padding
                                                            :initial-element #\Space)))
              (append-line widget item))))
      (when selected-index
        (let ((selected-line-index (1+ selected-index)))
          (see widget `(:line ,selected-line-index :char 0))
          (move-cursor-to widget `(:line ,selected-line-index :char 0))
          (setf selected-tag (highlight-text-line widget selected-line-index))))
      widget)))

(defun boldify-multifont-item (widget line bold-char-indices)
  (loop for index in bold-char-indices do
    (let ((tag-name (create-tag-name)))
      (tag-create widget
                  tag-name
                  `(:line ,line :char ,index)
                  `(:line ,line :char ,(1+ index)))
      (tag-configure widget
                     tag-name
                     :font "bold"))))

(defmethod initialize-instance :after ((object multifont-listbox) &key &allow-other-keys)
  (set-multifont-listbox-read-only object)
  (configure object :wrap :none)
  (bind (inner-text object)
        #$<ButtonPress-1>$
        (lambda (e)
          (declare (ignore e))
          (let* ((new-selected-line  (cursor-index object))
                 (new-selected-index (1- new-selected-line)))
            (when (and (>= new-selected-index 0)
                       (<  new-selected-index (listbox-size object)))
              (listbox-select object new-selected-index))))))

(defparameter *force-sync-data-multifont-listbox* t)

(defmacro with-sync-data ((widget) &body body)
  (let ((last-form         (a:last-elt body))
        (all-but-last-form (subseq body 0 (1- (length body)))))
    `(progn
       ,@all-but-last-form
       (prog1
           ,last-form
         (when *force-sync-data-multifont-listbox*
           (sync-multifont-data ,widget))))))

(defmethod listbox-append ((object multifont-listbox) (vals list))
  (with-sync-data (object)
    (with-accessors ((items items)) object
      (loop for value in vals do
        (let ((reversed-items (nreverse items)))
          (push value reversed-items)
          (setf items (nreverse reversed-items)))))))

(defmethod listbox-append ((object multifont-listbox) vals)
  (listbox-append object (list vals)))

(defun multifont-translate-end-tcl->lisp (end-value)
  (if (eq end-value :end)
      nil
      end-value))

(defmethod listbox-delete ((object multifont-listbox) &optional (start 0) (end :end))
  (with-sync-data (object)
    (with-accessors ((items items)) object
      (let* ((actual-end (multifont-translate-end-tcl->lisp end)))
        (if (null actual-end)
            (setf items (subseq items 0 start))
            (setf items
                  (append (subseq items 0 start)
                          (subseq items actual-end))))))))

(defmethod listbox-get-selection-index ((object multifont-listbox))
  (list (selected-index object)))

(defmethod listbox-get-selection-value ((object multifont-listbox))
  (list (elt (items object) (selected-index object))))

(defmethod listbox-values-in-range ((object multifont-listbox) &key (from 0) (to :end))
  (with-accessors ((items items)) object
    (let ((actual-end (multifont-translate-end-tcl->lisp to)))
      (subseq items from actual-end))))

(defmethod listbox-all-values ((object multifont-listbox))
  (items object))

(defmethod listbox-move-selection ((object multifont-listbox) offset)
  (with-sync-data (object)
    (with-accessors ((selected-index selected-index)) object
      (incf selected-index offset))))

(defmethod listbox-clear  ((object multifont-listbox) &optional (start 0) (end :end))
  (with-sync-data (object)
    (let ((actual-end (or (multifont-translate-end-tcl->lisp end)
                          (length (items object)))))
      (when (<= start (selected-index object) (1- actual-end))
        (setf (selected-index object) nil)))))

(defmethod listbox-select ((object multifont-listbox) (val number))
  "modify the selection in listbox, if nil is given, the selection is cleared,
if  a  number   is  given  the  corresponding   element  is  selected."
  (with-accessors ((selected-index selected-index)) object
    (with-sync-data (object)
      (setf selected-index val))))

(defmethod listbox-select ((object multifont-listbox) (val null))
  (listbox-clear object))

(defmethod listbox-size ((object multifont-listbox))
  (length (items object)))

(defclass autocomplete-candidates (toplevel)
  ((listbox
    :initform nil
    :initarg :listbox
    :accessor listbox)
   (attached-entry
    :initform nil
    :initarg :attached-entry
    :accessor attached-entry)))

(defmethod initialize-instance :after ((object autocomplete-candidates) &key &allow-other-keys)
  (setf (listbox object) (make-instance 'multifont-listbox
                                        :master object))
  (set-wm-overrideredirect object 1)
  (pack (listbox object) :side :left :expand t :fill :both)
  (hide-candidates object))

(defun show-candidates (candidates)
  (with-accessors ((attached-entry attached-entry)) candidates
    (wait-complete-redraw)
    (let ((x-entry (root-x attached-entry))
          (y-entry (root-y attached-entry))
          (h-entry (window-height attached-entry)))
      (set-geometry-xy candidates
                       (truncate x-entry)
                       (truncate (+ y-entry h-entry)))
      (set-geometry-wh candidates
                       (window-width  attached-entry)
                       (window-height candidates))
      (normalize candidates)
      (raise candidates))))

(defun hide-candidates (candidates)
  (withdraw candidates))

(defmethod configure ((object autocomplete-candidates) option value &rest others)
  (apply #'configure (listbox object) option value others))

(defmethod listbox-append ((l autocomplete-candidates) values)
  (listbox-append (listbox l) values)
  l)

(defmethod listbox-get-selection ((l autocomplete-candidates))
  (listbox-get-selection (listbox l)))

(defmethod listbox-get-selection-index ((object autocomplete-candidates))
  (listbox-get-selection-index (listbox object)))

(defmethod listbox-get-selection-value ((object autocomplete-candidates))
  (listbox-get-selection-value (listbox object)))

(defmethod listbox-select ((l autocomplete-candidates) val)
  (listbox-select (listbox l) val)
  l)

(defmethod listbox-select-mode ((object autocomplete-candidates) (mode symbol))
  (listbox-select-mode (listbox object) mode))

(defmethod listbox-export-selection ((object autocomplete-candidates) value)
  (listbox-export-selection (listbox object) value))

(defmethod listbox-clear ((object autocomplete-candidates) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)
                   (data    data)) object
    (listbox-clear listbox start end)
    object))

(defmethod listbox-delete ((object autocomplete-candidates) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)) object
    (listbox-delete listbox start end)
    object))

(defmethod listbox-values-in-range ((object autocomplete-candidates) &key (from 0) (to :end))
  (with-accessors ((listbox listbox)) object
    (listbox-values-in-range listbox :from from :to to)))

(defmethod listbox-all-values ((object autocomplete-candidates))
  (with-accessors ((listbox listbox)) object
    (listbox-all-values listbox)))

(defmethod listbox-move-selection ((object autocomplete-candidates) offset)
  (with-accessors ((listbox listbox)) object
    (listbox-move-selection listbox offset)))

(defmethod see ((object autocomplete-candidates) pos)
  (with-accessors ((listbox listbox)) object
    (see listbox `(:line ,(1+ pos) :char 0))))

(defclass autocomplete-entry ()
  ((entry-widget
    :initform nil
    :initarg :entry-widget
    :accessor entry-widget)
   (candidates-widget
    :initform (make-instance 'autocomplete-candidates)
    :initarg  :candidates-widget
    :accessor  candidates-widget
    :type     (or null autocomplete-candidates))
   (autocomplete-function
    :initform (lambda (hint) (values hint '()))
    :initarg  :autocomplete-function
    :accessor autocomplete-function
    :type     function
    :documentation "A function that accepts a single parameter and return two values:
 - the list of candidates (or nil) that matches `hint' and are suitable to complete the text contained in the entry;
- a list where each element is a list of index value that idicates the matching character in the corresponding string, for example:

first value:  (\"foo\" \"school\")
second value: ((1 2) (3 4))

The matching character are the two 'o' in the candidates."))
  (:documentation
   "A text  entry that display in  a listbox, the possible  candidates to
complete the  text input.  Clicking on  a listbox  item will  fill the
entry with the  clicked item's text, pressing <tab>  complete the text
with the  selected item;  finally pressing  \"up\" or  \"down\" arrows
will shift the selected item up o down respectively."))

(defmethod pack ((object autocomplete-entry)
                 &key
                   (side :top)
                   fill
                   expand
                   after
                   before
                   padx
                   pady
                   ipadx
                   ipady
                   anchor)
  (pack (entry-widget object)
        :side   side
        :fill   fill
        :expand expand
        :after  after
        :before before
        :padx   padx
        :pady   pady
        :ipadx  ipadx
        :ipady  ipady
        :anchor anchor))

(defmethod grid ((object autocomplete-entry)  row column
                 &key
                   columnspan
                   ipadx
                   ipady
                   padx
                   pady
                   rowspan
                   sticky)
  (grid (entry-widget object)
        row
        column
        :columnspan columnspan
        :ipadx      ipadx
        :ipady      ipady
        :padx       padx
        :pady       pady
        :rowspan    rowspan
        :sticky     sticky))

(defun autocomplete-click-1-clsr (candidates-widget entry-widget)
  (lambda (event)
    (declare (ignore event))
    (a:when-let ((selected (listbox-get-selection-value candidates-widget)))
      (setf (text entry-widget)
            (first selected))
      (set-cursor-index entry-widget :end)
      (focus entry-widget)
      (hide-candidates candidates-widget))))

(defun autocomplete-keypress-clsr (candidates-widget entry-widget autocomplete-function)
  (lambda (event)
    (declare (ignore event))
    (let ((hint (text entry-widget)))
      (multiple-value-bind (candidates matching-indices)
           (funcall autocomplete-function hint)
      (if (string-empty-p hint)
          (hide-candidates candidates-widget)
          (let ((*force-sync-data-multifont-listbox* nil))
            (listbox-delete candidates-widget)
            (listbox-append candidates-widget candidates)
            (listbox-select candidates-widget 0)
            (sync-multifont-data (listbox candidates-widget))
            (when matching-indices
              (loop for i from 0 below (length matching-indices) do
                (boldify-multifont-item (listbox candidates-widget)
                                        (1+ i)
                                        (elt matching-indices i))))
            (show-candidates candidates-widget)))))))

(defun scroll-candidates (candidates-widget offset)
  (lambda (event)
    (declare (ignore event))
    (listbox-move-selection candidates-widget offset)
    (see candidates-widget (first (listbox-get-selection-index candidates-widget)))))

(defmethod initialize-instance :after ((object autocomplete-entry)
                                       &key (master nil) &allow-other-keys)
  (with-accessors ((entry-widget          entry-widget)
                   (candidates-widget     candidates-widget)
                   (autocomplete-function autocomplete-function)) object
    (setf entry-widget (make-instance 'entry :master master))
    (setf (attached-entry candidates-widget) entry-widget)
    (bind candidates-widget #$<1>$ (autocomplete-click-1-clsr candidates-widget entry-widget))
    (bind entry-widget #$<KeyPress-Down>$ (scroll-candidates candidates-widget 1))
    (bind entry-widget #$<KeyPress-Up>$ (scroll-candidates candidates-widget -1))
    (bind entry-widget #$<KeyPress-Tab>$ (autocomplete-click-1-clsr candidates-widget entry-widget)
          :exclusive t)
    (bind entry-widget
          #$<KeyPress>$
          (autocomplete-keypress-clsr candidates-widget entry-widget autocomplete-function)
          :append t)))

(defmethod configure ((object autocomplete-entry) option value &rest others)
  (apply #'configure (entry-widget object) option value others))

(defmethod text ((object autocomplete-entry))
  (text (entry-widget object)))

(defun autocomplete-entry-test ()
  (with-nodgui ()
    (let* ((data                  (append '("foo" "bar" "baz")
                                          (loop for i from 0 to 10 collect
                                                                   (format nil "~2,'0d" i))))
           (autocomplete-function (lambda (hint)
                                    (loop for datum in data when (cl-ppcre:scan hint datum)
                                          collect
                                          (multiple-value-bind (start end)
                                              (cl-ppcre:scan hint datum)
                                              (values (loop for i from start below end collect i)
                                                      datum)))))
           (autocomplete-widget   (make-instance 'autocomplete-entry
                                                 :autocomplete-function autocomplete-function))
           (button-command        (lambda ()
                                    (do-msg (format nil
                                                    "selected ~s~%"
                                                    (text (entry-widget autocomplete-widget))))))
           (button                (make-instance 'button
                                                 :text "OK"
                                                 :command button-command)))
      (grid autocomplete-widget 0 0)
      (grid button              1 0))))
