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



#|

This is the Nodgui megawidgets package. It consists of widgets usable
for Nodgui, written in Lisp/tcl. So wherever the Nodgui package runs, this
extensing package should run as well.


Widgets offered are:

- history-entry
    An entry widget keeping the history of previous input (which can be
    browsed through with cursor up and down)

- treelist
    A widget to display a tree-like structure by a set of listboxes.

- tooltip
    Shows tooltips for registered widgets

- searchable list

- graphical tree display

|#

(in-package :nodgui.mw)

(cl-syntax:use-syntax nodgui-event-syntax)

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
    :accessor history
    :initform (list))
   (history-pos
    :accessor  history-pos
    :initform -1)
   (keepinput
    :accessor keepinput
    :initform nil
    :initarg  :keepinput)))

(defgeneric add-history (entry txt))

(defgeneric clear-history (entry))

(defmethod add-history ((entry history-entry) (txt string))
  (when (> (length txt) 0)
      (push txt (history entry)))
  (setf (history-pos entry) -1))

(defmethod clear-history ((entry history-entry))
  (setf (history entry) nil)
  (setf (history-pos entry) -1))

(defmethod initialize-instance :after ((entry history-entry) &key command)
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
  (when command
    (setf (command entry) command)))

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
    :documentation "list of selected values")))

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
                (format nil "~a" text)))))
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
                                               (+ 30 (event-root-x event))
                                               (+ 10 (event-root-y event))
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
  ((data :accessor data
         :initform nil
         :initarg :data)))

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
      (format t "data: ~s~%" (data tree)) (force-output))))

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
    :initarg  :data)
   (key
    :accessor key
    :initform #'identity
    :initarg  :key)
   (remove-non-matching-p
    :accessor remove-non-matching-p
    :initform t
    :initarg :remove-non-matching-p)
   (matching-fn
    :initform #'cl-ppcre:scan
    :initarg  :matching-fn
    :accessor matching-fn)
   (entry-label
    :initform "Search"
    :initarg  :entry-label
    :accessor entry-label)
   (displayed
    :accessor displayed
    :initform nil
    :initarg :displayed)))

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

;;; autocomple-listbox

(defclass autocomplete-listbox (searchable-listbox)
  ((autocomplete-function-hook
    :accessor autocomplete-function-hook
    :initform nil
    :initarg  :autocomplete-function-hook)))

(defun %autocomplete (listbox)
  (with-accessors ((autocomplete-function-hook autocomplete-function-hook)) listbox
    (listbox-delete listbox)
    (when autocomplete-function-hook
      (listbox-append listbox (funcall autocomplete-function-hook
                                       (search-text listbox))))))

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
                                     (not (string= text ""))
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
                         (button-message "OK")
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
