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

(define-constant +treeview-root+             "{}" :test #'string=)

(define-constant +treeview-last-index+      "end" :test #'string=)

(define-constant +treeview-first-column-id+  "#0"
  :test #'string=
  :documentation "The conventional id of the primary value of each item of a treeview")

(defun escape-node-if-not-root (s)
  (if (string= s +treeview-root+)
      +treeview-root+
      `(\"+ ,s \")))

(defargs treeview (widget)
  xscrollcommand
  yscrollcommand
  columns
  displaycolumns
  height
  padding
  selectmode
  show)

;;; treeview widget

(defwrapper treeview (tktextvariable widget)
  ((xscroll
    :accessor xscroll
    :initarg  :xscroll
    :initform nil)
   (yscroll
    :accessor yscroll
    :initarg  :yscroll
    :initform nil)
   (items
    :accessor items
    :initarg :items
    :initform nil))
  "ttk::treeview")

(defmethod print-object ((object treeview) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~%~{ ~a~%~}" (items object))))

(defgeneric setup-display-columns (object column-ids))

(defgeneric setup-columns (object column-ids
                           &key column-labels min-widths widths))

(defun send-wish-columns-data (widget switch column-ids)
  (assert (or (string= switch "columns")
              (string= switch "displaycolumns")))
  (let ((column-list (with-no-emitted-spaces
                       (with-no-emitted-newline
                         (tclize (loop for i in column-ids collect `({+ ,i })))))))
    (format-wish (tclize `(,(widget-path widget) " "
                           configure
                           ,(format nil "-~a" switch) " "
                           [+ list ,(make-bypass-escape :data column-list) ])))))

(defmethod setup-columns ((object treeview) (column-ids list)
                          &key
                            (column-labels (append (list "") column-ids))
                            (min-widths (make-fresh-list (1+ (length column-ids)) 10))
                            (widths     (make-fresh-list (1+ (length column-ids)) 200)))
  "Set id,  header and width for the columns of this treeview"
  (assert column-ids)
  (assert (= (1+ (length column-ids)) ;; the first column has always id '#0'
             (length column-labels)
             (length min-widths)
             (length widths)))
  (assert (every #'numberp widths))
  (assert (every #'numberp min-widths))
  (assert (every #'(lambda (a) (> a 0)) widths))
  (assert (every #'(lambda (a) (> a 0)) min-widths))
  (send-wish-columns-data object "columns" column-ids)
  (loop
     for ct        from 0 by 1
     for id        in (append (list :placeholder) column-ids)
     for label     in column-labels
     for min-width in min-widths
     for width     in widths        do
       (let ((*suppress-newline-for-tcl-statements* t)
             (actual-id    (if (= 0 ct)
                               +treeview-first-column-id+
                               id))
             (actual-label (if (= 0 ct)
                               ""
                               label)))
         (format-wish (tclize `(,(widget-path object) " "
                                 heading {+ ,actual-id } " "
                                 -text {+ ,actual-label })))
         (format-wish (tclize `(,(widget-path object) " "
                                 column {+ ,actual-id  } " "
                                 ,(empty-string-if-nil min-width
                                      `(-minwidth  {+ ,min-width } " "))
                                -width {+ ,width }))))))

(defmethod setup-display-columns ((object treeview) (column-ids sequence))
  (send-wish-columns-data object "displaycolumns" column-ids))

(defclass tree-item ()
  ((id
    :accessor id
    :initform (create-name)
    :initarg  :id)
   (tree
    :accessor tree
    :initform nil
    :initarg  :tree
    :documentation "The container widget (i.e. a treeview instance)")
   (index
    :accessor index
    :initform +treeview-last-index+
    :initarg  :index
    :documentation "the position of this node among to its siblings")
   (parent
    :accessor parent
    :initform +treeview-root+
    :initarg  :parent
    :documentation "The id of the parent of this item")
   (text
    :accessor text
    :initform nil
    :initarg :text
    :documentation "The primary value of this item")
   (image
    :accessor image
    :initform nil
    :initarg :image)
   (tag
    :accessor tag
    :initform nil
    :initarg :tag
    :documentation "The tags of this item, used for event handling")
   (column-values
    :accessor column-values
    :initform nil
    :initarg :column-values
    :documentation "The secondary (optional)  values associed with this item"))
  (:documentation "A single node of a treeview"))

(defmethod initialize-instance :after ((item tree-item) &key &allow-other-keys)
  item)

(defmethod print-object ((object tree-item) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (with-accessors ((id     id)
                     (index  index)
                     (parent parent)
                     (text   text)) object
      (format stream
              "id: ~s index: ~s parent: ~s text: ~s"
              id index parent text))))

(defgeneric treeview-insert-item (object &key &allow-other-keys))

(defgeneric treeview-find-item (object item &key key test))

(defgeneric treeview-insert-item-new (object &key test &allow-other-keys))

(defgeneric treeview-refit-columns-width (object))

(defmethod treeview-insert-item ((object treeview)
                                 &key
                                   (item          nil)
                                   (parent        +treeview-root+)
                                   (index         +treeview-last-index+)
                                   (id            (create-name))
                                   (tag           nil)
                                   (text          nil)
                                   (image         nil)
                                   (column-values nil)
                                   &allow-other-keys)
  "Create a  tree-item and insert it into  the tree, by default  items are
added after the last one, and the ids are autogenerated"
  (flet ((build-column-values (treeitem)
           (let ((raw-values (column-values treeitem)))
             (with-no-emitted-newline
               (with-no-emitted-spaces
                 (with-no-escape-tilde
                   (when raw-values
                     (loop for i in raw-values collect (tclize `(\"+ ,i \"))))))))))
    (let* ((res                  (or item
                                     (make-instance 'tree-item
                                                    :tree          object
                                                    :parent        parent
                                                    :index         index
                                                    :id            id
                                                    :tag           tag
                                                    :text          text
                                                    :image         image
                                                    :column-values column-values)))
           (actual-column-values (build-column-values res)))
      (push res (items object))
      (let ((*suppress-newline-for-tcl-statements*             t)
            (*add-space-after-emitted-unspecialized-element* nil))
        (format-wish (tclize `(,(widget-path object)      " "
                               insert
                               ,(escape-node-if-not-root (parent res))
                               " "
                               ,index  " "
                               -id  \"+ ,(id res) \" " "
                               ,(empty-string-if-nil (text res)
                                                     `(-text  ,(text res) ))
                               " "
                               ,(empty-string-if-nil (image res)
                                                     `(-image  {+ ,(image res) }))
                               ,(empty-string-if-nil actual-column-values
                                                     `(-values
                                                       [list ,(make-bypass-escape :data actual-column-values) ]))
                               ,(empty-string-if-nil (tag res)
                                                     `(-tags {+  ,(tag res) }))))))
      res)))

(defmethod treeview-find-item ((object treeview) item &key (key #'identity) (test #'eq))
  (find item (items object) :key key :test test))

(defmethod treeview-insert-item-new ((object treeview)
                                     &key
                                       (item          nil)
                                       (test          #'eq)
                                       (parent        +treeview-root+)
                                       (index         +treeview-last-index+)
                                       (id            (create-name))
                                       (tag           nil)
                                       (text          nil)
                                       (image         nil)
                                       (column-values nil)
                                       &allow-other-keys)
  "Create a  tree-item and insert into  the tree iff the  new item is
not equal to all the others. The test is performed calling :test"
    (let ((res (or item
                   (make-instance 'tree-item
                                  :tree          object
                                  :parent        parent
                                  :index         index
                                  :id            id
                                  :tag           tag
                                  :text          text
                                  :image         image
                                  :column-values column-values))))
    (when (notany #'(lambda (a) (funcall test res a))
                  (items object))
      (treeview-insert-item object :item res))))

(defmethod (setf text) (val (item tree-item))
  (setf (text item) val)
  (format-wish "~a item \"~a\" -text {~A}" (widget-path (tree item)) (id item) val)
  val)

(defmethod (setf image) (val (item tree-item))
  (setf (image item) val)
  (format-wish "~a item \"~a\" -image {~A}" (widget-path (tree item)) (id item) val)
  val)

(defmethod see ((tv treeview) (item tree-item))
  (format-wish "~a see {~a}" (widget-path tv) (id item))
  tv)

(defgeneric children (tree item))

(defmethod children ((tree treeview) (item tree-item))
  "List the children of item. Item must be contained in tree"
  (children tree (id item)))

(defmethod children ((tree treeview) (item string))
  (format-wish (tclize `(senddatastrings [,(widget-path tree) " "
                                         children
                                         ,(escape-node-if-not-root item)
                                         ])))
  (read-data :expected-list-as-data t))

(defgeneric (setf children) (val tree item))

(defmethod (setf children) (val (tree treeview) item)
  (loop for id-val in val do
       (let ((items-change-parent (remove-if-not (lambda (a) (find id-val (id a) :test #'equal))
                                                 (items tree))))
         (loop for item in items-change-parent do
              (setf (parent item) item))))
    (format-wish "~a children \"~a\" {~{\"~a\"~^ ~}}" (widget-path tree) item (mapcar #'id val)))

(defmethod (setf children) (val (tree treeview) (item tree-item))
  (setf (children tree (id item)) val))

(defgeneric column-configure (tree column option value &rest rest))

(defmethod column-configure ((tree treeview) column option value &rest rest)
  (format-wish "{~a} column {~a} {-~(~a~)} {~a}~{ {-~(~a~)} {~(~a~)}~}"
               (widget-path tree) column
               option value rest))

(defgeneric treeview-delete (tree items))

(defgeneric treeview-test-item (a b))

(defmethod treeview-test-item ((a string) (b string))
  (string= a b))

(defmethod treeview-test-item ((a number) (b number))
  (epsilon= a b))

(defmethod treeview-test-item ((a string) (b number))
  (if (safe-parse-number a)
      (treeview-test-item (safe-parse-number a) b)
      nil))

(defmethod treeview-test-item ((a number) (b string))
  (if (safe-parse-number b)
      (treeview-test-item (safe-parse-number b) a)
      nil))

(defmethod treeview-test-item ((a tree-item) (b tree-item))
  (treeview-test-item (id a) (id b)))

(defmethod treeview-test-item (a (b tree-item))
  (treeview-test-item a (id b)))

(defmethod treeview-test-item ((a tree-item) b)
  (treeview-test-item (id a) b))

(defmethod treeview-delete ((tree treeview) item)
  "Delete item and all its children from tree"
  (setf (items tree)
        (remove item (items tree) :test #'treeview-test-item))
  (setf (items tree)
        (remove-if (lambda (a) (treeview-test-item (parent a) item))
                   (items tree)))
  (format-wish "~a delete \"~a\"" (widget-path tree) item))

(defmethod treeview-delete ((tree treeview) (item tree-item))
  (treeview-delete tree (id item)))

(defmethod treeview-delete ((tree treeview) (items list))
  (loop for item in items do
       (treeview-delete tree item)))

(defgeneric treeview-delete-all (tree))

(defmethod treeview-delete-all ((tree treeview))
  "Delete all items contained in tree, if any"
  (let ((children (children tree +treeview-root+)))
    (treeview-delete tree children)))

(defgeneric treeview-exists (tree item))

(defmethod treeview-exists ((tree treeview) item)
  "Check if tree contains item"
  (with-read-data (nil)
    (format-wish "senddata [~a exists {~a}]" (widget-path tree) item)
    (tcl-bool->lisp (read-data))))

(defgeneric treeview-focus (tree))

(defmethod treeview-focus ((tree treeview))
  (with-read-data (nil)
    (format-wish "senddatastring [~a focus]" (widget-path tree))
    (let ((name (read-data)))
      (find name (items tree) :key #'id :test #'equal))))

(defgeneric treeview-identify-item (tree x y))

(defmethod treeview-identify-item ((tree treeview) x y)
  (with-read-data (nil)
    (format-wish "senddatastring [~a identify row ~a ~a ]" (widget-path tree)
                 (tk-number x)
                 (tk-number y))
    (let ((name (read-data)))
      (find name (items tree) :key #'id :test #'equal))))

(defgeneric (setf treeview-focus) (item tree))

(defmethod (setf treeview-focus) (item tree)
  (format-wish "~a focus {~a}" (widget-path tree) item))

(defmethod (setf treeview-focus) ((item tree-item) tree)
  (format-wish "~a focus {~a}" (widget-path tree) (id item)))

(defun treeview-item (tree item &rest options)
  "Query or modify the options for the specified item."
  (cond
    ((second options) ;; modify
     (format-wish "~a item {~a} ~{ {-~(~a~)} {~a}~}"
                  (widget-path tree) item (mapcar #'down options)))
    (t ;; query
     (with-read-data ()
       (format-wish "senddatastring [~a item {~a} ~@[ {-~(~a~)}~]]"
                    (widget-path tree) item (car options))))))

(defun treeview-column (tree column &rest options)
  "Query or modify the options for the specified column."
  (cond
    ((second options) ;; modify
     (format-wish "~a column {~a} ~{ {-~(~a~)} {~a}~}"
                  (widget-path tree) column (mapcar #'down options)))
    (t ;; query
     (with-read-data ()
       (format-wish "senddatastring [~a column {~a} ~@[ {-~(~a~)}~]]"
                    (widget-path tree) column (car options))
       (read-data)))))

(defgeneric treeview-heading (object column-id &key text image anchor command))

(defmethod treeview-heading ((object treeview) column-id
                             &key
                               (text    nil)
                               (image   nil)
                               (anchor  nil)
                               (command nil))
  "Add properties (text, callback etc.) to the header of this treeview"
  (let ((callback-name (format nil "~a:~a" (widget-path object) (sanitize column-id)))
        (*suppress-newline-for-tcl-statements* t))
    (when command
      ;; register the callback
      (add-callback callback-name command))
    (format-wish (tclize `(,(widget-path object) " "
                            heading                " "
                            {+ ,column-id }
                            ,(empty-string-if-nil text
                                 `(-text  {+  ,text }))
                            ,(empty-string-if-nil image
                                 `(-image {+  ,image }))
                            ,(empty-string-if-nil anchor
                                 `(-anchor {+ ,(down text) }))
                            ,(empty-string-if-nil command
                                 `(-command { callback ,callback-name })))))))

(defun treeview-move* (tree item &optional (test #'eq) (key #'identity) (parent nil) (index nil))
  (let ((child     (treeview-find-item tree item :test test :key key))
        (parent-id (or parent +treeview-root+)))
    (assert child)
    (setf (parent child) parent-id)
    (format-wish "~a move \"~a\" \"~a\" \"~a\""
                 (widget-path tree)
                 (id item)
                 parent-id
                 (or index  +treeview-last-index+))))

(defgeneric treeview-move (object item &key test key parent index))

(defmethod  treeview-move ((object treeview) item
                           &key (test #'eq) (key #'identity) (parent nil) (index nil))
  (treeview-move* object item test key parent index))

(defgeneric treeview-get-selection (w))

(defmethod treeview-get-selection ((tv treeview))
  "Get the selected items (a list of tree-item) of this treeview"
  (with-read-data (nil)
    (format-wish "senddatastrings [~a selection]" (widget-path tv))
    (let ((names (read-data))
          (items (items tv)))
      (mapcar (lambda (name) (find name items :key #'id :test #'treeview-test-item))
              names))))

(defgeneric treeview-set-selection (w items))

(defmethod treeview-set-selection ((tv treeview) items)
  (format-wish "~a selection set {~{~a ~}}" (widget-path tv) (mapcar #'id items)))

(defmethod treeview-refit-columns-width ((object treeview))
  "Nothe that works only with default font"
  (with-accessors ((items items)) object
    (a:when-let* ((all-values (loop for item in items collect (column-values item)))
                  (all-texts  (loop for item in items collect (text item)))
                  (max-text   (reduce (lambda (a b)
                                        (if (> (length a) (length b))
                                            a
                                            b))
                                      all-texts))
                  (max-values (loop for column from 0 below (length (first all-values))
                                    collect
                                      (let ((max-column ""))
                                        (loop for row from 0 below (length all-values)
                                              do
                                                 (let ((candidate (elt (elt all-values row)
                                                                       column)))
                                                   (when (> (length candidate)
                                                            (length max-column))
                                                     (setf max-column candidate))))
                                        max-column))))
      (flet ((string-width (string)
               (font-measure +tk-text-font+ (strcat string "oooo"))))
        (loop repeat (1+ (length all-values)) do
          (column-configure object +treeview-first-column-id+
                            :minwidth (string-width max-text)))
        (loop for i from 1
              for max-value in max-values do
                (column-configure object
                                  (format nil "#~a" i)
                                  :minwidth (string-width max-value))))))
  object)

(defclass scrolled-treeview (frame)
  ((treeview :accessor treeview)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll))
  (:documentation "A treeview with two bar to scroll the widget"))

(defmethod initialize-instance :after ((object scrolled-treeview)
                                       &key
                                         (columns            nil)
                                         (columns-min-width  nil)
                                         (columns-width      nil)
                                         (displaycolumns     columns)
                                         &allow-other-keys)
  (setf (hscroll object) (make-scrollbar object :orientation "horizontal"))
  (setf (vscroll object) (make-scrollbar object))
  (setf (treeview object) (make-instance 'treeview
                                         :master  object
                                         :xscroll (hscroll object)
                                         :yscroll (vscroll object)))
  (when columns
    (setup-columns object columns
                   :widths     (or columns-width
                                   (make-fresh-list (1+ (length columns)) 200))
                   :min-widths (or columns-min-width
                                   (make-fresh-list (1+ (length columns)) 10)))
    (setup-display-columns object displaycolumns))
  (grid (treeview object) 0 0 :sticky :news)
  (grid (hscroll object) 1 0  :sticky :we)
  (grid (vscroll object) 0 1  :sticky :ns)
  (grid-columnconfigure object 0 :weight 1)
  (grid-columnconfigure object 1 :weight 0)
  (grid-rowconfigure object 0 :weight 1)
  (grid-rowconfigure object 1 :weight 0)
  (configure (hscroll object) "command" (strcat (widget-path (treeview object)) " xview"))
  (configure (vscroll object) "command" (strcat (widget-path (treeview object)) " yview"))
  (configure (treeview object)
             "xscrollcommand"
             (strcat (widget-path (hscroll object)) " set"))
  (configure (treeview object)
             "yscrollcommand"
             (strcat (widget-path (vscroll object)) " set")))

(defmethod print-object ((object scrolled-treeview) stream)
  (print-object (treeview object) stream))

(defmacro with-inner-treeview ((treeview-slot scrolled-treeview) &body body)
  "Syntatic sugar to  access the treeview slot of a scrolled-treeview

  `(with-accessors ((,treeview-slot treeview)) ,scrolled-treeview
     ,@body))"
  `(with-accessors ((,treeview-slot treeview)) ,scrolled-treeview
     ,@body))

(defmethod setup-display-columns ((object scrolled-treeview) (column-ids sequence))
  (with-inner-treeview (treeview object)
    (setup-display-columns treeview column-ids)))

(defmethod setup-columns ((object scrolled-treeview) (column-ids list)
                          &key
                            (column-labels (append (list "") column-ids))
                            (min-widths (make-fresh-list (1+ (length column-ids)) 10))
                            (widths     (make-fresh-list (1+ (length column-ids)) 200)))
  "Set id,  header and width for the columns of this treeview"
  (with-inner-treeview (treeview object)
    (setup-columns treeview column-ids
                   :column-labels column-labels
                   :min-widths    min-widths
                   :widths        widths)))

(defmethod treeview-insert-item ((object scrolled-treeview)
                                 &key
                                   (item      nil)
                                   (parent        +treeview-root+)
                                   (index         +treeview-last-index+)
                                   (id            (create-name))
                                   (tag           nil)
                                   (text          nil)
                                   (image         nil)
                                   (column-values nil)
                                   &allow-other-keys)
  (with-inner-treeview (treeview object)
    (if item
        (treeview-insert-item treeview :item item)
        (treeview-insert-item treeview
                              :parent        parent
                              :index         index
                              :id            id
                              :tag           tag
                              :text          text
                              :image         image
                              :column-values column-values))))

(defmethod treeview-find-item ((object scrolled-treeview) item &key (key #'identity) (test #'eq))
  (with-inner-treeview (treeview object)
    (treeview-find-item treeview item :key key :test test)))

(defmethod treeview-insert-item-new ((object scrolled-treeview)
                                     &key
                                       (test          #'eq)
                                       (parent        +treeview-root+)
                                       (index         +treeview-last-index+)
                                       (id            (create-name))
                                       (tag           nil)
                                       (text          nil)
                                       (image         nil)
                                       (column-values nil)
                                       &allow-other-keys)
  (with-inner-treeview (treeview object)
    (treeview-insert-item-new treeview
                              :test test
                              :parent        parent
                              :index         index
                              :id            id
                              :tag           tag
                              :text          text
                              :image         image
                              :column-values column-values)))

(defmethod treeview-heading ((object scrolled-treeview) column-id
                             &key
                               (text    nil)
                               (image   nil)
                               (anchor  nil)
                               (command nil))
  (with-inner-treeview (treeview object)
    (treeview-heading treeview
                      column-id
                      :text    text
                      :image   image
                      :anchor  anchor
                      :command command)))

(defmethod treeview-get-selection ((object scrolled-treeview))
  (with-inner-treeview (treeview object)
    (treeview-get-selection treeview)))

(defmethod treeview-delete ((object scrolled-treeview) item)
  (with-inner-treeview (treeview object)
    (treeview-delete treeview item)))

(defmethod treeview-delete-all ((object scrolled-treeview))
  (with-inner-treeview (treeview object)
    (treeview-delete-all treeview)))

(defmethod treeview-exists ((object scrolled-treeview) item)
  "Check if tree contains item"
  (with-inner-treeview (treeview object)
    (treeview-exists treeview item)))

(defmethod treeview-move ((object scrolled-treeview) (item tree-item)
                          &key (test #'eq) (key #'identity) (parent nil) (index nil))
  "Moves item, and all its descendant, to position index in parent's list of children.
   If parent is nil the root of the tree is assumed.
   If index is nil  the last index is assumed."
  (with-inner-treeview (treeview object)
    (let ((actual-parent (if (typep parent 'tree-item)
                             (id parent)
                             +treeview-root+))
          (actual-index  (or index +treeview-last-index+)))
      (treeview-move treeview
                     item
                     :test   test
                     :key    key
                     :parent actual-parent
                     :index  actual-index))))

(defmethod children ((object scrolled-treeview) (item tree-item))
  "List the children of item. Item must be contained in tree"
   (with-inner-treeview (treeview object)
     (children treeview (id item))))

(defmethod children ((object scrolled-treeview) (item string))
  "List the children of item. Item must be contained in tree"
   (with-inner-treeview (treeview object)
     (children treeview item)))

(defmethod (setf children) (val (object scrolled-treeview) item)
  "Set the children of a node: e.g. '(setf (children tree item) (list item1 item2...))"
  (with-inner-treeview (treeview object)
    (setf (children treeview item) val)))

(defgeneric column-configure (tree column option value &rest rest))

(defmethod column-configure ((tree scrolled-treeview) column option value &rest rest)
  (with-inner-treeview (treeview tree)
    (apply #'column-configure treeview column option value rest)))

(defmethod items ((object scrolled-treeview))
  "Get the items of this treeview"
  (with-inner-treeview (treeview object)
    (items treeview)))

(defmethod (setf items) (val (object scrolled-treeview))
  "Set the items of this treeview"
  (with-inner-treeview (treeview object)
    (setf (items treeview) val)))

(defmethod treeview-refit-columns-width ((object scrolled-treeview))
  (with-inner-treeview (treeview object)
    (treeview-refit-columns-width treeview)))
