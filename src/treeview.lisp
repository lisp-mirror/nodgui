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

(alexandria:define-constant +treeview-root+             "{}" :test #'string=)

(alexandria:define-constant +treeview-last-index+      "end" :test #'string=)

(alexandria:define-constant +treeview-first-column-id+  "#0"
  :test #'string=
  :documentation "The conventional id of the primary value of each item of a treeview")

(defun escape-node-if-not-root (s)
  (if (string= s +treeview-root+)
      +treeview-root+
      (tclize `({+ ,s }))))

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
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (items   :accessor items   :initform nil :initarg :items))
  "ttk::treeview")

(defgeneric setup-columns (object column-ids
                           &key column-labels min-widths widths))

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
  (format-wish (tclize `(,(widget-path object) " "
                          configure
                          -columns ,(wrap-braces (join-with-strings column-ids " ")))))
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
                                 heading {+ ,actual-id } -text {+ ,actual-label })))
         (format-wish (tclize `(,(widget-path object) " "
                                 column {+ ,actual-id } " "
                                 ,(tclize-if-true min-width
                                      `(-minwidth  {+ ,min-width } " "))
                                 -width ,(wrap-braces width)))))))

(defclass tree-item ()
  ((id
    :accessor id
    :initform nil
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

(defgeneric treeview-insert-item (object &key &allow-other-keys))

(defgeneric treeview-insert-item-new (object &key test &allow-other-keys))

(defmethod treeview-insert-item ((object treeview)
                                 &key
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
  (let ((res                 (make-instance 'tree-item
                                            :tree          object
                                            :parent        parent
                                            :index         index
                                            :id            id
                                            :tag           tag
                                            :text          text
                                            :image         image
                                            :column-values column-values))
        (actual-column-values (let ((*suppress-newline-for-tcl-statements* t))
                                (loop for i in column-values collect
                                     (tclize `(\"+
                                               ,(tkescape i)
                                               \"))))))
    (push res (items object))
    (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(,(widget-path object)      " "
                              insert
                              ,parent                   " "
                              ,index                    " "
                              -id     ,id               " "
                              ,(tclize-if-true text
                                   `(-text  {+ ,(tkescape text) }))
                              ,(tclize-if-true image
                                   `(-image  ,(wrap-braces image)))
                              ,(tclize-if-true column-values
                                   `(-values [list ,actual-column-values ]))
                              ,(tclize-if-true tag
                                   `(-tags   ,(wrap-braces tag)))))))
    res))

(defmethod treeview-insert-item-new ((object treeview)
                                     &key
                                       (test #'eq)
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
  (let ((res (make-instance 'tree-item
                            :tree          object
                            :parent        parent
                            :index         index
                            :id            id
                            :tag           tag
                            :text          text
                            :image         image
                            :column-values column-values)))
    (when (notany #'(lambda (a) (funcall test res a))
                  (items object))
      (treeview-insert-item object
                            :parent        parent
                            :index         index
                            :id            id
                            :tag           tag
                            :text          text
                            :image         image
                            :column-values column-values))))

(defmethod (setf text) (val (item tree-item))
  (format-wish "~a item {~a} -text {~A}" (widget-path (tree item)) (id item) val)
  val)

(defmethod (setf image) (val (item tree-item))
  (format-wish "~a item {~a} -image {~A}" (widget-path (tree item)) (id item) val)
  val)

(defmethod see ((tv treeview) (item tree-item))
  (format-wish "~a see {~a}" (widget-path tv) (id item))
  tv)

(defgeneric children (tree item))

(defmethod children ((tree treeview) (item tree-item))
  "List the children of item. Item must be contained in tree"
  (children tree (id item)))

(defmethod children ((tree treeview) (item string))
  (format-wish (tclize `(senddata [,(widget-path tree) " "
                                  children
                                  ,(escape-node-if-not-root item)
                                  ])))
  (read-data :expected-list-as-data t))

(defgeneric (setf children) (val tree item))

(defmethod (setf children) (val (tree treeview) item)
  (format-wish "~a children {~a} {~{~a~^ ~}}" (widget-path tree) item val))

(defmethod (setf children) (val (tree treeview) (item tree-item))
  (format-wish "~a children {~a} {~{~a~^ ~}}" (widget-path tree) (id item) val))

(defgeneric column-configure (tree column option value &rest rest))

(defmethod column-configure ((tree treeview) column option value &rest rest)
  (format-wish "~a column ~a -~(~a~) {~a}~{ -~(~a~) {~(~a~)}~}" (widget-path tree) column
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
  (format-wish "~a delete {~a}" (widget-path tree) item))

(defmethod treeview-delete ((tree treeview) (item tree-item))
  (treeview-delete tree (id item)))

(defmethod treeview-delete ((tree treeview) (items list))
  (loop for item in items do
       (treeview-delete tree item)))

(defgeneric treeview-delete-all (tree))

(defmethod treeview-delete-all ((tree treeview))
  "Delete all items contained in tree, if any"
  (treeview-delete tree (children tree +treeview-root+)))

(defgeneric treeview-exists (tree item))

(defmethod treeview-exists ((tree treeview) item)
  "Check if tree contains item"
  (format-wish "senddata [~a exists {~a}]" (widget-path tree) item)
  (tcl-bool->lisp (read-data)))

(defgeneric treeview-focus (tree))

(defmethod treeview-focus ((tree treeview))
  (format-wish "senddatastring [~a focus]" (widget-path tree))
  (let ((name (read-data)))
    (find name (items tree) :key #'id :test #'equal)))

(defgeneric treeview-identify-item (tree x y))

(defmethod treeview-identify-item ((tree treeview) x y)
  (format-wish "senddatastring [~a identify row ~a ~a ]" (widget-path tree)
               (tk-number x)
               (tk-number y))
  (let ((name (read-data)))
    (find name (items tree) :key #'id :test #'equal)))

(defgeneric (setf treeview-focus) (item tree))

(defmethod (setf treeview-focus) (item tree)
  (format-wish "~a focus {~a}" (widget-path tree) item))

(defmethod (setf treeview-focus) ((item tree-item) tree)
  (format-wish "~a focus {~a}" (widget-path tree) (id item)))

(defun treeview-insert (tree &rest options
                        &key
                          (parent +treeview-root+)
                          (index  +treeview-last-index+)
                          (id (create-name)) &allow-other-keys)
  "Better use treeview-insert-item"
  ;; Remove the keys that aren't optional in Tcl.
  (remf options :parent)
  (remf options :index)
  (format-wish "~a insert {~a} {~a} ~{ -~(~a~) ~/nodgui::tk-princ/~}"
               (widget-path tree)
               parent
               index
               options)
  #| Note:
  It is tempting to use senddata/read-data and let Tk allocate an id.
  BAD IDEA!  Process swapping causes a massive slowdown (observed 100x longer).
  |#
  id)

(defun treeview-item (tree item &rest options)
  "Query or modify the options for the specified item."
  (cond
    ((second options) ;; modify
     (format-wish "~a item {~a} ~{ -~(~a~) ~/nodgui::tk-princ/~}"
                  (widget-path tree) item options))
    (t ;; query
     (format-wish "senddatastring [~a item {~a} ~@[ -~(~a~)~]]"
                  (widget-path tree) item (car options))
     (read-data))))

(defun treeview-column (tree column &rest options)
  "Query or modify the options for the specified column."
  (cond
    ((second options) ;; modify
     (format-wish "~a column {~a} ~{ -~(~a~) ~/nodgui::tk-princ/~}"
                  (widget-path tree) column options))
    (t ;; query
     (format-wish "senddatastring [~a column {~a} ~@[ {-~(~a~)}~]]"
                  (widget-path tree) column (car options))
     (read-data))))

(defgeneric treeview-heading (object column-id &key text image anchor command))

(defmethod treeview-heading ((object treeview) column-id
                             &key
                               (text    nil)
                               (image   nil)
                               (anchor  nil)
                               (command nil))
  "Add properties (text, callback etc.) to the header of this treeview"
  (let ((callback-name (format nil "~a:~a" (widget-path object) column-id))
        (*suppress-newline-for-tcl-statements* t))
    (when command
      ;; register the callback
      (add-callback callback-name command))
    (format-wish (tclize `(senddatastring [,(widget-path object) " "
                                          heading                " "
                                          ,(wrap-braces column-id)
                                          ,(tclize-if-true text
                                               `(-text  {+  ,(tkescape text) }))
                                          ,(tclize-if-true image
                                               `(-image   ,(wrap-braces image)))
                                          ,(tclize-if-true anchor
                                               `(-anchor  ,(wrap-braces (down text))))
                                          ,(tclize-if-true command
                                               `(-command { callback ,callback-name }))
                                          ])))))

(defun treeview-move (tree item &optional parent index)
  "Moves item to position index in parent's list of children."
  (format-wish "~a move {~a} {~a} {~a}"
               (widget-path tree)
               item
               (or parent +treeview-root+)
               (or index  +treeview-last-index+)))

(defgeneric treeview-get-selection (w))

(defmethod treeview-get-selection ((tv treeview))
  "Get the selected items (a list of tree-item) of this treeview"
  (format-wish "senddatastrings [~a selection]" (widget-path tv))
  (let ((names (read-data))
        (items (items tv)))
    (mapcar (lambda (name) (find name items :key #'id :test #'treeview-test-item))
            names)))

(defgeneric treeview-set-selection (w items))

(defmethod treeview-set-selection ((tv treeview) items)
  (format-wish "~a selection set {~{~a ~}}" (widget-path tv) (mapcar #'id items)))

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
                                   (make-fresh-list (1+ (length columns)) 10))))
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

(defmacro with-inner-treeview ((treeview-slot scrolled-treeview) &body body)
  "Syntatic sugar to  access the treeview slot of a scrolled-treeview

  `(with-accessors ((,treeview-slot treeview)) ,scrolled-treeview
     ,@body))"
  `(with-accessors ((,treeview-slot treeview)) ,scrolled-treeview
     ,@body))

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
                               (parent        +treeview-root+)
                               (index         +treeview-last-index+)
                               (id            (create-name))
                               (tag           nil)
                               (text          nil)
                               (image         nil)
                               (column-values nil)
                               &allow-other-keys)
  (with-inner-treeview (treeview object)
    (treeview-insert-item treeview
                          :parent        parent
                          :index         index
                          :id            id
                          :tag           tag
                          :text          text
                          :image         image
                          :column-values column-values)))

(defmethod treeview-insert-item-new ((object scrolled-treeview)
                                     &key
                                       (test #'eq)
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
