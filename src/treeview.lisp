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

#-:tk84
(defwrapper treeview (tktextvariable widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (items   :accessor items   :initform nil :initarg :items))
  "ttk::treeview")

(defclass treeitem (tkobject)
  ((tree :accessor tree :initform nil :initarg :tree)
   (text :accessor text :initform nil :initarg :text)
   (image :accessor image :initform nil :initarg :image)
   (master :accessor master :initarg :master :initform nil)
   (tag    :accessor tag    :initform nil :initarg :tag)
   (column-values :accessor column-values :initform nil :initarg :column-values)))

(defmethod initialize-instance :after ((item treeitem) &key)
  (setf (name item) (create-name))
  (format-wish "~a insert ~a end -id ~a -text \"~a\" ~@[-tag ~a~] ~@[-image ~a~]" (widget-path (tree item)) (if (master item)
                                                                                    (name (master item))
                                                                                    "{}")
               (name item) (tkescape (text item)) (tag item) (and (image item) (if (stringp (image item))
                                                                        (image item)
                                                                        (name (image item)))))
  (push item (items (tree item)))
  item)

(defmethod (setf text) (val (item treeitem))
  (format-wish "~a item ~a -text {~A}" (widget-path (tree item)) (name item) val)
  val)

(defmethod (setf image) (val (item treeitem))
  (format-wish "~a item ~a -image {~A}" (widget-path (tree item)) (name item) val)
  val)

(defmethod see ((tv treeview) (item treeitem))
  (format-wish "~a see ~a" (widget-path tv) (name item))
  tv)

(defgeneric children (tree item))

(defmethod children ((tree treeview) item)
  (format-wish "~a children ~a" (widget-path tree) item))

(defmethod children ((tree treeview) (item treeitem))
  (format-wish "~a children ~a" (widget-path tree) (name item)))

(defgeneric (setf children) (val tree item))

(defmethod (setf children) (val (tree treeview) item)
  (format-wish "~a children ~a {~{~a~^ ~}}" (widget-path tree) item val))

(defmethod (setf children) (val (tree treeview) (item treeitem))
  (format-wish "~a children ~a {~{~a~^ ~}}" (widget-path tree) (name item) val))

(defgeneric column-configure (tree column option value &rest rest))

(defmethod column-configure ((tree treeview) column option value &rest rest)
  (format-wish "~a column ~a -~(~a~) {~a}~{ -~(~a~) {~(~a~)}~}" (widget-path tree) column
               option value rest))

(defgeneric treeview-delete (tree items))

(defmethod treeview-delete ((tree treeview) item)
  (format-wish "~a delete {~a}" (widget-path tree) item))

(defmethod treeview-delete ((tree treeview) (item treeitem))
  (setf (items tree) (remove item (items tree)))
  (format-wish "~a delete {~a}" (widget-path tree) (name item)))

(defmethod treeview-delete ((tree treeview) (items cons))
   (format-wish "~a delete {~{~a~^ ~}}" (widget-path tree) items))

(defgeneric treeview-exists (tree item))

(defmethod treeview-exists ((tree treeview) item)
  (format-wish "senddata [~a exists ~a]" (widget-path tree) item)
  (= (read-data) 1))

(defgeneric treeview-focus (tree))

(defmethod treeview-focus ((tree treeview))
  (format-wish "senddatastring [~a focus]" (widget-path tree))
  (let ((name (read-data)))
    (find name (items tree) :key #'name :test #'equal)))

(defgeneric treeview-identify-item (tree x y))

(defmethod treeview-identify-item ((tree treeview) x y)
  (format-wish "senddatastring [~a identify row ~a ~a ]" (widget-path tree) x y)
  (let ((name (read-data)))
    (find name (items tree) :key #'name :test #'equal)))


(defgeneric (setf treeview-focus) (item tree))

(defmethod (setf treeview-focus) (item tree)
  (format-wish "~a focus ~a" (widget-path tree) item))

(defmethod (setf treeview-focus) ((item treeitem) tree)
  (format-wish "~a focus ~a" (widget-path tree) (name item)))
