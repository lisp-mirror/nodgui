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

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(alexandria:define-constant +legal-select-mode-values+ '(:single :browse :multiple :extended)
  :test #'equalp)

(defargs listbox ()
  activestyle
  background
  borderwidth
  cursor
  disabledforeground
  exportselection
  font
  foreground
  height
  highlightbackground
  highlightcolor
  highlightthickness
  relief
  selectbackground
  selectborderwidth
  selectforeground
  selectmode
  setgrid
  state
  takefocus
  width
  xscrollcommand
  yscrollcommand
  listvariable)

(defwrapper listbox (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil))
  "listbox")

(defmethod (setf command) (val (listbox listbox))
  (add-callback (name listbox) val)
  (format-wish "bind ~a <<ListboxSelect>> {callbackval {~a} ([~a curselection])}"
               (widget-path listbox)
               (name listbox)
               (widget-path listbox))
  val)

(defgeneric listbox-append (l vals))

(defgeneric listbox-select (l val))

(defgeneric listbox-get-selection (l))

(defgeneric listbox-get-selection-index (l))

(defgeneric listbox-get-selection-value (l))

(defgeneric listbox-select-mode (object mode))

(defgeneric listbox-clear (l &optional start end))

(defgeneric listbox-delete (l &optional start end))

(defgeneric listbox-insert (l index values))

(defgeneric listbox-configure (l i &rest options))

(defgeneric listbox-nearest (listbox y))

(defgeneric listbox-export-selection (object value))

(defgeneric listbox-values-in-range (object &key from to))

(defgeneric listbox-all-values (object))

(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (format-wish "~a insert end ~{ \"~a\"~}" (widget-path l) values)
      (format-wish "~a insert end \"~a\"" (widget-path l) values))
  l)

(defmethod listbox-get-selection ((l listbox))
  "please use listbox-get-selection-index; if  you want the *value* of
   the selection use: listbox-get-selection-index instead"
  (format-wish "senddata \"([~a curselection])\"" (widget-path l))
  (read-data :expected-list-as-data t))

(defmethod listbox-get-selection-index ((object listbox))
  (format-wish (tclize `(senddata [ ,#[widget-path object ] " " curselection])))
  (read-data :expected-list-as-data t))

(defmethod listbox-get-selection-value ((object listbox))
  (let ((indices (listbox-get-selection-index object)))
    (loop for i in indices collect
         (let ((*add-space-after-emitted-unspecialized-element* nil))
           (format-wish (tclize `(senddatastring [ ,#[widget-path object ] " "
                                                 get {+ ,#[i ]} ])))
           (read-data)))))

(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if  a  number   is  given  the  corresponding   element  is  selected,
alternatively a list of numbers may be given"
  (if (null val)
      (format-wish "~a selection clear 0 end" (widget-path l))
      (if (listp val)
          (format-wish "~a selection set ~{ {~a}~}" (widget-path l) val)
          (format-wish "~a selection set {~a}" (widget-path l) val)))
  l)

(defmethod listbox-clear ((l listbox) &optional (start 0) (end :end))
  "Clear selected elements of this listbox"
  (format-wish (tclize `(,(widget-path l) " "
                          selection clear {+ ,#[down start ] } {+ ,#[down end ] })))
  l)

(defmethod listbox-delete ((l listbox) &optional (start 0) (end :end))
  "Delete elements from listbox"
  (format-wish (tclize `(,#[widget-path l ] " "
                         delete
                         {+ ,#[down start ] }
                         ,(empty-string-if-nil end
                             `({+ ,#[down end ] })))))
  l)

(defmethod listbox-insert ((l listbox) index values)
  (if (listp values)
      (format-wish "~a insert {~a} ~{ \"~a\"~}" (widget-path l) index values)
      (format-wish "~a insert {~a} \"~a\"" (widget-path l) index values))
  l)

(defmethod listbox-configure ((l listbox) index &rest options)
  (format-wish "~a itemconfigure {~a} ~{ {-~(~a~)} {~a}~}"
               (widget-path l)
               index
               (mapcar #'down options))
  l)

(defmethod listbox-nearest ((l listbox) y)
  (format-wish "senddata [~a nearest {~a}]" (widget-path l) y)
  (read-data))

(defmethod see ((lb listbox) pos)
  (format-wish "~a see {~(~a~)}" (widget-path lb) pos)
  lb)

(alexandria:define-constant +legal-select-mode-values+ '(:single :browse :multiple :extended)
  :test #'equalp)

(defmethod listbox-select-mode ((object listbox) (mode symbol))
  (assert (find mode +legal-select-mode-values+))
  (format-wish (tclize `(,#[widget-path object ] " "
                          configure -selectmode {+ ,#[down mode ] }))))

(defmethod listbox-export-selection ((object listbox) value)
  (format-wish (tclize `(,#[widget-path object ] " "
                          configure -exportselection ,#[lisp-bool->tcl value ]))))

(defmethod listbox-values-in-range ((object listbox) &key (from 0) (to :end))
  "Get the values of the entries in a listbox in range [from to]"
  (format-wish (tclize
                `(senddatastrings [ ,#[widget-path object ] " "
                                  get
                                  {+ ,#[down from ] }
                                  {+ ,#[down to ] }
                           ])))
  (read-data))

(defmethod listbox-all-values ((object listbox))
  "Get all values of a listbox"
  (listbox-values-in-range object :from 0 :to :end))

(defclass scrolled-listbox (frame)
  ((listbox
    :initform nil
    :accessor listbox)
   (hscroll
    :initform nil
    :accessor hscroll)
   (vscroll
    :initform nil
    :accessor vscroll)))

(defmethod initialize-instance :after ((object scrolled-listbox)
                                       &key
                                         (select-mode       :browse)
                                         (export-selection   nil)
                                         &allow-other-keys)
  (setf (hscroll object) (make-scrollbar object :orientation "horizontal"))
  (setf (vscroll object) (make-scrollbar object))
  (setf (listbox object) (make-instance 'listbox
                                        :master  object
                                        :xscroll (hscroll object)
                                        :yscroll (vscroll object)))
  (grid (listbox object) 0 0 :sticky :news)
  (grid (hscroll object) 1 0 :sticky :we)
  (grid (vscroll object) 0 1 :sticky :ns)
  (grid-columnconfigure object 0 :weight 1)
  (grid-columnconfigure object 1 :weight 0)
  (grid-rowconfigure object 0 :weight 1)
  (grid-rowconfigure object 1 :weight 0)
  (configure (hscroll object) "command"        (strcat (widget-path (listbox object)) " xview"))
  (configure (vscroll object) "command"        (strcat (widget-path (listbox object)) " yview"))
  (configure (listbox object) "xscrollcommand" (strcat (widget-path (hscroll object)) " set"))
  (configure (listbox object) "yscrollcommand" (strcat (widget-path (vscroll object)) " set"))
  (listbox-export-selection object export-selection)
  (listbox-select-mode object select-mode))

(defmethod listbox-append ((l scrolled-listbox) values)
  (listbox-append (listbox l) values)
  l)

(defmethod listbox-get-selection ((l scrolled-listbox))
  (listbox-get-selection (listbox l)))

(defmethod listbox-get-selection-index ((object scrolled-listbox))
  (listbox-get-selection-index (listbox object)))

(defmethod listbox-get-selection-value ((object scrolled-listbox))
  (listbox-get-selection-value (listbox object)))

(defmethod listbox-select ((l scrolled-listbox) val)
  (listbox-select (listbox l) val)
  l)

(defmethod listbox-select-mode ((object scrolled-listbox) (mode symbol))
  (listbox-select-mode (listbox object) mode))

(defmethod listbox-export-selection ((object scrolled-listbox) value)
  (listbox-export-selection (listbox object) value))

(defmethod listbox-clear ((object scrolled-listbox) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)
                   (data    data)) object
    (listbox-clear listbox start end)
    object))

(defmethod listbox-delete ((object scrolled-listbox) &optional (start 0) (end :end))
  (with-accessors ((listbox listbox)) object
    (listbox-delete listbox start end)
    object))

(defmethod listbox-values-in-range ((object scrolled-listbox) &key (from 0) (to :end))
  (with-accessors ((listbox listbox)) object
    (listbox-values-in-range listbox :from from :to to)))

(defmethod listbox-all-values ((object scrolled-listbox))
  (with-accessors ((listbox listbox)) object
    (listbox-all-values listbox)))
