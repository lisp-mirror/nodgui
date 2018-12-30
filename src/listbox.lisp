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
  (format-wish "bind ~a <<ListboxSelect>> {callbackval ~a ([~a curselection])}"
               (widget-path listbox)
               (name listbox)
               (widget-path listbox))
  val)

(defgeneric listbox-append (l vals))

(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (format-wish "~a insert end ~{ \{~a\}~}" (widget-path l) values)
      (format-wish "~a insert end \{~a\}" (widget-path l) values))
  l)

(defgeneric listbox-get-selection (l))

(defgeneric listbox-get-selection-index (l))

(defgeneric listbox-get-selection-value (l))

(defmethod listbox-get-selection ((l listbox))
  "please use listbox-get-selection-index; if  you want the *value* of
   the selection use: listbox-get-selection-index instead"
  (format-wish "senddata \"([~a curselection])\"" (widget-path l))
  (read-data :expected-list-as-data t))

(defmethod listbox-get-selection-index ((object listbox))
  (format-wish (tclize `(senddata [ ,(widget-path object) " " curselection])))
  (read-data :expected-list-as-data t))

(defmethod listbox-get-selection-value ((object listbox))
  (let ((indices (listbox-get-selection-index object)))
    (loop for i in indices collect
         (progn
           (format-wish (tclize `(senddatastrings [ ,(widget-path object) " " get ,i ])))
           (alexandria:first-elt (read-data))))))

(defgeneric listbox-select (l val))

(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if a number is given the corresponding element is selected, alternatively
a list of numbers may be given"
  (if (null val)
      (format-wish "~a selection clear 0 end" (widget-path l))
      (if (listp val)
          (format-wish "~a selection set ~{ ~a~}" (widget-path l) val)
          (format-wish "~a selection set ~a" (widget-path l) val)))
  l)

(defgeneric listbox-clear (l))

(defmethod listbox-clear ((l listbox))
  (format-wish "~a delete 0 end" (widget-path l))
  l)

(defgeneric listbox-delete (l start &optional end))

(defmethod listbox-delete ((l listbox) start &optional end)
  (format-wish "~a delete ~a ~@[~(~a~)~]" (widget-path l) start end)
  l)

(defgeneric listbox-insert (l index values))

(defmethod listbox-insert ((l listbox) index values)
  (if (listp values)
      (format-wish "~a insert ~a ~{ \{~a\}~}" (widget-path l) index values)
      (format-wish "~a insert ~a \{~a\}" (widget-path l) index values))
  l)

(defgeneric listbox-configure (l i &rest options))

(defmethod listbox-configure ((l listbox) index &rest options)
  (format-wish "~a itemconfigure ~a ~{ -~(~a~) {~/nodgui::pprint-down/}~}" (widget-path l) index options)
  l)

(defgeneric listbox-nearest (listbox y))

(defmethod listbox-nearest ((l listbox) y)
  (format-wish "senddata [~a nearest ~a]" (widget-path l) y)
  (read-data))

(defmethod see ((lb listbox) pos)
  (format-wish "~a see ~(~a~)" (widget-path lb) pos)
  lb)

(defclass scrolled-listbox (frame)
  ((listbox :accessor listbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)))

(defmethod initialize-instance :after ((sl scrolled-listbox) &key)
  (setf (hscroll sl) (make-scrollbar sl :orientation "horizontal"))
  (setf (vscroll sl) (make-scrollbar sl))
  (setf (listbox sl) (make-instance 'listbox :master sl :xscroll (hscroll sl) :yscroll (vscroll sl)))
  (grid (listbox sl) 0 0 :sticky "news")
  (grid (hscroll sl) 1 0 :sticky "we")
  (grid (vscroll sl) 0 1 :sticky "ns")
  (grid-columnconfigure sl 0 :weight 1)
  (grid-columnconfigure sl 1 :weight 0)
  (grid-rowconfigure sl 0 :weight 1)
  (grid-rowconfigure sl 1 :weight 0)

  (configure (hscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " xview"))
  (configure (vscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " yview"))
  (configure (listbox sl) "xscrollcommand" (concatenate 'string (widget-path (hscroll sl)) " set"))
  (configure (listbox sl) "yscrollcommand" (concatenate 'string (widget-path (vscroll sl)) " set")))

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
