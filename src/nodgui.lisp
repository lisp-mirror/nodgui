;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018,2019 cage

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

;;; tcl -> lisp: puts "$x" mit \ und " escaped
;;;  puts [regsub {"} [regsub {\\} $x {\\\\}] {\"}]

;;; table used for callback every callback consists of a name of a widget and
;;; a function to call

(defun add-callback (sym fun)
  "create a callback sym is the name to use for storage, fun is the function to call"
  (dbg "add-callback (~A ~A)~%" sym fun)
  (setf (gethash sym (wish-callbacks *wish*)) fun))

(defun remove-callback (sym)
  (dbg "remove-callback (~A)~%" sym)
  (setf (gethash sym (wish-callbacks *wish*)) nil))

(defun unistall-callback (sym)
  (remove-callback sym))

(defun install-callback (function)
  "generate an handle to call lisp `function' from tcl, returns the handle that can be used as argumento for tcl commands (e.g. \"-callback\" ot \"tag-bind\")."
  (let ((name (create-name)))
    (dbg "install-callback (~a)~%" name)
    (add-callback name function)
    name))

(defun callback (sym arg)
  "perform the call of the function associated with sym and the args arg"
  (let ((fun (gethash sym (wish-callbacks *wish*))))
    (when fun
      (apply fun arg))))

(defun senddatastring (tclcmd args)
  (let ((fmt (format nil (tcl-send-data-code) tclcmd)))
    (apply 'format-wish fmt args)))

(defun after (time fun)
  "after <time> msec call function <fun>, returns the after event id,
which can be passed to AFTER-CANCEL"
  (incf (wish-after-counter *wish*))
  (let* ((name (create-name (format nil "after~a" (wish-after-counter *wish*))))
         (id   (with-read-data ()
                 (format-wish (tcl-str (senddatastring [after {~a} {callback ~A}]))
                              time
                              name)))
         (blah (wish-after-ids *wish*)))
    (setf (gethash id blah) name)
    (add-callback name
                  (lambda ()
                    (funcall fun)
                    (remhash id blah)
                    (remove-callback name)))
    id))

(defun after-idle (fun)
 "call fun when tk becomes idle, returns the after event id, which
can be passed to AFTER-CANCEL"
  (with-read-data (nil)
    (let ((name (format nil "afteridle~a" (incf (wish-after-counter *wish*)))))
      (format-wish "senddatastring [after idle {callback ~A}]" name)
      (let ((id (read-data))
            (blah (wish-after-ids *wish*)))
        (add-callback name
                      (lambda ()
                        (funcall fun)
                        (remhash id blah)
                        (remove-callback name)))
        id))))

(defun after-cancel (id)
 "cancels a call scheduled with AFTER or AFTER-IDLE by its id"
 (format-wish "after cancel {~a}" id)
 (let ((blah (wish-after-ids *wish*)))
   (remove-callback (gethash id blah))
   (remhash id blah)))

;; tool functions used by the objects

(defparameter *generate-name-lock* (make-lock))

(defun get-counter()
  "incremental counter to create unique numbers"
  (with-lock-held (*generate-name-lock*)
    (incf (wish-counter *wish*))))

(defun create-name (&optional (prefix nil))
  "create unique widget name, append unique number to 'n'"
  (format nil "~@[~a~]n~a" prefix (encode-base-52 (get-counter))))

(defun create-tag-name ()
  (create-name "tag"))

(defun create-path (master name)
  "create pathname from master widget <master> and widget name <name>"
  (let ((master-path (if (or (null master)
                             (eql master (root-toplevel)))
                         ""
                         (widget-path master))))
    (format nil "~a.~a" master-path name)))


#+nil(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    (setf *class-args* (append (remove-if (lambda (entry)
                                             (equal (car entry) class))
                                           *class-args*)))
    `(setf *class-args* (append (remove-if (lambda (entry)
                                             (equal (car entry) ',class))
                                           *class-args*) (list '(,class ,@args))))))


;;; the library implementation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cursors*
    (list
     "X_cursor" "arrow" "based_arrow_down" "based_arrow_up" "boat" "bogosity"
     "bottom_left_corner" "bottom_right_corner" "bottom_side" "bottom_tee"
     "box_spiral" "center_ptr" "circle" "clock" "coffee_mug" "cross"
     "cross_reverse" "crosshair" "diamond_cross" "dot" "dotbox" "double_arrow"
     "draft_large" "draft_small" "draped_box" "exchange" "fleur" "gobbler"
     "gumby" "hand1" "hand2" "heart" "icon" "iron_cross" "left_ptr" "left_side"
     "left_tee" "leftbutton" "ll_angle" "lr_angle" "man" "middlebutton" "mouse"
     "pencil" "pirate" "plus" "question_arrow" "right_ptr" "right_side"
     "right_tee" "rightbutton" "rtl_logo" "sailboat" "sb_down_arrow"
     "sb_h_double_arrow" "sb_left_arrow" "sb_right_arrow" "sb_up_arrow"
     "sb_v_double_arrow" "shuttle" "sizing" "spider" "spraycan" "star"
     "target" "tcross" "top_left_arrow" "top_left_corner" "top_right_corner"
     "top_side" "top_tee" "trek" "ul_angle" "umbrella" "ur_angle" "watch" "xterm"))

  (defgeneric find-cursor (object))

  (defmethod find-cursor ((object symbol))
    (if (eq object :x-cursor)
        "X_cursor"
        (let* ((old-pointer-name (symbol-name object))
               (new-pointername  (string-downcase (cl-ppcre:regex-replace-all "-"
                                                                              old-pointer-name
                                                                              "_"))))
          (find-cursor new-pointername))))

  (defmethod find-cursor ((object string))
    (find object *cursors* :test #'string=))

  (a:define-constant +standard-cursor+ (find-cursor :arrow)))

(defun configure-mouse-pointer (widget pointer-shape-name)
  (let ((cursor (find-cursor pointer-shape-name)))
    (assert cursor)
    (configure widget :cursor cursor)
    widget))

(defun bell ()
  (send-wish (format nil "bell")))

(defun destroy (widget)
  (when (slot-boundp widget 'widget-path)
    (format-wish "destroy ~a" (widget-path widget))
    (unless (eql widget
                 (root-toplevel))
      (slot-makunbound widget 'widget-path))))

(defun clipboard-clear ()
  (send-wish "clipboard clear"))

(defun clipboard-get ()
  (with-read-data ()
    (format-wish "senddatastring [clipboard get]")))

(defun clipboard-append (txt)
  (format-wish "clipboard append {~a}" txt))

(defun tk-number (number)
  "convert number to integer/single float"
  (cond
    ((integerp number)
     number)
    ((typep number 'single-float)
     number)
    ((typep number 'double-float)
     (coerce number 'single-float))
    ((typep number 'rational)
     (coerce number 'single-float))
    ((null number)
     nil)
    (t
     (error "~s is not a one of integer, float or rational." number))))

(defun update-idle-tasks ()
  (send-wish "update idletasks"))

(defun wait-complete-redraw ()
  (update-idle-tasks))

(defclass scrolled-frame (frame)
  ((frame-class
    :accessor frame-class
    :initform 'frame
    :initarg  :frame-class)
   (canvas
    :accessor canvas
    :initform nil
    :initarg :canvas)
   (inner
    :accessor interior)
   (hscroll
    :accessor hscroll)
   (vscroll
    :accessor vscroll)))

(defmethod reset-scroll ((sf scrolled-frame))
  (format-wish "after idle {resetscroll ~a}" (widget-path (canvas sf))))

(defmethod scroll-to-top ((sf scrolled-frame))
  (format-wish "~a yview moveto 0" (widget-path (canvas sf))))

;  (flush-wish))

(defmethod initialize-instance :after ((sf scrolled-frame) &key background)
  (let* ((canvas (make-instance 'canvas :master sf :background background))
         (f (if background
                (make-instance (frame-class sf) :master canvas :background background)
                (make-instance (frame-class sf) :master canvas))))
    (setf (canvas sf) canvas)
    (setf (interior sf) f) ;; (make-instance 'frame :master f :background background))
    (setf (hscroll sf) (make-instance 'scrollbar :master sf :orientation "horizontal"))
    (setf (vscroll sf) (make-instance 'scrollbar :master sf :orientation "vertical"))
    (grid canvas 0 0 :sticky "news")
    (grid (hscroll sf) 1 0 :sticky "we")
    (grid (vscroll sf) 0 1 :sticky "ns")
    (grid-columnconfigure sf 0 "weight" 1)
    (grid-columnconfigure sf 1 "weight" 0)
    (grid-rowconfigure sf 0 "weight" 1)
    (grid-rowconfigure sf 1 "weight" 0)
    ;; TODO: use tclize
    (format-wish
     "
~a configure -xscrollcommand [list ~a set] -yscrollcommand [list ~a set]
~a configure -command [list ~a xview]
~a configure -command [list ~a yview]
~a create window 10 10 -window ~a -anchor nw -tags f

after idle [list resetscroll ~a]

bind ~a <Configure> [list resetscroll ~a]

"
     (widget-path canvas) (widget-path (hscroll sf)) (widget-path (vscroll sf))
     (widget-path (hscroll sf)) (widget-path canvas)
     (widget-path (vscroll sf)) (widget-path canvas)
     (widget-path canvas) (widget-path f)
     (widget-path canvas)
     (widget-path f) (widget-path canvas))))

#+nil
(defclass scrolled-frame (frame)
  ((inner :accessor interior)
   (displayframe :accessor scrolled-frame-display)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)))

#+nil
(defmethod initialize-instance :after ((sf scrolled-frame) &key background)
  (let ((f (make-instance 'frame :master sf :background background)))
    (setf (scrolled-frame-display sf) f)
    (setf (interior sf) (make-instance 'frame :master f :background background))
    (setf (hscroll sf) (make-instance 'scrollbar :master sf :orientation "horizontal"))
    (setf (vscroll sf) (make-instance 'scrollbar :master sf :orientation "vertical"))
    (grid f 0 0 :sticky "news")
    (grid (hscroll sf) 1 0 :sticky "we")
    (grid (vscroll sf) 0 1 :sticky "ns")
    (grid-columnconfigure sf 0 "weight" 1)
    (grid-columnconfigure sf 1 "weight" 0)
    (grid-rowconfigure sf 0 "weight" 1)
    (grid-rowconfigure sf 1 "weight" 0)

    (place (interior sf) 0 0)
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (hscroll sf))))
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (vscroll sf))))
    (send-wish (format nil "~a configure -command ~axv" (widget-path (hscroll sf)) (name sf)))
    (send-wish (format nil "~a configure -command ~ayv" (widget-path (vscroll sf)) (name sf)))
    (send-wish (format nil "
proc ~axv {{com moveto} {val 0} {unit 0}} {

set x [winfo x ~a]
set y [winfo y ~a]
set wx [winfo width ~a]
set w [winfo width ~a]

if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wx-$w)/$wx]} {set val  [expr 1.0*($wx-$w)/$wx]}
if {$wx<$w} { set val 0 }
place ~a -x [expr -($val * $wx)] -y $y
set x [winfo x ~a]
~a set [expr -1.0*$x/$wx] [expr 1.0*($w-$x)/$wx]
}
proc ~ayv {{com moveto} {val 0} {unit 0}} {
set x [winfo x ~a]
set y [winfo y ~a]
set wy [winfo height ~a]
set h [winfo height ~a]
if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wy-$h)/$wy]} {set val  [expr 1.0*($wy-$h)/$wy]}
if {$wy<$h} { set val 0 }
place ~a -x $x -y [expr -($val * $wy)]
set y [winfo y ~a]
~a set [expr -1.0*$y/$wy] [expr 1.0*($h-$y)/$wy]
}

" (name sf)
  (widget-path (interior sf)) (widget-path (interior sf)) (widget-path (interior sf))
  (widget-path f)  (widget-path (interior sf))  (widget-path (interior sf))
  (widget-path (hscroll sf))
  (name sf)   (widget-path (interior sf))  (widget-path (interior sf))
  (widget-path (interior sf))  (widget-path f)  (widget-path (interior sf))
  (widget-path (interior sf))    (widget-path (vscroll sf))
  ))
    (format-wish "bind ~a <Configure> {nodguidebug \"~a configure\";~axv configure;~ayv configure}" (widget-path sf) (name sf)(name sf)(name sf))
    (format-wish "bind ~a <Configure> {nodguidebug \"~a iconfigure\";~axv configure;~ayv configure}" (widget-path (interior sf)) (name sf)(name sf)(name sf))
    ))

(defun dictionary-plist (string)
  "return a plist representing the TCL dictionary"
  ;; crude but rather effective
  (do* ((*package* (find-package :keyword))
        (length (length string))
        (plist nil)
        (key (position #\- string)
             (position #\- string :start (1+ val)))
        (val (position #\Space string :start (if key (1+ key) length))
             (position #\Space string :start (if key (1+ key) length))))
       ((null val)
        (reverse plist))
    (push (read-from-string string t t :start (1+ key)) plist)
    (push (read-from-string string t t :start (1+ val)) plist)))

;;;; generic methods on widgets

;;; pack method for widget arrangement in container

(defgeneric pack (w &key side fill expand after before padx pady ipadx ipady anchor))

(defun tclize-pad (padding-command padding-value)
  (let ((pad (if (listp padding-value)
                 (progn
                   (assert (= (length padding-value) 2))
                   (format nil "~{~a ~}" (sanitize padding-value)))
                  padding-value)))
     `(,padding-command { ,pad } " ")))

(defmethod pack ((w widget) &key (side :top) (fill :none) expand after before padx pady ipadx ipady anchor)
  (cond ((stringp side)
         (warn "Using a string for the :SIDE parameter is deprecated."))
        ((stringp fill)
         (warn "Using a string for the :FILL parameter is deprecated.")))
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(pack ,(widget-path w) " "
                                -side ,(keyword->tcl side :downcase t)  " "
                                ,(empty-string-if-nil fill
                                   `(-fill ,(keyword->tcl fill
                                                          :downcase t))) " "
                                ,(empty-string-if-nil expand
                                   `(-expand 1 " "))
                                ,(empty-string-if-nil after
                                   `(-after ,(widget-path after) " " ))
                                ,(empty-string-if-nil before
                                   `(-before ,(widget-path before) " "))
                                ,(empty-string-if-nil padx
                                   (tclize-pad '-padx padx))
                                ,(empty-string-if-nil pady
                                   (tclize-pad '-pady pady))
                                ,(empty-string-if-nil ipadx
                                   `(-ipadx ,ipadx " "))
                                ,(empty-string-if-nil ipady
                                   `(-ipady ,ipady " "))
                                ,(empty-string-if-nil anchor
                                  `(-anchor ,(keyword->tcl anchor
                                                           :downcase t)))))))
  w)

(defmethod pack ((list list) &rest rest)
  (mapcar #'(lambda (w)
              (apply #'pack w rest))
          list))

(defgeneric pack-propagate (widget flag))

(defmethod pack-propagate ((w widget) flag)
  (format-wish "pack propagate ~A ~A"
               (widget-path w)
               ;; TODO: use lisp-bool->tcl
               (if flag "true" "false"))
  w)

(defgeneric pack-forget (widget))

(defmethod pack-forget ((w widget))
  (format-wish "pack forget ~A" (widget-path w))
  w)

(defgeneric pack-forget-all (widget))

(defmethod pack-forget-all ((w widget))
  "removes all widgets packed into w"
  (format-wish "foreach slave [pack slaves ~A] { pack forget $slave}" (widget-path w))
  w)

;;; place manager

(defgeneric place (widget x y &key anchor bordermode width height in relheight relwidth relx rely))

(defmethod place (widget x y &key anchor width bordermode height in relheight relwidth relx rely)
  (format-wish "place ~A -x ~A -y ~A~@[ -anchor {~a}~]~@[ -width ~a~]~@[ -height ~a~]~@[ -bordermode {~a}~]~@[ -in {~a}~]~@[ -relheight ~a~]~@[ -relwidth ~a~]~@[ -relx ~a~]~@[ -rely ~a~]" (widget-path widget)
               (tk-number x)
               (tk-number y)
               anchor
               (tk-number width)
               (tk-number height)
               bordermode
               in
               (tk-number relheight)
               (tk-number relwidth)
               (tk-number relx)
               (tk-number rely))
  widget)

(defgeneric place-forget (widget))

(defmethod place-forget ((w widget))
  (format-wish "place forget ~A" (widget-path w))
  w)

;;; grid manager

(defgeneric grid (widget row column
                  &key columnspan ipadx ipady padx pady rowspan sticky))

(defmethod grid ((w widget) row column
                 &key columnspan ipadx ipady padx pady rowspan sticky)
  (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(grid ,(widget-path w) " "
                                  -row        ,(tk-number row) " "
                                  -column     ,(tk-number column) " "
                                  ,(empty-string-if-nil columnspan
                                    `(-columnspan ,(tk-number columnspan) " "))
                                  ,(empty-string-if-nil rowspan
                                     `(-rowspan ,(tk-number rowspan) " "))
                                  ,(empty-string-if-nil sticky
                                     `(-sticky ,(keyword->tcl sticky :downcase t)
                                               " "))
                                  ,(empty-string-if-nil padx
                                     (tclize-pad '-padx padx))
                                  ,(empty-string-if-nil pady
                                     (tclize-pad '-pady pady))
                                  ,(empty-string-if-nil ipadx
                                     `(-ipadx ,ipadx " "))
                                  ,(empty-string-if-nil ipady
                                     `(-ipady ,ipady " "))))))
  w)

(defgeneric grid-implicit (widgets &key columnspan ipadx ipady padx pady rowspan sticky))

(defmethod grid-implicit ((widgets list) &key columnspan ipadx ipady padx pady rowspan sticky)
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(grid ,(format nil
                                         "~{~a ~}"
                                         (mapcar #'widget-path widgets))
                                  ,(empty-string-if-nil columnspan
                                    `(-columnspan ,(tk-number columnspan) " "))
                                  ,(empty-string-if-nil rowspan
                                     `(-rowspan ,(tk-number rowspan) " "))
                                  ,(empty-string-if-nil sticky
                                     `(-sticky ,(keyword->tcl sticky :downcase t)
                                               " "))
                                  ,(empty-string-if-nil padx
                                     (tclize-pad '-padx padx))
                                  ,(empty-string-if-nil pady
                                     (tclize-pad '-pady pady))
                                  ,(empty-string-if-nil ipadx
                                     `(-ipadx ,ipadx " "))
                                  ,(empty-string-if-nil ipady
                                     `(-ipady ,ipady " "))))))
  widgets)

(defgeneric grid-columnconfigure (widget column option value))

(defmethod grid-columnconfigure (widget column option value)
  ;; TODO use tclize
  (format-wish "grid columnconfigure {~a} {~a} {-~(~a~)} {~a}"
               (down widget) (down column) option value)
  widget)

(defgeneric grid-rowconfigure (widget row option value))

(defmethod grid-rowconfigure (widget row option value)
  (format-wish "grid rowconfigure {~a} {~a} {-~(~a~)} {~a}"
               (down widget) (down row) option value)
  widget)

(defgeneric grid-configure (widget o v))

(defmethod grid-configure (widget option value)
  (format-wish "grid configure {~a} {-~(~a~)} {~a}"
               (down widget) option value)
  widget)

(defgeneric grid-forget (widget))

(defmethod grid-forget ((w widget))
  (format-wish "grid forget ~A" (widget-path w))
  w)

;;; configure a widget parameter

(defgeneric configure (widget option value &rest others))

(defmethod configure (widget option value &rest others)
  (format-wish "~a configure~{ {-~(~a~)} {~a}~}"
               (widget-path widget)
               (mapcar #'down (list* option value others)))
  widget)

(defgeneric tag-bind (object tag event fun &key exclusive))

(defgeneric tag-configure (object tag-name option value &rest others))

(defgeneric tag-raise (object tag-name &optional on-top-of-tag))

(defgeneric tag-lower (object tag-name &optional on-top-of-tag))

(defgeneric see (object pos)
  (:documentation "Makes sure the widget is visible"))

;;; for tkobjects, the name of the widget is taken
(defmethod configure (widget option (value tkobject) &rest others)
  (format-wish "~A configure {-~(~A~)} {~A} ~{ {-~(~a~)} {~(~a~)}~}"
               (widget-path widget) option (widget-path value) others)
  widget)

(defgeneric cget (widget option &key &allow-other-keys))

(defmethod cget ((widget widget) option &key &allow-other-keys)
  (with-read-data ()
    (format-wish "senddatastring [~a cget {-~(~a~)}]" (widget-path widget) option)))

;(defun background (widget)
;  (cget widget :background))

#-:gcl
;(defun (setf background) (val widget)
;  (configure widget :background val))

#|
(defmacro defoption (option)
  `(progn
     (defun ,option (widget)
       (cget widget "asdf"))
     (export ,option)))

(defoption fill)
|#


;;; font functions
;; use {~/nodgui::pprint-down/} on the font name to match itemconfigure

;;(defun font-actual ...)

(defmacro make-font-constant (name &key (documentation ""))
  (let ((constant-name (string-upcase (wrap-with (camel-case->snail-case name
                                                                         :make-downcase nil)
                                                 "+"))))
    `(a:define-constant   ,(alexandria:format-symbol t constant-name)
         ,name
         :test          #'string=
         :documentation ,documentation)))

(defmacro make-font-constants (names-docs)
  `(progn
     ,@(loop for (name . doc) in names-docs collect
            `(make-font-constant ,name :documentation ,doc))))

(make-font-constants (("TkDefaultFont"      . "The default font")
                      ("TkTextFont"         . "Text of widgets")
                      ("TkFixedFont"        . "Monospaced font")
                      ("TkMenuFont"         . "")
                      ("TkHeadingFont"      . "")
                      ("TkCaptionFont"      . "")
                      ("TkSmallCaptionFont" . "")
                      ("TkIconFont"         . "")
                      ("TkTooltipFont"      . "")))

(defun font-configure (name &key family size weight slant underline overstrike)
  (format-wish "font configure {~a}~@[ -family {~a}~]~@[ -size {~a}~]~@[ -weight {~(~a~)}~]~@[ -slant {~(~a~)}~]~@[ -underline {~a}~]~@[ -overstrike {~a}~]"
               (down name) family size weight slant underline overstrike))

(defun font-create (name &key family size weight slant underline overstrike)
  (with-read-data ()
    (format-wish "senddatastring [font create {~a}~@[ -family {~a}~]~@[ -size {~a}~]~@[ -weight {~(~a~)}~]~@[ -slant {~(~a~)}~]~@[ -underline {~a}~]~@[ -overstrike {~a}~]]"
                 (down name) family size weight slant underline overstrike)))

(defun font-delete (&rest names)
  (format-wish "font delete~{ {~a}~}" (down names)))

(defun font-measure (font-spec text &key (display-of nil))
  (with-read-data ()
    (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(senddata [ font measure
                                      ,(empty-string-if-nil font-spec
                                                            `({+ ,font-spec } " "))
                                      ,(empty-string-if-nil display-of
                                                            `(-displayof {+ ,(widget-path display-of) }
                                                                         " "))
                                      ,(empty-string-if-nil text
                                                            `({+ ,text }))
                                      ]))))))

(defun font-metrics (font)
  (with-read-data ()
    (format-wish "sendpropertylist [font metrics {~a}]" (down font))))

(defun font-actual (font-spec &key (display-of nil))
  (with-read-data (nil)
    (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(senddatastrings [ font actual
                                        {+ ,font-spec } " "
                                      ,(empty-string-if-nil display-of
                                                            `(-displayof {+ ,(widget-path display-of) }
                                                                         " "))
                                      ])))
      (let ((raw (read-data)))
        (a:flatten (loop for (key value) on raw by 'cddr
                         collect
                         (list (a:make-keyword (string-upcase (subseq key 1)))
                               value)))))))

;;(defun font-names ...)

(defun font-families (&optional (display-of nil))
  (with-read-data ()
    (format-wish (tclize `(senddatastrings [ font families " "
                                           ,(empty-string-if-nil display-of
                                                                 `(-displayof  {+ ,(widget-path display-of) }
                                                                               " "))
                                           ])))))

(defun font-chooser-show (&key (parent (root-toplevel)) (title "Choose a font"))
  (with-read-data ()
    (format-wish (tclize `(tk fontchooser configure
                              -parent {+ ,(widget-path parent) }
                            -title  \"+ ,title \"
                            -command senddatastring)))
    (format-wish (tclize `(tk fontchooser show)))))

(defun font-chooser-hide ()
  (format-wish (tclize `(tk fontchooser hide))))

(defgeneric print-dialog (object)
  (:documentation "Open a dialog window to print the widget on a physical printer, if an error occurred (e.g. no printer is available or installed), `nil' is returned."))

(defun %print-dialog (widget)
  (tcl-bool->lisp
   (with-read-data ()
       (format-wish (tclize `(senddata [print_dialog ,(widget-path widget) ]))))))

;;; misc functions

(defvar *default-theme* "default"
  "The default color theme for nodgui. Available ones are: default, alt, aqua (Mac OS only), clam, classic, vista (Windows only) and yaru.
  Yaru is a more modern theme coming from the ttkthemes collection.")

(defun default-theme ()
  *default-theme*)

(defparameter *themes-directory* (asdf:system-relative-pathname :nodgui "themes"
                                                                :type :directory)
  "A directory where nodgui looks for themes. By default, it looks under the themes/ directory where nodgui is installed.

 Each theme must be placed in their own directory as a subdirectory of the aforementioned variable, the name of the directory must be the name of the theme; moreover the name of the TCL file that specify the file must be named as the same of the theme with the extension \"tcl\" appended

For example the theme foo has to be: \"foo/foo.tcl\".

Provided these conditions are met using a new theme should be as simple as type:

(nodgui:use-theme \"foo\") ; or (:foo, :FOO, 'foo, symbols will be downcased)

It is also possible to load a third-party .tcl theme file with:

(nodgui: eval-tcl-file)

and then calling use-theme.

The function THEME-NAMES will return both the default and the custom themes.")

(defgeneric eval-tcl-file (object))

(defmethod eval-tcl-file ((object string))
  "This function will feed the TCL interpreter with the contents of the file `file-path'.
   Please, as this function will load  and execute a script, ensure to
   load files only from trusted sources otherwise severe security problem may arise."
  (format-wish "source {~a}" object))

(defmethod eval-tcl-file ((object pathname))
  "This function will feed the TCL interpreter with the contents of the file `file-path'.
   Please, as this function will load  and execute a script, ensure to
   load files only from trusted sources otherwise severe security problem may arise."
  (eval-tcl-file (namestring object)))

(defun send-use-theme (name)
  (format-wish "ttk::style theme use {~a}" name))

(defun build-theme-filename (name)
  (strcat name ".tcl"))

(defun build-theme-pathfile (name)
  (merge-pathnames (merge-pathnames (strcat name "/")
                                    *themes-directory*)
                   (build-theme-filename name)))

(defun use-theme (name)
  (let ((actual-name (down name)))
    (cond
      ((find actual-name (embedded-theme-names) :test #'string=)
       (send-use-theme actual-name))
      ((file-exists-p (build-theme-pathfile actual-name))
       (eval-tcl-file (build-theme-pathfile actual-name))
       (send-use-theme actual-name))
      (t
       (error "Unable to find the theme ~a in the the directory ~a (and it is not an embedded theme)"
              name
              *themes-directory*)))))

(defun embedded-theme-names ()
  (with-read-data ()
    (send-wish "senddatastrings [ttk::style theme names]")))

(defun current-time-milliseconds ()
  (with-read-data ()
    (send-wish "senddata [clock milliseconds]")))

(defun glob (root-directory pattern &key (type nil))
  "`type' must be a keyword that contains only characters in set
   (b c d f / p s r w x)"
  (warn "Deprecated: use match-path.")
  (let ((chars (remove-duplicates (coerce (string-downcase (symbol-name type))
                                          'list)
                                  :test #'char=)))
    (assert (or (null type)
                (every (lambda (a) (member a
                                           '(#\b #\c #\d #\f #\/ #\p #\s #\r #\w #\x)
                                           :test #'char=))
                       chars)))
    ;;(with-read-data ()
    (let* ((*suppress-newline-for-tcl-statements* t)
           (globbing-command (with-no-escape-tilde
                               (tclize `(glob
                                         -directory \"+ ,root-directory \" " "
                                         ,(empty-string-if-nil type
                                                               `(-type { ,@chars })) " "
                                                               -- \"+ ,pattern \")
                                       :sanitize nil))))
      (let ((has-error-p (tcl-bool->lisp
                          (with-read-data ()
                            (with-no-escape-tilde
                              (send-wish (tclize `(senddata [catch { ,globbing-command }])
                                                 :sanitize nil)))))))
        (when (not has-error-p)
          (sort (with-read-data ()
                  (with-no-escape-tilde
                    (send-wish (tclize `(senddatastrings [ ,globbing-command ])
                                       :sanitize nil))))
                #'string<))))))

(defun theme-names ()
  (flet ((theme-name (dir-pathname)
           (let ((dir-namestring (namestring dir-pathname)))
               (path-last-element dir-namestring))))
    (let ((embedded (with-read-data ()
                    (send-wish "senddatastrings [ttk::style theme names]")))
        (custom   (remove-if-not (lambda (dir)
                                   (let* ((theme-name     (theme-name dir))
                                          (theme-filename (build-theme-filename theme-name)))
                                     (file-exists-p (merge-pathnames dir theme-filename))))
                                 (subdirectories *themes-directory*))))
    (remove-duplicates (append embedded
                               (mapcar (lambda (a) (theme-name a)) custom))
                       :test #'string=))))

(defgeneric focus (object))

(defmethod focus ((object widget))
  (format-wish "focus ~a" (widget-path object))
  object)

(defmethod focus ((object null))
  (with-read-data ()
    (format-wish "senddatastring [focus]")))

(defun widget-with-focus ()
  (widget-path->widget (focus nil)))

(defun force-focus (widget)
  (format-wish "focus -force ~a" (widget-path widget))
  widget)

(defun bind-tag-set-focus-next (focus-widget widget-to-pass-focus)
  (format-wish "bind ~a <Tab> { focus ~a; break }"
               (widget-path focus-widget)
               (widget-path widget-to-pass-focus)))

(defun set-focus-next (widget next)
  (warn "Please, do not use `set-focus-next', use `bind-tag-set-focus-next' instead")
  (bind-tag-set-focus-next widget next))

(defun cm (tree widget-path)
  (cond
   ((eq tree :separator)
    (format-wish "{~A} add separator" widget-path))
   ((listp (second tree))
    (let ((newpath (format nil "~A.~A" widget-path (create-name))))
      (when (and (equal widget-path ".menubar")
                 (or (equal (first tree) "Help")
                     (equal (first tree) "help")
                     (equal (first tree) "Hilfe")))
        (setf newpath ".menubar.help"))
      (format-wish "menu {~A} -tearoff 0" newpath)
      (format-wish "~a add cascade -label {~a} -menu {~a}"
                   widget-path
                   (first tree)
                   newpath)
      (dolist (entry (second tree))
        (cm entry newpath))))
   (t
    (let* ((name (create-name)))
      (add-callback name (second tree))
      (format-wish "{~A} add command -label {~A} -command {puts -nonewline  {(\"~A\")};flush $server}"
                   widget-path (first tree) name)))))

(defun create-menu2 (menutree)
  (send-wish "menu .menubar -tearoff 0 -type menubar")
  (dolist (e menutree)
    (cm e ".menubar"))
  (send-wish ". configure -menu .menubar"))

(defstruct modal-toplevel
  (lock    (make-lock))
  (close-condition nil)
  (root-widget nil)
  (results     nil)
  (parent-mainloop-coordination-data nil))

(defmethod widget-path ((object modal-toplevel))
  (widget-path (modal-toplevel-root-widget object)))

(defgeneric exit-from-modal-toplevel (object))

(defmethod exit-from-modal-toplevel ((object modal-toplevel))
  (with-lock-held ((modal-toplevel-lock object))
    (grab-release (modal-toplevel-root-widget object))
    (withdraw (modal-toplevel-root-widget object))
    (flush-wish)
    (break-mainloop)))

(defmacro with-modal-toplevel ((toplevel-struct &rest toplevel-initargs) &body body)
  (a:with-gensyms (toplevel wish-process modal-widget-thread parent-mainloop-coordination-data)
    `(let* ((,wish-process *wish*)
            (,toplevel-struct (make-modal-toplevel :parent-mainloop-coordination-data
                                                   (wish-main-loop-coordination-data ,wish-process)))
            (,parent-mainloop-coordination-data (modal-toplevel-parent-mainloop-coordination-data ,toplevel-struct))
            (,modal-widget-thread (make-thread
                                   (lambda ()
                                     (let* ((*wish*    ,wish-process)
                                            (,toplevel (make-instance 'toplevel
                                                                      ,@toplevel-initargs)))
                                       (setf (modal-toplevel-root-widget ,toplevel-struct)
                                             ,toplevel)
                                       (wait-complete-redraw)
                                       (on-close ,toplevel
                                                 (lambda ()
                                                   (exit-from-modal-toplevel ,toplevel-struct)))
                                       (grab ,toplevel)
                                       (with-lock-held ((mainloop-coordination-pause-lock ,parent-mainloop-coordination-data))
                                         (setf (mainloop-coordination-pause ,parent-mainloop-coordination-data)
                                               t))
                                       (when (not *event-popped-from-mainloop*)
                                         ;; ensure  at  least  an  event
                                         ;; (albeit  ignored) is  pushed
                                         ;; in the event queue to unlock
                                         ;; the  parent   mainloop  that
                                         ;; could  be stuck  waiting for
                                         ;; the queue  (see line  641 of
                                         ;; "wish-communication.lisp")
                                         (push-enqueued-event :ignored)
                                         ;; this  code ensure  the event
                                         ;; queue   is    empty   before
                                         ;; starting  a  new loop,  this
                                         ;; procedure   is   needed   to
                                         ;; ensure  the  mainloops  does
                                         ;; not   steals   events   each
                                         ;; others
                                         (with-lock-held (*popped-mainloop-event-lock*)
                                           (loop while (check-enqueued-event)
                                                 do
                                                    (condition-wait *popped-mainloop-event-condvar*
                                                                    *popped-mainloop-event-lock*))))
                                       (push-mainloop-thread)
                                       (start-main-loop :thread-special-bindings
                                                        ,(getf toplevel-initargs
                                                               :main-loop-thread-special-bindings))
                                       (setf (modal-toplevel-results ,toplevel-struct)
                                             (progn ,@body))
                                       (flush-wish)
                                       (join-thread (wish-main-loop-thread *wish*))
                                       (pop-mainloop-coordination-data)
                                       (pop-mainloop-thread)
                                       (with-lock-held ((mainloop-coordination-pause-lock ,parent-mainloop-coordination-data))
                                         (setf (mainloop-coordination-pause ,parent-mainloop-coordination-data) nil)
                                         (condition-notify (mainloop-coordination-pause-condvar ,parent-mainloop-coordination-data)))
                                       (modal-toplevel-results ,toplevel-struct)))
                                   :initial-bindings
                                   (append (list (cons '*event-popped-from-mainloop*
                                                       *event-popped-from-mainloop*))
                                           ,(or (getf toplevel-initargs
                                                      :modal-toplevel-thread-special-bindings)
                                                (getf toplevel-initargs
                                                      :main-loop-thread-special-bindings))))))
       (join-thread ,modal-widget-thread)
       (modal-toplevel-results ,toplevel-struct))))

(defun exit-from-toplevel (toplevel)
  (grab-release toplevel)
  (withdraw toplevel)
  (flush-wish))

(defmacro with-toplevel ((toplevel &rest toplevel-initargs) &body body)
  `(let* ((,toplevel (make-instance 'toplevel ,@toplevel-initargs)))
     (wait-complete-redraw)
     (grab ,toplevel)
     (on-close ,toplevel (lambda () (exit-from-toplevel ,toplevel)))
     (progn ,@body)))


(defmacro with-hourglass (widgets &rest body)
  `(unwind-protect
        (progn
          ,@(mapcar (lambda (w)
                      `(when ,w
                         (configure ,w :cursor :watch)))
                    widgets)
          (flush-wish)
          ,@body)
     ,@(mapcar (lambda (w)
                 `(when ,w
                    (configure ,w :cursor "")))
               widgets)))
