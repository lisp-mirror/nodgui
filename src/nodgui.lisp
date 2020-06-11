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
  (when *debug-tk*
    (format *trace-output* "add-callback (~A ~A)~%" sym fun))
  (setf (gethash sym (wish-callbacks *wish*)) fun))

(defun remove-callback (sym)
  (when *debug-tk*
    (format *trace-output* "remove-callback (~A)~%" sym))
  (setf (gethash sym (wish-callbacks *wish*)) nil))

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
 (let ((name (format nil "after~a" (incf (wish-after-counter *wish*)))))
   (format-wish (tcl-str (senddatastring [after {~a} {callback ~A}])) time name)
   ;(senddatastring "[after ~a {callback ~A}]" time name)
   (let ((id (read-data))
         (blah (wish-after-ids *wish*)))
     (setf (gethash id blah) name)
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-idle (fun)
 "call fun when tk becomes idle, returns the after event id, which
can be passed to AFTER-CANCEL"
 (let ((name (format nil "afteridle~a" (incf (wish-after-counter *wish*)))))
   (format-wish "senddatastring [after idle {callback ~A}]" name)
   (let ((id (read-data))
         (blah (wish-after-ids *wish*)))
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-cancel (id)
 "cancels a call scheduled with AFTER or AFTER-IDLE by its id"
 (format-wish "after cancel {~a}" id)
 (let ((blah (wish-after-ids *wish*)))
   (remove-callback (gethash id blah))
   (remhash id blah)))

;; tool functions used by the objects

(defun get-counter()
  "incremental counter to create unique numbers"
  (incf (wish-counter *wish*)))

#+nil(defun create-name ()
  "create unique widget name, append unique number to 'w'"
  (format nil "w~A" (get-counter)))

(defun create-name ()
  "create unique widget name, append unique number to 'n'"
  (format nil "n~a" (encode-base-52 (get-counter))))

(defun create-path (master name)
  "create pathname from master widget <master> and widget name <name>"
  (let ((master-path (if (or (null master)
                             (eql master *tk*))
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

(defun bell ()
  (send-wish (format nil "bell")))

(defun destroy (widget)
  (when (slot-boundp widget 'widget-path)
    (format-wish "destroy ~a" (widget-path widget))
    (unless (eql widget *tk*)
      (slot-makunbound widget 'widget-path))))

(defun clipboard-clear ()
  (send-wish "clipboard clear"))

(defun clipboard-get ()
  (format-wish "senddatastring [clipboard get]")
  (read-data))

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

(defstruct event
  x
  y
  char-code
  keycode
  char
  width
  height
  root-x
  root-y
  mouse-button
  unicode-char
  others)

(defun construct-tk-event (properties)
  "create an event structure from a list of values as read from tk"
  (make-event
   :x            (first properties)  ; 0
   :y            (second properties) ; 1
   :char-code    (third properties)  ; 2
   :keycode      (fourth properties) ; 3
   :char         (fifth properties)  ; 4
   :width        (sixth properties)  ; 5
   :height       (seventh properties); 6
   :root-x       (eighth properties) ; 7
   :root-y       (ninth properties)  ; 8
   :mouse-button (tenth properties)  ; 9
   :unicode-char (elt   properties 10)
   :others       (if (string= (elt properties 11)
                              "")
                     nil
                     (elt properties 11))))

(defgeneric bind (w event fun &key append exclusive))

(defmethod bind ((w widget) event fun &key append exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  ~a {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A ~:[~;;break~]}"
                 (widget-path w) event append name exclusive)
    w))

(defmethod bind (s event fun &key append exclusive)
  "bind fun to event within context indicated by string ie. 'all' or 'Button'"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  {~a} {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A ~:[~;;break~]}"
                 s event append name exclusive)))

;;; window menu bar

(defclass menubar (widget) ())

(defun make-menubar(&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))

;(defmethod create ((mb menubar))
(defmethod initialize-instance :after ((mb menubar) &key)
  (format-wish "menu ~a -tearoff 0 -type menubar" (widget-path mb))
  (format-wish "~a configure -menu ~a" (if (master mb)
                                           (widget-path (master mb))
                                           (widget-path *tk*))
               (widget-path mb)))

;;; method to pop up a menue at the root window coordinates x and y

(defgeneric popup (menu x y))

(defmethod popup ((menu menu) x y)
  (format-wish "tk_popup ~A ~A ~A" (widget-path menu)
               (tk-number x) (tk-number y))
  menu)

(defgeneric menu-delete (menu index &optional end))

(defmethod menu-delete ((menu menu) index &optional (end :end))
  (format-wish "~A delete {~a} ~@[ {~(~A~)}~]" (widget-path menu) index (down end))
  menu)

(defgeneric sash-coord (window index))

(defmethod sash-coord ((pw paned-window) index)
  (format-wish "senddata \"([~a sashpos {~a}])\"" (widget-path pw) index)
  (read-data))

(defgeneric sash-place (window index pos))

(defmethod sash-place ((pw paned-window) index pos)
  (format-wish "~a sashpos {~a} {~a}" (widget-path pw) index pos))

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

(defmethod pack ((w widget) &key (side :top) (fill :none) expand after before padx pady ipadx ipady anchor)
  (cond ((stringp side)
         (warn "Using a string for the :SIDE parameter is deprecated."))
        ((stringp fill)
         (warn "Using a string for the :FILL parameter is deprecated.")))
  ;; TODO: use tclize
  (format-wish "pack ~A -side {~(~A~)} -fill {~(~A~)} ~@[~* -expand 1~]~@[ -after ~A~]~@[ -before ~A~]~@[ -padx {~a}~]~@[ -pady {~a}~]~@[ -ipadx {~a}~]~@[ -ipady {~a}~]~@[ -anchor {~(~A~)}~]"
               (widget-path w)
               side
               fill
               expand
               (and after (widget-path after))
               (and before (widget-path before))
               padx
               pady
               ipadx
               ipady
               anchor)
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

(defgeneric grid (widget r c &key columnspan ipadx ipady padx pady rowspan sticky))

(defmethod grid ((w widget) row column &key columnspan ipadx ipady padx pady rowspan sticky)
  ;; TODO use tclize
  (format-wish "grid {~a} -row {~a} -column {~a} ~@[ -columnspan {~a}~]~@[ -ipadx {~a}~]~
             ~@[ -ipady {~a}~]~@[ -padx {~a}~]~@[ -pady {~a}~]~@[ -rowspan {~a}~]~
             ~@[ -sticky {~(~a~)}~]"
               (widget-path w)
               row
               column
               columnspan
               ipadx
               ipady
               padx
               pady
               rowspan
               sticky)
  w)

(defgeneric grid-columnconfigure (widget c o v))

(defmethod grid-columnconfigure (widget column option value)
  ;; TODO use tclize
  (format-wish "grid columnconfigure {~a} {~a} {-~(~a~)} {~a}"
               (down widget) (down column) option value)
  widget)

(defgeneric grid-rowconfigure (widget r o v))

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
  (format-wish "~A configure~{ {-~(~a~)} {~a}~}"
               (widget-path widget)
               (mapcar #'down (list* option value others)))
  widget)

(defmethod configure ((item menuentry) option value &rest others)
  (let ((path (widget-path (master item))))
    (format-wish "~A entryconfigure [~A index {~A}]~{ {-~(~a~)} {~a}~}"
                 path
                 path
                 (text item)
                 (mapcar #'down (list* option value others))))
  item)

(defmethod configure ((item canvas-item) option value &rest others)
  (format-wish "~A itemconfigure ~A~{ {-~(~a~)} {~a}~}"
               (widget-path (canvas item)) (handle item)
               (mapcar #'down (list* option value others)))
  item)

(defgeneric tag-bind (object tag event fun &key exclusive))

(defgeneric tag-configure (txt tag option value &rest others))

(defmethod tag-configure ((c canvas) tag option value &rest others)
  (format-wish "~a itemconfigure {~a}~{ {-~(~a~)} {~a}~}" (widget-path c)
               (if (stringp tag)
                   tag
                   (format nil "~(~a~)" tag))
               (mapcar #'down (list* option value others)))
  c)

;;; for tkobjects, the name of the widget is taken
(defmethod configure (widget option (value tkobject) &rest others)
  (format-wish "~A configure {-~(~A~)} {~A} ~{ {-~(~a~)} {~(~a~)}~}"
               (widget-path widget) option (widget-path value) others)
  widget)

(defgeneric cget (widget option))

(defmethod cget ((widget widget) option)
  (format-wish "senddatastring [~a cget {-~(~a~)}]" (widget-path widget) option)
  (read-data))

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


(defmethod raise ((item canvas-item) &optional above)
  (itemraise (canvas item) (handle item) (and above (handle above))))

;;; font functions
;; use {~/nodgui::pprint-down/} on the font name to match itemconfigure

;;(defun font-actual ...)

(defmacro make-font-constant (name &key (documentation ""))
  (let ((constant-name (string-upcase (wrap-with (camel-case->snail-case name
                                                                         :make-downcase nil)
                                                 "+"))))
    `(define-constant   ,(alexandria:format-symbol t constant-name)
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
  (format-wish "senddatastring [font create {~a}~@[ -family {~a}~]~@[ -size {~a}~]~@[ -weight {~(~a~)}~]~@[ -slant {~(~a~)}~]~@[ -underline {~a}~]~@[ -overstrike {~a}~]]"
               (down name) family size weight slant underline overstrike)
   (read-data))

(defun font-delete (&rest names)
  (format-wish "font delete~{ {~a}~}" (down names)))

(defun font-measure (font-spec text &key (display-of nil))
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(senddata [ font measure
                                    ,(empty-string-if-nil font-spec
                                         `({+ ,font-spec } " "))
                                    ,(empty-string-if-nil display-of
                                         `(-displayof  {+ ,(widget-path display-of) }
                                           " "))
                                    ,(empty-string-if-nil text
                                         `({+ ,text }))
                                    ])))
    (read-data)))

(defun font-metrics (font)
  (format-wish "sendpropertylist [font metrics {~a}]" (down font))
  (read-data))

;;(defun font-names ...)

(defun font-families (&optional (display-of nil))
  (format-wish (tclize `(senddatastrings [ font families " "
                                         ,(empty-string-if-nil display-of
                                              `(-displayof  {+ ,(widget-path display-of) }
                                                " "))
                                         ])))
  (read-data))

;;; misc functions

(defun eval-tcl-file (file-path)
  "This function will feed the TCL interpreter with the contents of the file `file-path'.
   Please, as this function will load  and execute a script, ensure to
   load files only from trusted sources otherwise severe security problem may arise."
  (assert (stringp file-path))
  (format-wish "source {~a}" file-path))

(defun use-theme (name)
  (format-wish "ttk::style theme use {~a}" name))

(defun theme-names ()
  (send-wish "senddatastrings [ttk::style theme names]")
  (nodgui::read-data))

(defun focus (widget)
  (format-wish "focus ~a" (widget-path widget))
  widget)

(defun force-focus (widget)
  (format-wish "focus -force ~a" (widget-path widget))
  widget)

(defun set-focus-next (widget next)
  (format-wish "bind ~a <Tab> { focus ~a; break }" (widget-path widget) (widget-path next)))

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

;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the variable *exit-mainloop* is set

(defvar *exit-mainloop* nil)

(defvar *break-mainloop* nil)

(defun break-mainloop ()
  (setf *break-mainloop* t))

(defgeneric handle-output (key params))

(defmethod handle-output (key params)
  (declare (ignore key params)))

(defun process-one-event (event)
  (when event
    (when *debug-tk*
      (format *trace-output* "l:~s<=~%" event)
      (finish-output *trace-output*))
    (cond
     ((and (not (listp event))
           *trace-tk*)
      (princ event *trace-output*)
      (finish-output *trace-output*))
     ((not (listp event)) nil)
     ((eq (first event) :callback)
      (let ((params (rest event)))
        (callback (first params) (rest params))))
     ((eq (first event) :event)
      (let* ((params (rest event))
             (callback (first params))
             (evp (rest params))
             (event (construct-tk-event evp)))
        (callback callback (list event))))
      ((eq (first event) :keepalive)
       (format *trace-output* "Ping from wish: ~{~A~^~}~%" (rest event))
       (finish-output *trace-output*))
      ((eq (first event) :debug)
       (tcldebug (second event)))
     (t
      (handle-output
       (first event) (rest event))))))

(defun process-events (&optional (blockingp nil))
  "A function to temporarliy yield control to wish so that pending
events can be processed, useful in long loops or loops that depend on
tk input to terminate"
  (let (event)
    (loop
     while (setf event (read-event :blocking blockingp))
     do (with-atomic (process-one-event event)))))

(defparameter *inside-mainloop* ())

(defun main-iteration (&key (blocking t) (reentrant? nil))
  "The heart of the main loop.  Returns true as long as the main loop should continue."
  (let ((no-event (cons nil nil)))
    (labels ((proc-event ()
               (let ((event (read-event :blocking blocking
                                        :no-event-value no-event)))
                 (cond
                   ((null event)
                    (ignore-errors (close (wish-stream *wish*)))
                    (exit-wish)
                    nil)
                   ((eql event no-event)
                    t)
                   (t (with-atomic (process-one-event event))
                      (cond
                        (*break-mainloop* nil)
                        (*exit-mainloop*
                         (exit-wish)
                         nil)
                        (t t)))))))
      ;; For recursive calls to mainloop, we don't want to setup our ABORT and EXIT restarts.
      ;; They make things too complex.
      (if reentrant?
          (proc-event)
          (restart-case (proc-event)
            (abort ()
              :report "Abort handling Tk event"
              t)
            (exit ()
              :report "Exit Nodgui main loop"
              nil))))))

(defun mainloop (&key serve-event)
  (let ((reentrant? (member *wish* *inside-mainloop*))
        (*inside-mainloop* (adjoin *wish* *inside-mainloop*)))
    (cond
      (serve-event (install-input-handler))
      ((wish-input-handler *wish*)
       (let ((*exit-mainloop* nil)
             (*break-mainloop* nil))
         (loop until (or *break-mainloop* *exit-mainloop*)
               do (serve-event))))
      (t (let ((*exit-mainloop* nil)
               (*break-mainloop* nil))
           (if reentrant?
               (loop while (main-iteration :reentrant? t))
               (loop while (with-nodgui-handlers ()
                             (main-iteration)))))))))

;;; Event server

#-(or sbcl cmu)
(progn
  (defun install-input-handler ()
    (error "SERVE-EVENT is not implemented on this system"))
  (defun remove-input-handler ()
    nil)
  (defun serve-event ()
    (error "SERVE-EVENT is not implemented on this system")))

#+(or sbcl cmu)
(progn
  (defun add-fd-handler (fd direction function)
    #+sbcl (sb-sys:add-fd-handler fd direction function)
    #+cmu (system:add-fd-handler fd direction function))

  (defun remove-fd-handler (handler)
    #+sbcl (sb-sys:remove-fd-handler handler)
    #+cmu (system:remove-fd-handler handler))

  (defun serve-event ()
    #+sbcl (sb-sys:serve-event)
    #+cmu (system:serve-event))

  (defun fd-stream-fd (stream)
    #+sbcl (sb-sys:fd-stream-fd stream)
    #+cmu (system:fd-stream-fd stream))

  (defun make-input-handler (wish)
    "Return a SERVE-EVENT input handler."
    (let ((fd-stream (two-way-stream-input-stream (wish-stream wish))))
      (labels ((call-main ()
                 (with-nodgui-handlers ()
                   (handler-bind ((stream-error
                                   ;; If there was a stream error on the fd that
                                   ;; we're listening to, we need to remove the
                                   ;; input handler to avoid getting stuck in an
                                   ;; infinite loop of stream errors making
                                   ;; noise on the fd, causing us to try to read
                                   ;; from it, causing an error, which makes
                                   ;; noise on the fd...
                                   (lambda (e)
                                     (when (eql (stream-error-stream e) fd-stream)
                                       (return-from call-main nil)))))
                     (catch wish (main-iteration :blocking nil)))))
               (nodgui-input-handler (fd)
                 (declare (ignore fd))
                 (let ((*wish* wish)) ; use the wish we were given as an argument
                   (if (find wish *inside-mainloop*)
                       (call-main)
                       (let ((*exit-mainloop* nil)
                             (*break-mainloop* nil))
                         (unless (call-main)
                           (remove-input-handler)))))))
        #'nodgui-input-handler)))

  (defun install-input-handler ()
    (unless (wish-input-handler *wish*)
      (let ((fd (fd-stream-fd (two-way-stream-input-stream (wish-stream *wish*)))))
        (setf (wish-input-handler *wish*)
              (add-fd-handler fd :input (make-input-handler *wish*))
              *exit-mainloop* nil
              *break-mainloop* nil))))

  (defun remove-input-handler ()
    (remove-fd-handler (wish-input-handler *wish*))
    (setf (wish-input-handler *wish*) nil)))

;;

(defun filter-keys (desired-keys keyword-arguments)
  (loop for (key val) on keyword-arguments by #'cddr
        when (find key desired-keys) nconc (list key val)))

;;; wrapper macro - initializes everything, calls body and then mainloop

(defmacro with-nodgui ((&rest keys
                              &key (title "") (debug 2) stream serve-event &allow-other-keys)
                    &body body)
  "Create a new Nodgui connection, evaluate BODY, and enter the main loop.

  :DEBUG indicates the level of debugging support to provide.  It can be a
  number from 0 to 3, or one of the corresponding keywords:
  :minimum, :deploy, :develop, or :maximum.

  If :SERVE-EVENT is non-NIL, Nodgui will use SERVE-EVENT handlers instead of a
  blocking main loop.  This is only supported on SBCL and CMUCL.  Note that
  using SERVE-EVENT means that WITH-NODGUI will return immediately after evaluating
  BODY.

  If :STREAM is non-NIL, it should be a two-way stream connected to a running
  wish.  This will be used instead of running a new wish.

  With :title you can set the title of this window"
  (declare (ignore debug serve-event stream title))
  `(call-with-nodgui (lambda () ,@body) ,@keys))

(defun call-with-nodgui (thunk &rest keys &key (debug 2) stream serve-event remotep
                      &allow-other-keys)
  "Functional interface to with-nodgui, provided to allow the user the build similar macros."
  (declare (ignore stream))
  (flet ((start-wish ()
           (apply #'start-wish
                  :remotep remotep
                  (append (filter-keys '(:stream :debugger-class :debug-tcl)
                                       keys)
                          (list :debugger-class (debug-setting-condition-handler debug)))))
         (mainloop () (apply #'mainloop (filter-keys '(:serve-event) keys))))
    (let* ((*default-toplevel-title* (getf keys :title "notitle"))
           (*wish-args*              (append-wish-args (list +arg-toplevel-name+
                                                             *default-toplevel-title*)))
           (*wish*                   (make-nodgui-connection :remotep remotep)))
      (catch *wish*
        (unwind-protect
             (progn
               (start-wish)
               (multiple-value-prog1
                   (with-nodgui-handlers ()
                     (with-atomic (funcall thunk)))
                 (mainloop)))
          (unless serve-event
            (exit-wish)))))))

(defmacro with-modal-toplevel ((var &rest toplevel-initargs) &body body)
  `(let* ((,var (make-instance 'toplevel ,@toplevel-initargs))
          (*exit-mainloop* nil)
          ;(*buffer-for-atomic-output* nil)
          )
     (send-wish "update idletasks")
     (unwind-protect
         (catch 'modal-toplevel
           (block nil
             (on-close ,var (lambda () (return)))
             (grab ,var)
             (multiple-value-prog1
                 (progn ,@body)
               (flush-wish)
               (mainloop))))
       (grab-release ,var)
       (withdraw ,var)
       (flush-wish))))

(defun input-box (prompt &key (title "Input") default)
  (let* ((*exit-mainloop* nil)
         (ok t)
         (w (make-instance 'toplevel :title title))
         (l (make-instance 'label :master w :text prompt))
         (e (make-instance 'entry :master w :width 40))
         (f (make-instance 'frame :master w))
         (b_ok (make-instance 'button :master f :text "Ok"
                              :command (lambda ()
                                         (break-mainloop)
                                         )))
         (b_cancel (make-instance 'button :master f :text "Cancel"
                                  :command (lambda ()
                                             (setf ok nil)
                                             (break-mainloop)
                                             ))))
    (pack l :side :top :anchor :w)
    (pack e :side :top)
    (pack f :side :top :anchor :e)
    (pack b_cancel :side :right)
    (pack b_ok :side :right)
    (bind w "<Return>" (lambda (event)
                         (declare (ignore event))
                         (break-mainloop)))
    (when (and default (> (length default) 0))
      (setf (text e) default))
    (focus e)
    (grab w)
    (mainloop)
    (grab-release w)
    (withdraw w)
    (and ok
         (text e))))

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
