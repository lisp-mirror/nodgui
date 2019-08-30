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

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; widget class built helper functions

;;(defparameter *generate-accessors* nil)

  (defun iarg-name (arg) (nth 0 arg))

  (defun iarg-key (arg) (nth 1 arg))

  (defun iarg-format (arg) (nth 2 arg))

  (defun iarg-code (arg) (nth 3 arg))

  (defun iarg-comment (arg) (nth 4 arg))

  (defparameter *initargs*
    '((button.background
       Button.background
       "~@[ -Button.background {~(~a~)}~]"
       button.background
       "")
      (Button.cursor
       Button.cursor
       "~@[ -Button.cursor {~(~a~)}~]"
       Button.cursor
       "")
      (Button.relief Button.relief "~@[ -Button.relief {~(~a~)}~]" Button.relief "")
      (activebackground activebackground "~@[ -activebackground {~(~a~)}~]" activebackground
       "background of the active area")
      (activeborderwidth
       activeborderwidth
       "~@[ -activeborderwidth {~(~a~)}~]"
       activeborderwidth
       "the border width for active widgets (when the mouse cursor is over the widget)")
      (activeforeground
       activeforeground
       "~@[ -activeforeground {~(~a~)}~]"
       activeforeground
       "foreground color for active widgets (when the mouse cursor is over the widget)")
      (activerelief activerelief
       "~@[ -activerelief {~(~a~)}~]"
       activerelief
       "the border relief for active widgets (when the mouse cursor is over the widget)")
      (activestyle
       activestyle
       "~@[ -activestyle {~(~a~)}~]"
       activestyle
       "the style for drawing the active part (dotbox, none, underline (default))")
      (anchor
       anchor
       "~@[ -anchor {~(~a~)}~]"
       anchor
       "specify the alignment of text/image drawn on the widget, one of (:n :w :s :e :nw :sw :se :ne) with :nw designating the top left corner")
      (aspect
       aspect
       "~@[ -aspect {~(~a~)}~]"
       aspect
       "Aspect ratio for the wrapping of the text. 100 means that the text is redered as wide as, tall, 200 twice as wide.")
      (autoseparators
       autoseparators
       "~:[~; -autoseparators 1~]"
       autoseparators
       "when t, separators are added automatically to the undo stack")
      (background background
       "~@[ -background {~(~a~)}~]"
       background
       "background color of the widget")
      (bigincrement
       bigincrement
       "~@[ -bigincrement {~(~a~)}~]"
       bigincrement
       "size of the big step increment")
      (bitmap bitmap
       "~@[ -bitmap {~(~a~)}~]"
       bitmap
       "the bitmap to display on the widget, the display is affected by the options 'anchor' and 'justify'")
      (borderwidth
       borderwidth
       "~@[ -borderwidth {~(~a~)}~]"
       borderwidth
       "width of the border around the widget in pixels")
      (class
       class
       "~@[ -class {~(~a~)}~]"
       class
       "the class of the widget, used for lookup in the option database. This option cannot be changed after the widget creation.")
      (closeenough
       closeenough
       "~@[ -closeenough {~(~a~)}~]"
       closeenough
       "dermines when the mouse coursor is considered to be inside a shape, the default is 1.0")
      (colormap colormap "~@[ -colormap {~(~a~)}~]" colormap
       "The colormap to use for the widget.")
      (command
       command
       "~@[ -command {callback ~a}~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (name widget)))
       "function to call when the action of the widget is executed")

      (cbcommand command
       "~@[ -command {callbackval ~{~a $~a~}}~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (list (name widget) (name widget))))
       "function to call when the action of the widget is executed")
      (scalecommand
       command
       "~@[ -command {callbackval ~a }~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (name widget)))
       "function to call when the action of the widget is executed")
      (spinbox-command
       command
       "~@[ -command {callbackstring ~a %s}~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (name widget))))
      (command-radio-button
       command
       "~@[ -command {callbackval ~{~a $~a~}}~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (list (name widget) (radio-button-variable widget))))
       "function to call when the action of the widget is executed")
      (command-scrollbar
       command
       "~@[ -command {callback ~a}~]"
       (and command
            (progn
              (add-callback (name widget) command)
              (name widget)))"")
      (cols cols "~@[ -cols ~d~]" cols "")
      (compound compound "~@[ -compound {~(~a~)}~]" compound "")
      (confine
       confine
       "~:[~; -confine 1~]"
       confine
       "if t (default) allowed values for view are confined to the scrollregion")
      (container
       container
       "~:[~; -container 1~]"
       container
       "if t, then the widget will be used as a container for other widgets.")
      (cursor
       cursor
       "~@[ -cursor {~(~a~)}~]"
       cursor
       "mouse pointer to display on the widget (valid values are listed in *cursors*)")
      (default default "~@[ -default {~(~a~)}~]" default "")
      (digits
       digits
       "~@[ -digits {~(~a~)}~]"
       digits
       "number of digits to use when converting the value to a string.")
      (direction direction "~@[ -direction {~(~a~)}~]" direction "")
      (disabledbackground
       disabledbackground
       "~@[ -disabledbackground {~(~a~)}~]"
       disabledbackground
       "")
      (disabledforeground
       disabledforeground
       "~@[ -disabledforeground {~(~a~)}~]"
       disabledforeground
       "")
      (elementborderwidth
       elementborderwidth
       "~@[ -elementborderwidth {~(~a~)}~]"
       elementborderwidth
       "")
      (exportselection exportselection "~@[ -exportselection {~(~a~)}~]" exportselection "")
      (font font "~@[ -font {~a}~]" font "font to use to display text on the widget")
      (foreground foreground "~@[ -foreground {~(~a~)}~]" foreground "foreground color of the widget")
      (format format "~@[ -format {~(~a~)}~]" format "")
      (from from "~@[ -from {~(~a~)}~]" from "")
      (handlepad handlepad "~@[ -handlepad {~(~a~)}~]" handlepad "")
      (handlesize handlesize "~@[ -handlesize {~(~a~)}~]" handlesize "")
      (height height "~@[ -height {~(~a~)}~]" height "height of the widget")
      (highlightbackground highlightbackground
       "~@[ -highlightbackground {~(~a~)}~]"
       highlightbackground
       "")
      (highlightcolor highlightcolor "~@[ -highlightcolor {~(~a~)}~]" highlightcolor "")
      (highlightthickness
       highlightthickness
       "~@[ -highlightthickness {~(~a~)}~]"
       highlightthickness "")
      ;(image image "~@[ -image ~(~a~)~]" (and image (name image))
      (image image "~@[ -image {~a}~]" (and image (down (name image)))
       "the image to display on the widget, the display is affected by the options 'anchor' and 'justify'")
      (increment
       increment
       "~@[ -increment {~(~a~)}~]"
       increment
       "size of the increment of the widget")
      (indicatorOn indicatorOn "~@[ -indicatorOn {~(~a~)}~]" indicatorOn "")
      (insertbackground insertbackground "~@[ -insertbackground {~(~a~)}~]" insertbackground "")
      (insertborderWidth insertborderWidth "~@[ -insertborderWidth {~(~a~)}~]" insertborderWidth "")
      (insertofftime insertofftime "~@[ -insertofftime {~(~a~)}~]" insertofftime "")
      (insertontime insertontime "~@[ -insertontime {~(~a~)}~]" insertontime "")
      (insertwidth insertwidth "~@[ -insertwidth {~(~a~)}~]" insertwidth "")
      (invalidcommand invalidcommand "~@[ -invalidcommand {~(~a~)}~]" invalidcommand "")
      (jump jump "~@[ -jump {~(~a~)}~]" jump "")
      (justify justify "~@[ -justify {~(~a~)}~]" justify "justification of the text on the widget")
      (label label "~@[ -label {~(~a~)}~]" label "text to display on the widget")
      (labelanchor labelanchor "~@[ -labelanchor {~(~a~)}~]" labelanchor "")
      (labelwidget labelwidget "~@[ -labelwidget {~(~a~)}~]" labelwidget "")
      (length length "~@[ -length {~(~a~)}~]" length "")
      (listvariable listvariable "~@[ -listvariable {~(~a~)}~]" listvariable "")
      (maxundo maxundo "~@[ -maxundo {~(~a~)}~]" maxundo "")
      (menu menu "~@[ -menu {~(~a~)}~]" menu "")
      (offrelief offrelief "~@[ -offrelief {~(~a~)}~]" offrelief "")
      (offvalue offvalue "~@[ -offvalue {~(~a~)}~]" offvalue "")
      (offset offset "~@[ -offset {~(~a~)}~]" offset "")
      (onvalue onvalue "~@[ -onvalue {~(~a~)}~]" onvalue "")
      (opaqueresize opaqueresize "~@[ -opaqueresize {~(~a~)}~]" opaqueresize "")
      (orient orientation
       "~@[ -orient {~(~a~)}~]"
       orientation
       "orientation of the widget (horizontal, vertical)")
      (overrelief
       overrelief
       "~@[ -overrelief {~(~a~)}~]"
       overrelief
       "relief of the border, when the mouse is over the widget")
      (padx padx "~@[ -padx {~(~a~)}~]" padx "padding around text displayed on the widget")
      (pady pady "~@[ -pady {~(~a~)}~]" pady "padding around text displayed on the widget")
      (postcommand postcommand "~@[ -postcommand {~(~a~)}~]" postcommand "")
      (readonlybackground
       readonlybackground
       "~@[ -readonlybackground {~(~a~)}~]"
       readonlybackground
       "")
      (relief
       relief
       "~@[ -relief {~(~a~)}~]"
       relief
       "relief of the widgets border (raised, sunken, ridge, groove)")
      (repeatdelay repeatdelay "~@[ -repeatdelay {~(~a~)}~]" repeatdelay "")
      (repeatinterval repeatinterval "~@[ -repeatinterval {~(~a~)}~]" repeatinterval "")
      (resolution resolution "~@[ -resolution {~(~a~)}~]" resolution "")
      (rows rows "~@[ -rows ~d~]" rows "")
      (sashcursor sashcursor "~@[ -sashcursor {~(~a~)}~]" sashcursor "")
      (sashpad sashpad "~@[ -sashpad {~(~a~)}~]" sashpad "")
      (sashrelief sashrelief "~@[ -sashrelief {~(~a~)}~]" sashrelief "")
      (sashwidth sashwidth "~@[ -sashwidth {~(~a~)}~]" sashwidth "")
      (screen screen "~@[ -screen {~(~a~)}~]" screen "screen on which the toplevel is to be shown")
      (scrollregion
       scrollregion
       "~@[ -scrollregion {~(~a~)}~]"
       scrollregion
       "region in which the canvas should be scolled")
      (selectbackground selectbackground "~@[ -selectbackground {~(~a~)}~]" selectbackground "")
      (selectborderwidth selectborderwidth "~@[ -selectborderwidth {~(~a~)}~]" selectborderwidth "")
      (selectcolor selectcolor "~@[ -selectcolor {~(~a~)}~]" selectcolor "")
      (selectforeground selectforeground "~@[ -selectforeground {~(~a~)}~]" selectforeground "")
      (selectimage selectimage "~@[ -selectimage {~(~a~)}~]" selectimage "")
      (selectmode selectmode "~@[ -selectmode {~(~a~)}~]" selectmode "")
      (setgrid setgrid "~@[ -setgrid {~(~a~)}~]" setgrid "")
      (show show "~@[ -show {~(~a~)}~]" show "")
      (showhandle showhandle "~@[ -showhandle {~(~a~)}~]" showhandle "")
      (showvalue showvalue "~@[ -showvalue {~(~a~)}~]" showvalue "")
      (sliderlength sliderlength "~@[ -sliderlength {~(~a~)}~]" sliderlength "")
      (sliderrelief sliderrelief "~@[ -sliderrelief {~(~a~)}~]" sliderrelief "")
      (spacing1 spacing1 "~@[ -spacing1 {~(~a~)}~]" spacing1 "")
      (spacing2 spacing2 "~@[ -spacing2 {~(~a~)}~]" spacing2 "")
      (spacing3 spacing3 "~@[ -spacing3 {~(~a~)}~]" spacing3 "")
      (state state "~@[ -state {~(~a~)}~]" state "")
      (style style "~@[ -style ~s~]" style "")
      (tabs tabs "~@[ -tabs {~(~a~)}~]" tabs "")
      (takefocus
       takefocus
       "~@[ -takefocus {~(~a~)}~]"
       takefocus
       "if true, the widget can take the focus")
      (tearoff tearoff "~@[ -tearoff {~(~a~)}~]" tearoff "if true, the menu can be torn off")
      (tearoffcommand tearoffcommand "~@[ -tearoffcommand {~(~a~)}~]" tearoffcommand "")
      (text text "~@[ -text \"~a\"~]" text "")
      ;;(textvariable textvariable
      ;;"~@[ -textvariable text_~a~]" (and textvariable (name widget)) "")
      (textvariable text "~@[ -textvariable {text_~a}~]"
       (progn
         (format-wish "global {text_~a} ; set {text_~a} \"~a\"" (name widget) (name widget)
                      (or text "" ))
         (name widget)) "")
      (tickinterval tickinterval "~@[ -tickinterval {~(~a~)}~]" tickinterval "")
      (title title "~@[ -title {~(~a~)}~]" title "")
      (to to "~@[ -to {~(~a~)}~]" to "")
      (troughcolor troughcolor "~@[ -troughcolor {~(~a~)}~]" troughcolor "")
      (type type "~@[ -type {~(~a~)}~]" type "")
      (underline underline "~@[ -underline {~(~a~)}~]" underline "")
      (undo undo "~@[ -undo {~(~a~)}~]" undo "")
      (use use "~@[ -use {~(~a~)}~]" use "")
      (validate validate "~@[ -validate {~(~a~)}~]" validate "")
      ;(validatecommand validatecommand "~@[ -validatecommand ~(~a~)~]" validatecommand "")
      (validatecommand validatecommand "~@[ -validatecommand {callback ~a;1}~]"
       (and validatecommand
        (progn
          (add-callback (name widget) validatecommand)
          (name widget))))
      (value value "~@[ -value {~(~a~)}~]" value "")
      (value-radio-button nil "~@[ -value {~(~a~)}~]" (radio-button-value widget)
       "value for the radio button group to take, when the button is selected")
      (values values "~@[ -values {~{{~a}~^ ~}}~]" values "")
      (variable
       variable
       "~@[ -variable {~(~a~)}~]"
       variable
       "name of the variable associated with the widget")
      (variable-radio-button
       nil
       "~@[ -variable {~(~a~)}~]"
       (radio-button-variable widget)
       "name of the radio button group the button shall belong to as a string")
      (visual visual "~@[ -visual {~(~a~)}~]" visual "")
      (width width "~@[ -width {~(~a~)}~]" width "width of the widget")
      (wrap wrap "~@[ -wrap {~(~a~)}~]" wrap "")
      (wraplength wraplength "~@[ -wraplength {~(~a~)}~]" wraplength "")
      (xscrollcommand xscrollcommand "~@[ -xscrollcommand {~(~a~)}~]" xscrollcommand "")
      (xscrollincrement xscrollincrement "~@[ -xscrollincrement {~(~a~)}~]" xscrollincrement "")
      (yscrollcommand yscrollcommand "~@[ -yscrollcommand {~(~a~)}~]" yscrollcommand "")
      (yscrollincrement yscrollincrement "~@[ -yscrollincrement {~(~a~)}~]" yscrollincrement "")))

  (defparameter *class-args* '())

  (defun build-args (class parents defs)
    (declare (ignore class))
    ;;(format t  "class ~s parents ~s defs ~s~%" class parents defs) (finish-output)
    (let ((args nil))
      (dolist (p parents)
        (let ((arglist (rest (assoc p *class-args*))))
          ;;(format t "parent: ~s arglist: ~s~%" p arglist) (finish-output)
          (dolist (arg arglist)
            (unless (member arg args)
              (setf args (append args (list arg)))))))
      (loop
         while defs
         do
           (let ((arg (pop defs)))
             (cond
               ((eq arg :inherit)
                (let* ((inheritedclass (pop defs))
                       (arglist (rest (assoc inheritedclass *class-args*))))
                  (dolist (arg arglist)
                    (unless (member arg args)
                      (setf args (append args (list arg)))
                      ))))
               ((eq arg :delete)
                (setf args (delete (pop defs) args)))
               (t
                (setf args (append args (list arg)))))))
      ;;(format t "class: ~a args: ~a~&" class args) (finish-output)
      args
      )))

(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    (setf *class-args* (append (remove class *class-args* :key #'car)
                               (list (cons class args))))
    `(setf *class-args* (append (remove (quote ,class) *class-args* :key #'car)
                                (list '(,class ,@args))))))

(defmacro defwrapper (class parents slots cmd &rest code)
  (let ((args (sort (copy-list (rest (assoc class *class-args*)))
                    (lambda (x y)
                      (string< (symbol-name x) (symbol-name y))))))
    (let ((cmdstring (format nil "~~a ~~~~A "))
          (codelist nil)
          (keylist nil)
          (accessors nil))
      (dolist (arg args)
        (let ((entry (assoc arg *initargs*)))
          (cond
            (entry
             (setf cmdstring (concatenate 'string cmdstring (iarg-format entry)))
             (when (iarg-key entry)
               (setf keylist (append keylist (list (iarg-key entry)))))
             (setf codelist (append codelist (list (iarg-code entry)))))
            (t
             (setf cmdstring (strcat cmdstring
                                     (format nil "~~@[ {-~(~a~)} {~~(~~a~~)}~~]" arg)))
             (setf keylist (append keylist (list arg)))
             (setf codelist (append codelist (list arg)))))))
      (push `(widget-class-name :accessor widget-class-name
                                :initform ,cmd
                                :allocation :class)
            slots)
      `(progn
         (defclass ,class (,@parents)
           ,slots)
         (defmethod initialize-instance :after ((widget ,class) &key ,@keylist)
                    (setf (init-command widget)
                          (format nil ,cmdstring (widget-class-name widget)
                                  ,@(mapcar (lambda (a) `(sanitize ,a)) codelist)))
                    ,@code)
         ,@accessors))))
