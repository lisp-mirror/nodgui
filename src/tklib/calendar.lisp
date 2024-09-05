;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2019 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.tklib.calendar)

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(a:define-constant +calendar-library-name+ "widget::calendar" :test #'string=)

(a:define-constant +calendar-date-format-m/d/y+ "%m/%d/%Y"
  :test          #'string=
  :documentation "Format of this date as day (2 digits) / month (2 digits) / year (4 digits)")

(a:define-constant +calendar-date-format-d/m/y+ "%d/%m/%Y"
  :test #'string=
  :documentation "Format of this date as month (2 digits) / day (2 digits) / year (4 digits)")

(a:define-constant +calendar-supported-language+
    '(:de :en :es
      :fr :gr :he
      :it :ja :sv
      :pl :pt :zh
      :fi :tr :nl
      :ru :crk
      :crx-nak :crx-lhe)
  :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defargs calendar ()
    dateformat
    font
    firstday
    highlightcolor
    language
    shadecolor
    showpast
    textvariable)

  (setf *initargs*
        (append *initargs*
                '((font       font       "~@[ -font {~a} ~]"       font       "")
                  (dateformat dateformat "~@[ -dateformat {~a} ~]" dateformat ""))))

  (defwrapper calendar (tktextvariable widget) () "widget::calendar"))

(defun %make-font (name size)
  (let* ((*suppress-newline-for-tcl-statements* t)
         (actual-name                           (tclize `(,name))))
    (font-create actual-name :size size)))

(defun make-calendar (&key
                        (language        :en)
                        (date-format     +calendar-date-format-d/m/y+)
                        (first-day       :monday)
                        (highlight-color (cl-colors2:as-rgb "FFCC00"))
                        (shade-color     (cl-colors2:as-rgb "888888"))
                        (font            +tk-default-font+))
  "Make a calendar widget.
Parameters are:

- language, the language the calendar is displayed; legal values are the ones in
'+calendar-supported-language+' (default :en);

- date-format, the format of the inner date of this widget; legal values are:
'+calendar-date-format-d/m/y+' and '+calendar-date-format-d/m/y+'
(default '+calendar-date-format-d/m/y+');

- first-day, the day of the week shown as the first column of the calendar: legal values are
:monday and :sunday (default :monday);

- highlight-color, background color of the selected day (should be a cl-color:rgb struct);
  default color #xFFCC00;

- shade-color, color of the shaded elements shown (should be a cl-color:rgb struct);
  default color #x888888;

- font, font used by the calendar default: '+tk-default-font+'.
"
  (assert (member language +calendar-supported-language+))
  (assert (or (string= date-format +calendar-date-format-d/m/y+)
              (string= date-format +calendar-date-format-m/d/y+)))
  (assert (or (eq first-day :monday)
              (eq first-day :sunday)))
  (require-tcl-package +calendar-library-name+)
  (with-no-emitted-newline
    (with-stringify-keyword
        (make-instance 'calendar
                       :language       (tclize `(,language))
                       :dateformat     (tclize `(,date-format))
                       :firstday       (tclize `(,first-day))
                       :highlightcolor (tclize `(,(rgb->tk highlight-color)))
                       :shadecolor     (tclize `(,(rgb->tk shade-color)))
                       :font           font))))

(defmethod (setf command) (val (object calendar))
  "Set the function to be called when a day is selected (by clicking),
the function should accepts a  parameter: the string corresponding the
selected   date  in   a   format  specified   when   the  widget   was
instanced (see: make-calendar).

That date can be parsed by parse-selected-date"
  (nodgui::add-callback (nodgui::name object) val)
  (format-wish "~a configure -command {callbackstring ~a}"
               (widget-path object)
               (nodgui::name object))
  val)

(defmethod (setf text) :after (val (object calendar))
  "See: set-date or set-date*"
  (format-wish "~A configure -textvariable {text_~a}"
               (widget-path object)
               (nodgui::name object))
  val)

(defgeneric set-date (object day month year))

(defgeneric set-date* (object date))

(defun cget-date-format (calendar)
  "Convenience function to get the string corresponding to the internal date of widget"
  (cget calendar :dateformat))

(defmethod set-date ((object calendar) (day integer) (month integer) (year integer))
  "Set the date for this calendar (that also will be shown and selected by the widget)"
  (let* ((date-format    (cget-date-format object))
         (format-control "~a/~a/~a")
         (date-as-string (cond
                          ((string= date-format +calendar-date-format-d/m/y+)
                           (format nil format-control day month year))
                          ((string= date-format +calendar-date-format-m/d/y+)
                           (format nil format-control month day year)))))
    (setf (text object) date-as-string)))

(defmethod set-date* ((object calendar) (date string))
    "Set the date for this calendar (that also will be shown and selected by the widget).
The second parameter should be a string matching format of: +calendar-date-format-m/d/y+ or
+calendar-date-format-d/m/y+"
  (multiple-value-bind (day month year)
      (parse-selected-date object date)
    (set-date object day month year)))

(defun parse-selected-date (widget date-as-text)
  "Parse a string according to the internal date format of widget,
(see: (setf command)). The allowed formats are: +calendar-date-format-d/m/y+ and
+calendar-date-format-m/d/y+"
  (let ((parsed      (mapcar #'parse-integer (split "/" date-as-text)))
        (date-format (cget-date-format widget)))
    (cond
      ((string= date-format +calendar-date-format-d/m/y+)
       (values (elt parsed 0)   ; day
               (elt parsed 1)   ; month
               (elt parsed 2))) ; year
      ((string= date-format +calendar-date-format-m/d/y+)
       (values (elt parsed 1)   ; day
               (elt parsed 0)   ; month
               (elt parsed 2)))))) ; year
