(in-package :nodgui.tklib.calendar)

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(define-constant +calendar-library-name+ "widget::calendar" :test #'string=)

(define-constant +calendar-date-format-m/d/y+ "%m/%d/%Y" :test #'string=)

(define-constant +calendar-date-format-d/m/y+ "%d/%m/%Y" :test #'string=)

(define-constant +calendar-supported-language+
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
         (actual-name                           (tclize `(,#[name ]))))
    (font-create actual-name :size size)))

(defun make-calendar (&key
                        (language        :en)
                        (date-format     +calendar-date-format-d/m/y+)
                        (first-day       :monday)
                        (highlight-color (cl-colors:as-rgb "FFCC00"))
                        (shade-color     (cl-colors:as-rgb "888888"))
                        (font            +tk-default-font+))
  (assert (member language +calendar-supported-language+))
  (assert (or (string= date-format +calendar-date-format-d/m/y+)
              (string= date-format +calendar-date-format-m/d/y+)))
  (assert (or (eq first-day :monday)
              (eq first-day :sunday)))
  (require-tcl-package +calendar-library-name+)
  (let ((*suppress-newline-for-tcl-statements* t))
    (make-instance 'calendar
                   :language       (tclize `(,#[language ]))
                   :dateformat     (tclize `(,#[date-format ]))
                   :firstday       (tclize `(,#[first-day ]))
                   :highlightcolor (tclize `(,#[(rgb->tk highlight-color) ]))
                   :shadecolor     (tclize `(,#[(rgb->tk shade-color) ]))
                   :font           font)))

(defmethod (setf command) (val (object calendar))
  (nodgui::add-callback (nodgui::name object) val)
  (format-wish "~a configure -command {callbackstring ~a}"
               (widget-path object)
               (nodgui::name object))
  val)

(defmethod (setf text) :after (val (object calendar))
  (format-wish "~A configure -textvariable {text_~a}"
               (widget-path object)
               (nodgui::name object))
  val)

(defgeneric set-date (object day month year))

(defgeneric set-date* (object date))

(defun cget-date-format (calendar)
  (cget calendar :dateformat))

(defmethod set-date ((object calendar) (day integer) (month integer) (year integer))
  (let* ((date-format    (cget-date-format object))
         (format-control "~a/~a/~a")
         (date-as-string (cond
                          ((string= date-format +calendar-date-format-d/m/y+)
                           (format nil format-control day month year))
                          ((string= date-format +calendar-date-format-m/d/y+)
                           (format nil format-control month day year)))))
    (setf (text object) date-as-string)))

(defmethod set-date* ((object calendar) (date string))
  (multiple-value-bind (day month year)
      (parse-selected-date object date)
    (set-date object day month year)))

(defun parse-selected-date (widget date-as-text)
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
