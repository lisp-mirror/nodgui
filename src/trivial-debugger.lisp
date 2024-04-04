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

;;; Basic graphical error/warning/etc handling

(defun make-condition-handler-function (&key
                                          (class 'graphical-condition-handler)
                                          (title "Error"))
  "Create a function appropriate for use in a handler-bind."
  (declare (optimize (debug 3)))
  (if class
      (let ((prototype (make-instance class :prototype t)))
        (lambda (condition)
          (when (handle-condition-p prototype condition)
            (setf (backtrace prototype)
                  (with-output-to-string (out)
                    (print-backtrace condition out)))
            (with-toplevel (toplevel-widget :title title)
              (let ((debugger (make-instance class
                                             :backtrace (backtrace prototype)
                                             :master  toplevel-widget
                                             :condition condition)))
                (on-close toplevel-widget
                          (lambda ()
                            (exit-wish)
                            (abort-condition-handler debugger)))
                (pack debugger))))))
      (constantly nil)))

;;; The protocol for graphical condition handlers. This, and they must
;;; take :master, :condition and :prototype initargs.

(defgeneric handle-condition-p (handler condition)
  (:documentation
   "Handlers will be asked about all signalled conditions, errors, warnings or otherwise.
    They may choose to handle or decline them via this method."))

(defgeneric handler-condition (handler)
  (:documentation "An accessor for the condition that this handler is handling. May be nil."))

(defgeneric debugp (handler)
  (:documentation "Should this handler offer to send the user into the debugger?"))

(defgeneric describe-condition (handler)
  (:documentation "A text description of the condition, to be presented to the user."))

(defgeneric compute-buttons (handler master)
  (:documentation
   "This method should return a list of buttons (generally for invoking restarts) to be packed."))

(defgeneric report-bug (handler)
  (:documentation "Report this condition as a bug to the developer."))

(defgeneric abort-condition-handler (handler)
  (:documentation
   "Get rid of this handler; called, for example, when the user closes the handler's window.
    A handler basically has two choices here:

    - Call cl:abort, which will abort the Nodgui event that raised this condition.

    - Throw to nodgui:modal-toplevel, which will leave the condition unhandled."))

(defclass nodgui-condition-handler (frame)
  ((prototypep
    :initform nil
    :initarg :prototype
    :documentation
    "When set, do not actually create a Tk widget, this instance exists for purposes of generic-function dispatch only.")
   (condition :initform nil :initarg :condition :accessor handler-condition)))

(defmethod initialize-instance :around ((handler nodgui-condition-handler) &key prototype
                                        &allow-other-keys)
  (if prototype
      handler
      (call-next-method)))

;;; Our default condition handler, meant for use by the developer
;;; writing an Nodgui application. For other uses, this can be usefully
;;; subclassed and adapted.

(defclass graphical-condition-handler (nodgui-condition-handler)
  ((debugp :initform t :initarg :debugp :accessor debugp)
   (backtrace :initform t :initarg :backtrace :accessor backtrace)
   (details-pane :accessor details-pane
                 :documentation "The scrolled-text where we display the stacktrace.")))

(defmethod handle-condition-p ((handler graphical-condition-handler) condition)
  (typep condition 'serious-condition))

(defmethod initialize-instance :after ((handler graphical-condition-handler) &rest ignore)
  (declare (ignore ignore))
  (let* ((message (make-instance 'label
                    :master handler
                    :text (describe-condition handler)
                    :justify :left))
         (buttons-frame (make-instance 'frame :master handler)))
    (setf (details-pane handler) (make-instance 'scrolled-text :master handler))
    (pack (mapc (lambda (button) (setf (master button) handler))
                (compute-buttons handler buttons-frame))
          :side :left)
    (pack (list message buttons-frame))))

(defmethod describe-condition ((handler graphical-condition-handler))
  (let ((cont (find-restart 'continue))
        (cond (handler-condition handler)))
    (cond
      ((and cont cond) (format nil "~A~%~A?" cond cont))
      (cond (princ-to-string cond))
      (cont (format nil "An internal error has occured.~%~A?" cont))
      (t "An internal error has occured."))))

(defmethod compute-buttons ((handler graphical-condition-handler) master)
  (let ((exit (make-instance 'button :master master :text "Exit"))
        (show (make-instance 'button :master master :text "Show Details"))
        (report (make-instance 'button :master master :text "Report Bug"))
        (details (details-pane handler))
        (backtrace-done-p nil))
    (labels ((ensure-backtrace ()
               (unless backtrace-done-p
                 (append-text details (backtrace handler))
                 (setf backtrace-done-p t)))
             (show ()
               (ensure-backtrace)
               (pack details)
               (setf (text show) "Hide details"
                     (command show) #'hide))
             (hide ()
               (pack-forget details)
               (setf (text show) "Show details"
                     (command show) #'show))
             (exit ()
               (ignore-errors (exit-wish))))
      (setf (command exit) #'exit
            (command show) #'show
            (command report) (lambda () (report-bug handler)))
      (list exit show report))))

(defun print-backtrace (error stream)
  (format stream
          "~a~%  [Condition of type ~a]"
          error
          (class-name (class-of error)))
  (uiop:print-condition-backtrace error :stream stream))

(defmethod report-bug ((handler graphical-condition-handler))
  ;; This is modal only because the earlier non-modal one sometimes
  ;; died before the error report could be sent. I'd rather not be
  ;; modal here, and Nodgui users are encouraged to write their own bug
  ;; reporters that start a new Tcl/Tk process and send reports over
  ;; the net.
  (with-modal-toplevel (tl :title "Save a bug report")
    (let* ((toplevel-widget (modal-toplevel-root-widget tl))
           (summary  (make-instance 'label :master toplevel-widget :text "Summary:"))
           (esummary (make-instance 'entry :master toplevel-widget :text ""))
           (desc     (make-instance 'message :master toplevel-widget :width 400
                                             :text "Please describe what you were doing, and where so the developers can try to reproduce this situation"))
           (edesc    (make-instance 'scrolled-text :master toplevel-widget))
           (bsave    (make-instance 'button :master toplevel-widget :text "Save")))
      (labels ((save ()
                 (let ((summary (text esummary))
                       (desc (text edesc))
                       (file (get-save-file)))
                   (with-open-file (out file :direction :output :if-exists :supersede)
                     (multiple-value-bind (sec min hr day mo yr)
                         (decode-universal-time (get-universal-time) 0)
                       (let ((timestamp (format nil "~D/~D/~D ~D:~D:~D GMT"
                                                yr mo day hr min sec)))
                         (format out
                                 "This bug report was generated by the Nodgui debugger on ~A.~%"
                                 timestamp)
                         (format out "Nodgui version: ~A~%" +nodgui-version+)
                         (format out "Lisp: ~A ~A~%"
                                 (lisp-implementation-type) (lisp-implementation-version))
                         (format out "Summary: ~A~%Description:~%~A~%~%" summary desc)
                         (format out (backtrace handler) out))))
                   (exit-from-modal-toplevel tl))))
        (setf (command bsave) #'save)
        (grid-columnconfigure toplevel-widget 0 :weight 0)
        (grid-columnconfigure toplevel-widget 1 :weight 1)
        (grid summary 2 0 :sticky :w)
        (grid esummary 2 1 :sticky :ew)
        (grid desc 3 0 :columnspan 2 :sticky :w)
        (grid edesc 4 0 :columnspan 2 :sticky :ew)
        (grid bsave 5 0)))))

(defmethod abort-condition-handler ((handler graphical-condition-handler))
  (abort))

(defun debugger-test (&optional (debugger-class 'graphical-condition-handler))
  (with-nodgui (:debugger-class debugger-class)
    (pack (list (make-instance 'label :text (format nil "Debugger"))
                (make-instance 'button
                  :text "Error"
                  :command (lambda () (error "This is an error.")))
                (make-instance 'button
                  :text "Continuable Error"
                  :command (lambda ()
                             (cerror "Keep going with this computation"
                                     "This is an continuable error.")
                             (do-msg "You chose to continue!" :title "Congratulations")))
                (make-instance 'button
                  :text "Warning"
                  :command (lambda ()
                             (warn "This is a warning.")
                             (do-msg "After a warning was raised, computation continued."
                               :title "And then...")))
                (make-instance 'button
                  :text "Signal"
                  :command (lambda ()
                             (signal 'condition)
                             (do-msg "After a condition was signalled, computation continued."
                               :title "And then...")))))))
