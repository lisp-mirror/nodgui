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

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(define-constant +no-event-value+ (cons nil nil) :test #'equalp)

(define-constant +arg-toplevel-name+ "-name" :test #'string=)

(defparameter *wish* nil
  "The current connection to an inferior wish.")

(defparameter *wish-connections* ()
  "Connections pushed aside by invoking the NEW-WISH restart in START-WISH.")

(defun do-execute (program args &optional (waitp nil))
  "execute program with args a list containing the arguments passed to the program
   if waitp is non-nil, the function will wait for the execution of the program to return.
   returns a two way stream connected to stdin/stdout of the program and the process object"
  (let ((proc #-ccl (uiop:launch-program (append (list program) args)
                                         :input        :stream
                                         :output       :stream
                                         :error-output :stream)
              ;; To  manage in  a concurrent  way, the  pipe to  wish
              ;; process on ccl we need  to add a ccl specific keyword
              ;; parameter: ":sharing :lock".

              ;; uiop:launch-program  allows  a   variable  number  of
              ;; keyword   parameters   via   &allow-other-keys   and,
              ;; moreover, other implementations that do not recognize
              ;; ":sharing parameter"  will simply discard  it. Anyway
              ;; for sake of clarity we  uses a different form for ccl
              ;; together with conditional reader macro
              #+ccl (uiop:launch-program (append (list program) args)
                                         :input        :stream
                                         :output       :stream
                                         :error-output :stream
                                         :sharing      :lock)))
    (unless proc
      (error "Cannot create process."))
    (when waitp
      (uiop:wait-process proc))
    (values
     (make-two-way-stream (uiop:process-info-output proc)
                          (uiop:process-info-input  proc))
     proc)))

(defstruct mainloop-coordination
  (mainloop-name -1)
  (pause         nil)
  (pause-lock    (make-lock "lock"))
  (pause-condvar (make-condition-variable))
  (stop          nil)
  (stop-lock     (make-lock "lock")))

;;; global var for holding the communication stream
(defstruct (nodgui-connection
             (:constructor make-nodgui-connection)
             (:conc-name #:wish-))
  (stream                   nil)
  (callbacks                (make-hash-table :test #'equal))
  (after-ids                (make-hash-table :test #'equal))
  (counter                  1)
  (after-counter            1)
  (data-queue               (make-instance 'q:synchronized-queue))
  (event-queue              (make-instance 'q:synchronized-queue))
  (lock                     (make-lock "lock"))
  (read-data-lock           (make-lock "read-data"))
  (flush-lock               (make-lock "flush"))
  (read-lock                (make-lock "read"))
  (accept-garbage-for-next-event  nil)
  ;; This is should be a function that takes a thunk, and calls it in
  ;; an environment with some condition handling in place.  It is what
  ;; allows the user to specify error-handling in START-WISH, and have
  ;; it take place inside of MAINLOOP.
  (call-with-condition-handlers-function (lambda (f) (funcall f)))
  (output-buffer nil)
  (error-collecting-thread nil)
  (input-collecting-thread nil)
  (main-loop-thread nil)
  (saved-main-loop-threads '())
  (main-loop-coordination-data nil)
  (saved-main-loop-coordination-data '())
  (variables (make-hash-table :test #'equal))
  (debug-level :development))

(defmethod wish-variable (name (wish nodgui-connection))
  (gethash name (wish-variables wish)))

(defmethod (setf wish-variable) (val name (wish nodgui-connection))
  (setf (gethash name (wish-variables wish)) val))

(defmacro with-nodgui-handlers (() &body body)
  `(funcall (wish-call-with-condition-handlers-function *wish*)
            (lambda () ,@body)))

(defun push-mainloop-thread ()
  (push (wish-main-loop-thread *wish*)
        (wish-saved-main-loop-threads *wish*)))

(defun pop-mainloop-thread ()
  (setf (wish-main-loop-thread *wish*)
        (pop (wish-saved-main-loop-threads *wish*))))

(defun push-mainloop-coordination-data ()
  (push (wish-main-loop-coordination-data *wish*)
        (wish-saved-main-loop-coordination-data *wish*)))

(defun pop-mainloop-coordination-data ()
  (setf (wish-main-loop-coordination-data *wish*)
        (pop (wish-saved-main-loop-coordination-data *wish*))))

;;; global connection information

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation 'make-nodgui-connection 'function)
        "Create a new NODGUI-CONNECTION object.  This represents a connection to a
    specific wish.  You can maintain connections to several distinct wish
    processes by binding *WISH* to the one you desire to communicate with, and
    using NODGUI functions within that dynamic scope."))

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to *trace-output*

(defparameter *debug-tk* nil)

;; if set to t, nodgui will report the buffer size sent to tk
(defparameter *debug-buffers* nil)

(defvar *wish-pathname*
  #+freebsd "wish8.6"
  #-freebsd "wish")

(defparameter *default-toplevel-name* "NODGUI")

(defvar *wish-args* '())

(defun append-wish-args (other-args)
  (append *wish-args* other-args))

(defvar *init-wish-hook* nil)

(defparameter *buffer-for-atomic-output* nil)

(definline dbg (fmt &rest args)
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (ignorable fmt args))
  #-suppress-debug-messages
  (when *debug-tk*
    (apply #'format *trace-output* fmt args)
    (format *trace-output* "~%")
    (finish-output *trace-output*)))

(defmacro with-send-batch (&rest body)
  `(let ((*buffer-for-atomic-output* t))
     ,@body
     (flush-wish)))

(defmacro with-send-wish-atomic ((stream) &body body)
  (a:with-gensyms (results)
    `(with-lock-held ((wish-lock *wish*))
       (let ((,results (with-output-to-string (,stream) ,@body)))
         (push ,results (wish-output-buffer *wish*))
         (flush-wish)))))

(defmacro with-flush (&rest body)
  `(progn
     ,@body
     (flush-wish)))

(defmacro send-lazy (&rest body)
  `(let ((*buffer-for-atomic-output* t))
     ,@body))

(defmacro with-lazy (&rest body)
  `(let ((*buffer-for-atomic-output* t))
     ,@body))

(defun call-with-read-data (fn)
  (with-lock-held ((wish-read-data-lock *wish*))
    (funcall fn)))

(defmacro with-read-data ((&optional (read-data-fn 'read-data)) &body body)
  (assert (or (null read-data-fn)
              (member read-data-fn '(read-data read-wish))))
  `(progn
     ,(if read-data-fn
          `(call-with-read-data (lambda () (progn ,@body (,read-data-fn))))
          `(call-with-read-data (lambda () (progn ,@body))))))

(defmacro with-main-loop-lock (() &body body)
  `(with-lock-held ((wish-main-loop-lock *wish*))
     ,@body))

(defun require-tcl-package (name)
  (format-wish (tcl-str (:if ([catch {package require ~a} err ])
                             ("tk_messageBox" \++
                                              -icon    error
                                              -type    ok
                                              -message $err%)))
               name))

(defun try-to-load-tcl-package (name)
  "Return non nil if the package has been successfully loaded"
  (with-read-data (nil)
    (format-wish (tclize `(senddata [ catch { package require ,name } ])))
    (tcl-error->boolean (read-data))))

(defparameter *tkimg-loaded-p* nil)

(defun init-tkimg ()
  (setf *tkimg-loaded-p* (try-to-load-tcl-package "Img")))

;;; setup of wish
;;; put any tcl function definitions needed for running nodgui here

(defun init-wish ()
  (format-wish "package require Tk")
  (send-wish (wish-init-code))
  (flush-wish)
  (init-tkimg)
  (dolist (fun *init-wish-hook*) ; run init hook functions
    (funcall fun)))

(defun init-tcl (&key debug-tcl)
  (let ((translation "lf"))
    #+(and (or windows win32) (not sbcl)) (setf translation "crlf")
    (format (wish-stream *wish*) (tcl-init-code) debug-tcl translation)))

(defun make-error-collecting-thread (error-stream)
  (make-thread (lambda ()
                    (ignore-errors
                      (let ((ch (read-char error-stream)))
                        (unread-char ch error-stream)
                        (error (make-condition 'tk-communication-error
                                               :message (read-line error-stream))))))
                  :name "nodgui collecting thread"))

;;; start wish and set (wish-stream *wish*)

(defun start-wish-pipe (&optional (stream nil))
  (if stream
      (setf (wish-stream *wish*) stream)
      (multiple-value-bind (proc-stream proc)
          (do-execute *wish-pathname* *wish-args*)
        (setf (wish-stream *wish*) proc-stream)
        (let ((error-stream (uiop:process-info-error-output proc)))
          (setf (wish-error-collecting-thread *wish*)
                (make-error-collecting-thread error-stream)))))
  #+(or mswindows windows win32) (sleep 1))

(defun start-wish (&key debug-tcl)
  ;; perform tcl initialisations
  (with-nodgui-handlers ()
    (init-tcl :debug-tcl debug-tcl)
    (init-wish)))

;;; CMUCL, SCL, and SBCL, use a two-way-stream and the constituent
;;; streams need to be closed.

(defun close-process-stream (stream)
  "Close a 'stream open by 'do-execute."
  (dbg "Closing wish stream: ~S~%" stream)
  (ignore-errors (close stream))
  #+(or :cmu :scl :sbcl)
  (when (typep stream 'two-way-stream)
    (dbg "closing two way stream")
    (close (two-way-stream-input-stream stream) :abort t)
    (close (two-way-stream-output-stream stream) :abort t))
  nil)

(define-constant +closing-loop-event+ :closing-wish :test #'eq)

(define-constant +closing-loop-input+ "puts 1" :test #'string=)

(defun indicate-stop-mainloop-threads ()
  (push-enqueued-event +closing-loop-event+)
  (send-wish +closing-loop-input+))

(defun exit-wish ()
  (with-nodgui-handlers ()
    (a:when-let ((stream (wish-stream *wish*)))
      (break-mainloop)
      (when (thread-alive-p (wish-error-collecting-thread *wish*))
        (destroy-thread (wish-error-collecting-thread *wish*)))
      (setf (nodgui::wish-accept-garbage-for-next-event *wish*) t)
      (indicate-stop-mainloop-threads)
      (send-wish "exit")
      ;;(close-process-stream stream))
      (setf (wish-stream *wish*) nil)
      #+:allegro (system:reap-os-subprocess)
      (setf *wish-connections* (remove *wish* *wish-connections*)))))

(defun exit-nodgui ()
  (exit-wish))

(defun send-wish (text)
  (with-lock-held ((wish-lock *wish*))
    (push text (wish-output-buffer *wish*))
    (unless *buffer-for-atomic-output*
      (flush-wish))))

(defun send-wish-line (data)
  "Send data  to wish  shell. data  are not processed  so they  must be
coupled with a 'gets' from the TCL side, for example.

Note also that this function  blocks the communication until wish read
the data (see the TCL proc: 'callbacks_validatecommand' in tcl-glue-code.lisp)"
  (with-lock-held ((wish-lock *wish*))
    (let ((*print-pretty* nil)
          (stream         (wish-stream *wish*))
          (line           (format nil "~a~%" data)))
      (when *debug-tk*
        (dbg "sending line: ~s~%" data))
      (format stream line)
      (finish-output stream))))

;; maximum line length sent over to Tk
(defparameter *max-line-length* 1000)

(defun flush-wish ()
  #.nodgui.config:default-optimization
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-lock-held ((wish-flush-lock *wish*))
    (let ((buffer (nreverse (wish-output-buffer *wish*))))
      (declare (list buffer))
      (when buffer
        (let ((*print-pretty* nil)
              (len    (loop for s string in buffer summing (length s)))
              (stream (wish-stream *wish*)))
          (declare (dynamic-extent len))
          (declare (fixnum len))
          (declare (stream stream))
          (incf len (the fixnum (length buffer)))
          (setf (wish-output-buffer *wish*) nil)
          (handler-bind ((stream-error (lambda (e) (handle-dead-stream e stream)))
                         #+lispworks
                         (comm:socket-error (lambda (e) (handle-dead-stream e stream))))
            (cond
              (*max-line-length*
               (when (or *debug-buffers*
                         *debug-tk*)
                 (dbg "buffer size ~a~%" len))
               (dolist (string buffer)
                 (declare (string string))
                 (loop while (> (length string)
                                (the fixnum *max-line-length*))
                    do
                      (let ((sub (subseq string 0 *max-line-length*)))
                        (setf string (subseq string *max-line-length*))
                        (format stream "bt \"~A\"~%" (tkescape2 sub))
                        (finish-output stream)
                        (dbg "bt \"~A\"~%" (tkescape2 sub))))
                 (format stream "bt \"~A~%\"~%" (tkescape2 string))
                 (dbg "bt \"~A\"~%" (tkescape2 string)))
               (format stream "process_buffer~%")
               (dbg "process_buffer~%"))
              (t
               ;; (format stream "bt {~D }~%" len)
               ;; (dbg "bt {~D }~%" len)
               (dolist (string buffer)
                 (format stream "bt \"~A~%\"~%" (tkescape2 string))
                 (dbg "bt \"~A\"~%" (tkescape2 string)))
               (format stream "process_buffer~%")
               (dbg "process_buffer~%")))
            (finish-output stream)
            (setf (wish-output-buffer *wish*) nil)))))))

(defun handle-dead-stream (err stream)
  (when *debug-tk*
    (dbg "Error sending command to wish: ~A" err))
  (ignore-errors (close stream))
  (exit-wish))

(defun format-for-wish (stream control &rest args)
  "sanitize and format  `args' using 'control as control  string for wish
process"
  (apply #'format stream control (mapcar #'sanitize args)))

(defun format-wish (control &rest args)
  "sanitize and format  `args' using 'control as control  string and send
to wish process"
  (send-wish (apply #'format-for-wish nil control args)))

;; differences:
;; cmucl/sbcl READ expressions only if there is one more character in the stream, if
;; it is a whitespace its discarded. Lispworks READs the expression as soon as it can
;; be fully read from the stream - no character is discarded
;; so I am printing an additional space after every READable expression printed from tcl,
;; this has to be eaten for read-line from the stream in lispworks (which returns the line
;; ending character, cmucl/sbcl don't)

(defun read-all (stream)
  (declare (stream stream)
           #-:lispworks (inline read-char-no-hang))
  (let ((c (read-char-no-hang stream nil nil))
        (s (make-array 256 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop
       while c
       do
         (vector-push-extend c s)
         (setf c (read-char-no-hang stream nil nil)))
    (coerce s 'simple-string)))

#+(and lispworks mswindows)
(lw:defadvice (mp:process-wait-with-timeout peek-char-no-hang :around)
    (whostate timeout &optional function &rest args)
  (apply #'lw:call-next-advice
         whostate
         (if (and (eq timeout 1)
                  (stringp whostate)
                  (string= whostate "Waiting for pipe input"))
             0.001
           timeout)
         function args))

;; set it to like 0.1 to simulate bad networks
;;(defparameter *read-delay* nil)

;;; read from wish
(defun read-wish ()
  "Reads from wish. If the next thing in the stream is looks like a lisp-list
  read it as such, otherwise read one line as a string."
  (flush-wish)
  (with-lock-held ((wish-read-lock *wish*))
    (let ((*read-eval* nil)
          (*package* (find-package :nodgui))
          (stream (wish-stream *wish*)))
      (if (eql #\( (peek-char t stream nil))
          (read stream nil)
          (read-line stream nil)))))

(defun stream-readable-p (stream)
  "return t, if there is something to READ on the stream"
  (declare #-:lispworks (inline read-char-no-hang unread-char))
  (handler-case
      (let ((c (read-char-no-hang stream)))
        (if c
            (progn
              (unread-char c stream)
              t)
            nil))
    (error (e)
      (list :read-stream-error e))))

(defun normalize-error (errors-list)
  (join-with-strings (rest errors-list) " "))

(defun accept-garbage-for-next-event-p ()
  "Sometimes tcl  lib print on  standard error for  debugging purpose,
  ignore the data got in this case setting this variable to non nil"
  (wish-accept-garbage-for-next-event *wish*))

(defun verify-event (event)
  (cond
    ((accept-garbage-for-next-event-p)
     (setf (wish-accept-garbage-for-next-event *wish*) nil)
     (list :ignored event))
    ((not (listp event))
     (error "When reading from tcl, expected a list but instead got ~S" event))
    ((eq (first event) :error)
     (error 'tk-error :message (normalize-error event)))
    (t event)))

(defun tcldebug (something)
  (format t "tcl debug: ~a~%" something)
  (finish-output))

(defun read-event (no-event-value)
  "read the next event from wish, return the event or nil, if there is no
event to read and blocking is set to nil"
  (handler-case
      (let ((wstream (wish-stream *wish*)))
        (with-lock-held ((wish-read-lock *wish*))
          (dbg "queue ~a" (q::container (wish-data-queue *wish*)))
          (let ((event (read-preserving-whitespace wstream t nil)))
            (dbg "raw event ~a" event)
            (if event
                (verify-event event)
                no-event-value))))
    (error (e)
      (list :read-stream-error e))))

(defun event-got-error-p (event)
  (eq (first event) :read-stream-error))

(defun check-enqueued-data ()
  (not (q:emptyp (wish-data-queue *wish*))))

(defun pop-enqueued-data ()
  (q:pop-block (wish-data-queue *wish*)))

(defun push-enqueued-data (data)
  (dbg "push data unblock ~a" data)
  (q:push-unblock (wish-data-queue *wish*) data))

(defun check-enqueued-event ()
  (not (q:emptyp (wish-event-queue *wish*))))

(defun pop-enqueued-event ()
  (q:pop-block (wish-event-queue *wish*)))

(defun push-enqueued-event (event)
  (q:push-unblock (wish-event-queue *wish*) event))

(defun read-data (&key (expected-list-as-data nil))
  "Read data from wish. Non-data events are postponed, bogus messages (eg.
error-strings) are ignored."
  (dbg "read data enter")
  (let ((data (pop-enqueued-data)))
    (if (listp data)
        (cond
          ((event-got-error-p data)
           (exit-wish)
           (return-from read-data nil))
          ((null data)
           (exit-wish)
           (return-from read-data nil))
          ((eq (first data) :data)
           (dbg "got data ~a" data)
           (if expected-list-as-data
               (return-from read-data (rest data))
               (return-from read-data (second data))))
          ((eq (first data) :debug)
           (if expected-list-as-data
               (tcldebug (rest data))
               (tcldebug (second data))))
          ((eq (first data) :error)
           (error 'tk-error :message (normalize-error data)))
          (t
           (finish-output)
           data))
        data)))

(defun read-keyword ()
  (with-read-data (nil)
    (let ((string (read-data)))
      (when (> (length string) 0)
        (values (intern #-scl (string-upcase string)
                        #+scl (if (eq ext:*case-mode* :upper)
                                  (string-upcase string)
                                  (string-downcase string))
                        :keyword))))))

;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the slot break-mainloop is non nil

(defun break-mainloop ()
  (let ((lock (mainloop-coordination-stop-lock (wish-main-loop-coordination-data *wish*))))
    (with-lock-held (lock)
      (setf (mainloop-coordination-stop (wish-main-loop-coordination-data *wish*))
            t))))

(defun break-mainloop-p ()
  (let ((lock (mainloop-coordination-stop-lock (wish-main-loop-coordination-data *wish*))))
    (with-lock-held (lock)
      (mainloop-coordination-stop (wish-main-loop-coordination-data *wish*)))))

(defgeneric handle-output (key params))

(defmethod handle-output (key params)
  (declare (ignore key params)))

(defun process-one-event (event)
  (when event
    (dbg "event:~s<=~%" event)
    (cond
      ((not (listp event)) nil)
      ((eq (first event)
           +wish-to-lisp-callback-reply+)
       (let ((params (rest event)))
         (callback (first params) (rest params))))
      ((eq (first event)
           +wish-to-lisp-event-reply+)
       (let* ((params (rest event))
              (callback-name (first params))
              (evp (rest params))
              (event (construct-tk-event evp)))
         (callback callback-name (list event))))
      ((eq (first event) :keepalive)
       (dbg "Ping from wish: ~{~A~^~}~%" (rest event)))
      ((eq (first event) :debug)
       (tcldebug (second event)))
      (t
       ;; handle-output does nothing!
       (handle-output (first event)
                      (rest event))))))

;; (defun process-events ()
;;   "A function to temporarliy yield control to wish so that pending
;; events can be processed, useful in long loops or loops that depend on
;; tk input to terminate"
;;   (let (event)
;;     (loop
;;      while (setf event (read-event))
;;      do (with-send-batch (process-one-event event)))))

(defparameter *event-popped-from-mainloop* nil)

(defun manage-wish-output (event)
  (dbg "manage ~a from" event)
  (cond
    ((null event)
     (error "Wish interpreter returned a null event"))
    ((event-got-error-p event)
     (error (second event)))
    ((eq (first event) :data)
     (push-enqueued-data event))
    ((or (eql event +no-event-value+)
         (eq (first event) :ignored))
     t)
    (t
     (push-enqueued-event event))))

(defparameter *popped-mainloop-event-lock* (make-lock "popped-mainloop-event-lock"))

(defparameter *popped-mainloop-event-condvar* (make-condition-variable))

(let ((mainloop-name -1))
  (defun start-main-loop (&key (thread-special-bindings *thread-default-special-bindings*))
    (incf mainloop-name)
    (dbg "start mainloop ~a" mainloop-name)
    (let ((wish-process *wish*)
          (coordination-data (make-mainloop-coordination :mainloop-name mainloop-name)))
      (push-mainloop-coordination-data)
      (setf (wish-main-loop-coordination-data *wish*) coordination-data)
      (flet ((maybe-wait-for-other-mainloops ()
               (with-lock-held ((mainloop-coordination-pause-lock coordination-data))
                 (loop while (mainloop-coordination-pause coordination-data)
                       do (condition-wait (mainloop-coordination-pause-condvar coordination-data)
                                             (mainloop-coordination-pause-lock coordination-data)))
                 nil)))
        (setf (wish-main-loop-thread *wish*)
              (make-thread (lambda ()
                                (let ((*wish*       wish-process)
                                      (current-name (mainloop-coordination-mainloop-name coordination-data)))
                                  (loop while (not (or (break-mainloop-p)
                                                       (maybe-wait-for-other-mainloops)))
                                        do
                                           (dbg " mainloop ~a waiting"
                                                  current-name)
                                           (let ((event (pop-enqueued-event)))
                                             (dbg "pop from mainloop ~a ~a"
                                                  current-name
                                                  event)
                                             (when (null (check-enqueued-event))
                                               (with-lock-held (*popped-mainloop-event-lock*)
                                                 (condition-notify *popped-mainloop-event-condvar*)))
                                             (with-nodgui-handlers ()
                                               (with-flush
                                                   (let ((*event-popped-from-mainloop* t))
                                                     (process-one-event event))))))
                                  (dbg "main-loop ~a terminated" current-name)))
                              :name             (format nil "main loop ~a" mainloop-name)
                              :initial-bindings thread-special-bindings))))))

(defun filter-keys (desired-keys keyword-arguments)
  (loop for (key val) on keyword-arguments by #'cddr
        when (find key desired-keys) nconc (list key val)))

(defun start-reading-loop (&key (thread-special-bindings *thread-default-special-bindings*))
  (dbg "inizio reading loop")
  (let ((wish-process *wish*))
    (setf (wish-input-collecting-thread *wish*)
          (make-thread (lambda ()
                            (let ((*wish* wish-process))
                              (loop while (not (break-mainloop-p))
                                    do
                                       (dbg "reading input wait")
                                       (handler-case
                                           (let ((input (read-event +no-event-value+)))
                                             (dbg "read input ~a" input)
                                             (manage-wish-output input))
                                         (error (e)
                                           (exit-wish)
                                           (if (eq (wish-debug-level *wish*)
                                                   :deploy)
                                               (show-debugger e)
                                               (error e)))))
                              (dbg "read input thread terminated")))
                          :name "read loop"
                          :initial-bindings thread-special-bindings))))

;;; wrapper macro - initializes everything, calls body and then mainloop

(defmacro with-nodgui ((&rest keys
                        &key
                          (title   "")
                          (debug    :development)
                          (main-loop-thread-special-bindings *thread-default-special-bindings*)
                          (stream nil)
                        &allow-other-keys)
                    &body body)
  "Create a new Nodgui connection, evaluate BODY, and enter the main loop.

  :DEBUG indicates the level of debugging support to provide.  It can be a
  one of the keywords: :deploy, :development.

  If :STREAM is non-NIL, it should be a two-way stream connected to a running
  wish.  This will be used instead of running a new wish.

  :THEME is a string designating a color theme. \"yaru\" is a modern theme shipped in nodgui, from the ttkthemes collection. See also `*themes-directory*'.

  With :name (or :title as a synonym) you can set both title and class
  name of this window"
  (declare (ignore debug stream title main-loop-thread-special-bindings))
  `(call-with-nodgui (lambda () ,@body) ,@keys))

(defun wait-mainloop-threads ()
  (join-thread (wish-main-loop-thread *wish*))
  (join-thread (wish-input-collecting-thread *wish*)))

(defun call-with-nodgui (thunk
                         &rest keys
                         &key stream &allow-other-keys)
  "Functional interface to with-nodgui, provided to allow the user the build similar macros."
  (assert (or (null (getf keys :debug))
              (member (getf keys :debug) '(:deploy :development))))
  (flet ((start-wish ()
           (apply #'start-wish
                  (append (filter-keys '(:stream)
                                       keys)))))
    (let* ((class-name  (or (getf keys :class)
                            (getf keys :name)))
           (title-value (getf keys :title))
           (main-loop-special-bindings (getf keys :main-loop-thread-special-bindings))
           (theme (or (getf keys :theme)
                      (default-theme)))
           (*default-toplevel-name* (or class-name
                                        title-value
                                        *default-toplevel-name*))
           (*wish-args*              (append-wish-args (list +arg-toplevel-name+
                                                             *default-toplevel-name*)))
           (*wish*                   (make-nodgui-connection)))
      (if (null (wish-stream *wish*))
          (start-wish-pipe stream)
          (restart-case (nodgui-error "There is already an inferior wish.")
            (new-wish ()
              :report "Create an additional inferior wish."
              (push *wish* *wish-connections*)
              (setf *wish* (make-nodgui-connection))
              (apply #'start-wish keys))))
      (when (getf keys :debugger-class)
        (setf (wish-call-with-condition-handlers-function *wish*)
              (make-call-with-condition-handlers-function (getf keys :debugger-class))))
      (setf (wish-debug-level *wish*)
            (getf keys :debug :development))
      (start-main-loop :thread-special-bindings main-loop-special-bindings)
      (start-reading-loop)
      (let ((results-after-exit nil))
        (multiple-value-prog1
            (with-nodgui-handlers ()
              (start-wish)
              (when title-value
                (set-root-toplevel-title title-value))
              (when theme
                (use-theme theme))
              (on-close (root-toplevel) (lambda () (exit-wish)))
              (with-flush
                  (setf results-after-exit (funcall thunk)))))
        (wait-mainloop-threads)
        results-after-exit))))

(defun show-debugger (error)
  (with-nodgui (:debugger-class 'graphical-condition-handler)
    (error error)))
