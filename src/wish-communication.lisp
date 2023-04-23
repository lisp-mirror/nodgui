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

(define-constant +arg-toplevel-name+ "-name" :test #'string=)

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

;;; global var for holding the communication stream
(defstruct (nodgui-connection
             (:constructor make-nodgui-connection)
             (:conc-name #:wish-))
  (stream                   nil)
  (callbacks                (make-hash-table :test #'equal))
  (after-ids                (make-hash-table :test #'equal))
  (counter                  1)
  (after-counter            1)
  (data-queue               ())
  (event-queue              ())
  (waiting-data-p           nil)
  (lock                     (bt:make-recursive-lock))
  (flush-lock               (bt:make-recursive-lock))
  (read-lock                (bt:make-recursive-lock))
  (read-data-lock           (bt:make-recursive-lock))
  (read-event-lock          (bt:make-recursive-lock))
  (main-iteration-lock      (bt:make-recursive-lock))
  ;; This is should be a function that takes a thunk, and calls it in
  ;; an environment with some condition handling in place.  It is what
  ;; allows the user to specify error-handling in START-WISH, and have
  ;; it take place inside of MAINLOOP.
  (call-with-condition-handlers-function (lambda (f) (funcall f)))
  (output-buffer nil)
  (error-collecting-thread nil)
  (variables (make-hash-table :test #'equal)))

(defmethod wish-variable (name (wish nodgui-connection))
  (gethash name (wish-variables wish)))

(defmethod (setf wish-variable) (val name (wish nodgui-connection))
  (setf (gethash name (wish-variables wish)) val))

(defmacro with-nodgui-handlers (() &body body)
  `(funcall (wish-call-with-condition-handlers-function *wish*)
            (lambda () ,@body)))

;;; global connection information

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf
   (documentation 'make-nodgui-connection 'function)
   "Create a new NODGUI-CONNECTION object.  This represents a connection to a
    specific wish.  You can maintain connections to several distinct wish
    processes by binding *WISH* to the one you desire to communicate with, and
    using NODGUI functions within that dynamic scope."))

(defvar *wish* (make-nodgui-connection)
  "The current connection to an inferior wish.")

(defvar *wish-connections* ()
  "Connections pushed aside by invoking the NEW-WISH restart in START-WISH.")

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to stdout
(defvar *debug-tk* nil)

;; if set to t, nodgui will report the buffer size sent to tk
(defvar *debug-buffers* nil)

(defvar *trace-tk* nil)

(defvar *wish-pathname*
  #+freebsd "wish8.6"
  #-freebsd "wish")

(defparameter *default-toplevel-name* "NODGUI")

(defvar *wish-args* '())

(defun append-wish-args (other-args)
  (append *wish-args* other-args))

(defvar *init-wish-hook* nil)

(defparameter *buffer-for-atomic-output* nil)

(defun dbg (fmt &rest args)
  (when *debug-tk*
    (apply #'format *trace-output* fmt args)
    (finish-output *trace-output*)))

(defmacro with-atomic (&rest code)
  `(let ((*buffer-for-atomic-output* t))
     ,@code
     (flush-wish)))

(defmacro send-lazy (&rest code)
  `(let ((*buffer-for-atomic-output* t))
     ,@code))

(defmacro with-lazy (&rest code)
  `(let ((*buffer-for-atomic-output* t))
     ,@code))

(defun require-tcl-package (name)
  (format-wish (tcl-str (:if ([catch {package require ~a} err ])
                             ("tk_messageBox" \++
                                              -icon    error
                                              -type    ok
                                              -message $err%)))
               name))

(defun try-to-load-tcl-package (name)
  "Return non nil if the package has been successfully loaded"
  (format-wish (tclize `(senddata [ catch { package require ,name } ])))
  (tcl-error->boolean (read-data)))

(defparameter *tkimg-loaded-p* nil)

(defun init-tkimg ()
  (setf *tkimg-loaded-p* (try-to-load-tcl-package "Img")))

;;; setup of wish
;;; put any tcl function definitions needed for running nodgui here
(defun init-wish ()
  (send-lazy
   (send-wish "package require Tk")
   (flush-wish)
   (send-wish (wish-init-code))
   (init-tkimg)
   (dolist (fun *init-wish-hook*) ; run init hook functions
     (funcall fun))))

(defun init-tcl (&key debug-tcl)
  (let ((translation "lf"))
    #+(and (or windows win32) (not sbcl)) (setf translation "crlf")
    (format (wish-stream *wish*) (tcl-init-code) debug-tcl translation)))

(defun make-error-collecting-thread (error-stream)
  (bt:make-thread (lambda ()
                    (ignore-errors
                      (let ((ch (read-char error-stream)))
                        (unread-char ch error-stream)
                        (error (make-condition 'tk-communication-error
                                               :message (read-line error-stream))))))
                  :name "nodgui collecting thread"))
;;; start wish and set (wish-stream *wish*)
(defun start-wish (&rest keys &key debugger-class stream debug-tcl)
  ;; open subprocess
  (if (null (wish-stream *wish*))
      (progn
        (if stream
            (setf (wish-stream *wish*) stream)
            (multiple-value-bind (proc-stream proc)
                (do-execute *wish-pathname* *wish-args*)
              (setf (wish-stream *wish*) proc-stream)
              (let ((error-stream (uiop:process-info-error-output proc)))
                (setf (wish-error-collecting-thread *wish*)
                      (make-error-collecting-thread error-stream)))))
        #+(or mswindows windows win32) (sleep 1)
        (setf (wish-call-with-condition-handlers-function *wish*)
              (make-call-with-condition-handlers-function debugger-class))
        ;; perform tcl initialisations
        (with-nodgui-handlers ()
          (init-tcl :debug-tcl debug-tcl)
          (prog1 (init-wish)
            (ensure-timer))))
      ;; By default, we don't automatically create a new connection, because the
      ;; user may have simply been careless and doesn't want to push the old
      ;; connection aside.  The NEW-WISH restart makes it easy to start another.
      (restart-case (nodgui-error "There is already an inferior wish.")
        (new-wish ()
          :report "Create an additional inferior wish."
          (push *wish* *wish-connections*)
          (setf *wish* (make-nodgui-connection))
          (apply #'start-wish keys)))))

;;; CMUCL, SCL, and SBCL, use a two-way-stream and the constituent
;;; streams need to be closed.
(defun close-process-stream (stream)
  "Close a 'stream open by 'do-execute."
  (when *debug-tk*
    (format t "Closing wish stream: ~S~%" stream))
  (ignore-errors (close stream))
  #+(or :cmu :scl :sbcl)
  (when (typep stream 'two-way-stream)
    (close (two-way-stream-input-stream stream) :abort t)
    (close (two-way-stream-output-stream stream) :abort t))
  nil)

(defun exit-wish ()
  (with-nodgui-handlers ()
    (let ((stream (wish-stream *wish*)))
      (when stream
        (when (open-stream-p stream)
          (ignore-errors
            (send-wish "exit")
            (flush-wish)
            (bt:destroy-thread (wish-error-collecting-thread *wish*))
            (close-process-stream stream))))
      (setf (wish-stream *wish*) nil)
      #+:allegro (system:reap-os-subprocess)
      (setf *wish-connections* (remove *wish* *wish-connections*))))
  (throw *wish* nil))

(defun send-wish (text)
  (bt:with-recursive-lock-held ((wish-lock *wish*))
    (push text (wish-output-buffer *wish*))
    (unless *buffer-for-atomic-output*
      (flush-wish))))

(defun send-wish-line (data)
  "Send data  to wish  shell. data  are not processed  so thy  must be
coupled with a 'gets' from the TCL side, for example.

Note also that this function  blocks the communication until wish read
the data (see the TCL proc: 'callbacks_validatecommand' in tcl-glue-code.lisp)"
  (bt:with-recursive-lock-held ((wish-lock *wish*))
    (let ((*print-pretty* nil)
          (stream         (wish-stream *wish*))
          (line           (format nil "~a~%" data)))
      (when *debug-tk*
        (dbg "sending line: ~s~%" data))
      (format stream line)
      (finish-output stream))))

;; maximum line length sent over Tk
(defparameter *max-line-length* 1000)

(defun flush-wish ()
  (bt:with-recursive-lock-held ((wish-flush-lock *wish*))
    (let ((buffer (nreverse (wish-output-buffer *wish*))))
      (when buffer
        (let ((len (loop for s in buffer summing (length s)))
              (*print-pretty* nil)
              (stream (wish-stream *wish*)))
          (declare (stream stream))
          (incf len (length buffer))
          (setf (wish-output-buffer *wish*) nil)
          (handler-bind ((stream-error (lambda (e) (handle-dead-stream e stream)))
                         #+lispworks
                         (comm:socket-error (lambda (e) (handle-dead-stream e stream))))
            (cond
              (*max-line-length*
               (when (or *debug-buffers*
                         *debug-tk*)
                 (format t "buffer size ~a~%" len) (finish-output))
               (dolist (string buffer)
                 (loop while (> (length string) *max-line-length*)
                    do
                      (let ((sub (subseq string 0 *max-line-length*)))
                        (setf string (subseq string *max-line-length*))
                        (format stream "bt \"~A\"~%" (tkescape2 sub))
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
            #+nil(loop for string in buffer
                    do (loop with end = (length string)
                          with start = 0
                          for amount = (min 1024 (- end start))
                          while (< start end)
                          do (let ((string (subseq string start (+ start amount))))
                               (format stream "buffer_text {~A}~%" string)
                               (dbg "buffer_text {~A}~%" string)
                               (incf start amount)))
                      (format stream "buffer_text \"\\n\"~%")
                      (dbg "buffer_text \"\\n\"~%")
                    finally (progn (format stream "process_buffer~%")
                                   (dbg "process_buffer~%")
                                   (finish-output stream)))
            (setf (wish-output-buffer *wish*) nil)))))))

(defun handle-dead-stream (err stream)
  (when *debug-tk*
    (format *trace-output* "Error sending command to wish: ~A" err)
    (finish-output))
  (ignore-errors (close stream))
  (exit-wish))

(defun format-wish (control &rest args)
  "format 'args using 'control as control string to wish"
  (send-wish (apply #'format nil control (mapcar #'sanitize args))))

#+nil
(defmacro format-wish (control &rest args)
  "format 'args using 'control as control string to wish"
  (let ((stream (gensym)))
    `(progn
       (when *debug-tk*
         (format *trace-output* ,control ,@args)
         (format *trace-output* "~%")
         (finish-output))
       (let ((*print-pretty* nil)
             (,stream (wish-stream *wish*)))
         (declare (type stream ,stream)
                  (optimize (speed 3)))

         (format ,stream ,control ,@args)
         (format ,stream "~%")
         (finish-output ,stream))
       nil)))

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
  (bt:with-recursive-lock-held ((wish-read-lock *wish*))
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

(defparameter *accept-garbage-as-event-p* nil
  "Sometimes tcl  lib print on  standard error for  debugging purpose,
  ignore the data got in this case setting this variable to non nil")

(defun verify-event (event)
  (cond
    ((and *accept-garbage-as-event-p*
          (not (listp event)))
     nil)
    ((not (listp event))
     (error "When reading from tcl, expected a list but instead got ~S" event))
    ((eq (first event) :error)
     (error 'tk-error :message (normalize-error event)))
    (t event)))

(defvar *in-read-event* ()
  "A list of nodgui-connection objects that are currently waiting to read an event.")

(defun ping-all-wishes ()
  (dolist (*wish* *in-read-event*)
    (format-wish "keepalive")))

(defvar *nodgui-ping-timer* nil)

(defvar *ping-interval-seconds* nil)

(defun ensure-timer ()
  (unless *nodgui-ping-timer*
    (when *ping-interval-seconds*
      #+sbcl
      (let ((timer (sb-ext:make-timer (lambda () (ping-all-wishes))
                               :name "Nodgui ping timer")))
        (sb-ext:schedule-timer timer *ping-interval-seconds*
                        :repeat-interval *ping-interval-seconds*
                        :absolute-p nil)
        (setf *nodgui-ping-timer* timer))
      #+(not sbcl)
      nil)))

(defun tcldebug (something)
  (format t "tcl debug: ~a~%" something)
  (finish-output))

(defun read-event (&key (blocking t) (no-event-value nil) (force-read-from-stream nil))
  "read the next event from wish, return the event or nil, if there is no
event to read and blocking is set to nil"
  (handler-case
      (let ((wstream (wish-stream *wish*)))
        (flush-wish)
        (bt:with-recursive-lock-held ((wish-read-lock *wish*))
          (let ((event (if (and (not force-read-from-stream)
                                (check-enqueued-event))
                           (pop-enqueued-event)
                           (and (or blocking (stream-readable-p wstream))
                                (read-preserving-whitespace wstream
                                                            t
                                                            nil)))))
            (if event
                (verify-event
                 (let ((*in-read-event* (cons *wish* *in-read-event*)))
                   event))
                no-event-value))))
    (error (e)
      (list :read-stream-error e))))

(defun event-got-error-p (event)
  (eq (first event) :read-stream-error))

(defun check-enqueued-data ()
  (bt:with-recursive-lock-held ((wish-read-data-lock *wish*))
    (wish-data-queue *wish*)))

(defun pop-enqueued-data ()
  (bt:with-recursive-lock-held ((wish-read-data-lock *wish*))
    (pop (wish-data-queue *wish*))))

(defun push-enqueued-data (data)
  (bt:with-recursive-lock-held ((wish-read-data-lock *wish*))
    (let ((new-queue (append (wish-data-queue *wish*) (list data))))
      (setf (wish-data-queue *wish*)
            new-queue))))

(defun check-enqueued-event ()
  (bt:with-recursive-lock-held ((wish-read-event-lock *wish*))
    (wish-event-queue *wish*)))

(defun pop-enqueued-event ()
  (bt:with-recursive-lock-held ((wish-read-event-lock *wish*))
    (pop (wish-event-queue *wish*))))

(defun push-enqueued-event (event)
  (bt:with-recursive-lock-held ((wish-read-event-lock *wish*))
    (let ((new-queue (append (wish-event-queue *wish*) (list event))))
      (setf (wish-event-queue *wish*)
            new-queue))))

(defun read-data (&key (expected-list-as-data nil))
  "Read data from wish. Non-data events are postponed, bogus messages (eg.
error-strings) are ignored."
  (bt:with-recursive-lock-held ((wish-main-iteration-lock *wish*))
    (setf (wish-waiting-data-p *wish*) t)
    (labels ((get-data ()
               (declare (optimize (debug 0) (speed 3)))
               (cond
                 ((check-enqueued-data)
                  (pop-enqueued-data))
                 (t
                  (main-iteration :reentrant? t)
                  (get-data)))))
      (let ((data (get-data)))
        (setf (wish-waiting-data-p *wish*) nil)
        (if (listp data)
            (cond
              ((event-got-error-p data)
               (exit-wish)
               (return-from read-data nil))
              ((null data)
               (exit-wish)
               (return-from read-data nil))
              ((eq (first data) :data)
               (dbg "read-data: ~s~%" data)
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
            data)))))

(defun read-keyword ()
  (let ((string (read-data)))
    (when (> (length string) 0)
      (values (intern #-scl (string-upcase string)
                      #+scl (if (eq ext:*case-mode* :upper)
                                (string-upcase string)
                                (string-downcase string))
                      :keyword)))))
