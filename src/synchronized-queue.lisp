(in-package :syncronized-queue)

(defclass synchronized-queue ()
  ((container
    :initform '()
    :initarg :container
    :accessor container)
   (lock
    :initform (bt:make-lock)
    :initarg :lock
    :accessor lock)
   (condition-variable
    :initform (bt:make-condition-variable)
    :initarg :condition-variable
    :accessor condition-variable)))

(defgeneric pop-block (object))

(defgeneric push-unblock (object value))

(defgeneric emptyp (object))

(defmethod pop-block ((object synchronized-queue))
  (with-accessors ((lock               lock)
                   (condition-variable condition-variable)
                   (container          container)) object
  (bt:with-lock-held (lock)
    (loop while (null container)
          do
             (bt:condition-wait condition-variable lock))
    (prog1
        (a:last-elt container)
      (setf container (subseq container 0 (1- (length container))))))))

(defmethod push-unblock ((object synchronized-queue) value)
  (with-accessors ((lock               lock)
                   (condition-variable condition-variable)
                   (container          container)) object
    (bt:with-lock-held (lock)
      (let ((reverse-container (nreverse container)))
        (push value reverse-container)
        (setf container (nreverse reverse-container))
        (bt:condition-notify condition-variable)))))

(defmethod emptyp ((object synchronized-queue))
  (with-accessors ((lock      lock)
                   (container container)) object
    (bt:with-lock-held (lock)
      (null container))))

(defparameter *stop-events-loop* t)

(defparameter *events-loop-lock* (bt:make-lock "events-loop-lock"))

(defparameter *events-loop-thread* nil)

(defparameter *queue* (make-instance 'synchronized-queue))

(defun events-loop-running-p ()
  (bt:with-lock-held (*events-loop-lock*)
    (not *stop-events-loop*)))

(defun stop-events-loop ()
  (bt:with-lock-held (*events-loop-lock*)
    (setf *stop-events-loop* t)))

(defun start-events-loop ()
  (bt:with-lock-held (*events-loop-lock*)
    (format t "queue ~a~%" *queue*)
    (setf *stop-events-loop* nil))
  (setf *events-loop-thread*
        (bt:make-thread (lambda ()
                          (format  t "start~%")
                          (loop while (events-loop-running-p) do
                            (dispatch-program-events-or-wait))
                          (format  t "end~%")))))

(defun dispatch-program-events-or-wait ()
  (let ((event (pop-block *queue*)))
    (funcall event)))

(defun demo-test-loop ()
  (start-events-loop)
  (push-unblock *queue*
                (lambda ()
                  (format t "event!~%")
                  (finish-output)))
  (sleep 1)                             ; simulate user interation
  (stop-events-loop)
  (push-unblock *queue*
                (lambda ()
                  (format t "dummy~%")
                  (finish-output))))
