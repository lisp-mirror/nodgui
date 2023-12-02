(in-package :nodgui.non-blocking-queue)

(defstruct (queue-container (:print-function print-queue))
  (data)
  (next)
  (last))

(defun container-push (queue val)
  (let ((node (make-queue-container :data val :next nil)))
    (setf (queue-container-next (queue-container-last queue)) node)
    (setf (queue-container-last queue) node)
    (setf (queue-container-next node) nil))
  queue)

(defun container-empty-p (queue)
  (null queue))

(defun container-pop (queue)
  (when (not (container-empty-p queue))
    (let ((res (queue-container-data queue)))
      (when (queue-container-next queue)
        (setf (queue-container-last (queue-container-next queue))
              (queue-container-last queue)))
      (values res (queue-container-next queue)))))

(defun initialize-queue-container (data)
  (let ((queue (make-queue-container :data data)))
    (setf (queue-container-next queue) queue)
    (setf (queue-container-last queue) queue)
    queue))

(defparameter *printing-depth* 1)

(defun print-queue (queue stream depth)
  (when queue
    (print-unreadable-object (queue stream :type t :identity t)
      (if (< depth *printing-depth*)
          (format stream
                  "data: ~a next ~a last ~a"
                  (queue-container-data queue)
                  (with-output-to-string (string-stream)
                    (print-queue (queue-container-next queue) string-stream (1+ depth)))
                  (with-output-to-string (string-stream)
                    (print-unreadable-object ((queue-container-last queue)
                                              string-stream
                                              :type t
                                              :identity t))))
          (format stream "data: ~a next ~:[nil~;...~]"
                  (queue-container-data queue)
                  (queue-container-next queue))))))

(defclass queue ()
  ((container
    :initform nil
    :accessor container)
   (lock
    :initform (make-lock)
    :reader lock)))

(defmethod print-object ((object queue) stream)
  (format stream "~a" (container object)))

(defmethod push ((object queue) data)
  (with-accessors ((container container)
                   (lock      lock)) object
    (with-lock-held (lock)
      (if (null container)
          (setf container (initialize-queue-container data))
          (container-push container data))
      object)))

(defmethod emptyp ((object queue))
  (with-accessors ((container container)
                   (lock      lock)) object
    (with-lock-held (lock)
      (container-empty-p container))))

(defmethod pop ((object queue))
  (with-accessors ((container container)
                   (lock      lock)) object
    (with-lock-held (lock)
      (multiple-value-bind (data rest-container)
          (container-pop container)
        (setf container rest-container)
        data))))

(defun make-queue ()
  (make-instance 'queue))
