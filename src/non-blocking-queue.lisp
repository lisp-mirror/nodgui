(in-package :nodgui.non-blocking-queue)

(defparameter *container-extend-size* 8192)

(defun make-container (size &optional (start-copy 0)  (copy-from-vector nil))
  (let ((results (make-array size
                             :element-type    t
                             :initial-element nil
                             :adjustable nil)))
  (when copy-from-vector
    (loop for i fixnum from start-copy below (length copy-from-vector)
          for j from 0
          do
      (setf (aref results j)
            (aref copy-from-vector i))))
    results))

(defclass queue ()
  ((container
    :initform (make-container *container-extend-size*)
    :initarg :container
    :accessor container)
   (index-push
    :initform -1
    :initarg  :index-push
    :accessor index-push)
   (index-pop
    :initform 0
    :initarg  :index-pop
    :accessor index-pop)
   (lock
    :initform (bt:make-lock)
    :reader   lock)))

(defun initialize-indices (queue)
  (setf (index-push queue) -1
        (index-pop  queue)  0))

(defmethod initialize-instance :after ((object queue) &key &allow-other-keys)
  (initialize-indices object)
  object)

(defun push (queue val)
  (bt:with-lock-held ((lock queue))
    (with-accessors ((container       container)
                     (index-push index-push)
                     (index-pop  index-pop)) queue
      (incf index-push)
      (let ((saved-length-container (length container)))
        (when (>= index-push saved-length-container)
          (setf container
                (make-container (+ saved-length-container *container-extend-size*)
                                index-pop
                                container))
          (decf index-push index-pop)
          (setf index-pop 0))
        (setf (aref container index-push) val)))
    queue))

(defun emptyp (queue)
  (> (index-pop queue)
     (index-push queue)))

(defun pop (queue)
  (bt:with-lock-held ((lock queue))
    (if (not (emptyp queue))
      (let ((res (aref (container queue) (index-pop queue))))
        (incf (index-pop queue))
        res)
      (progn
        (initialize-indices queue)
        nil))))

(defun make-queue ()
  (make-instance 'queue))
