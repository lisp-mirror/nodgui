;; This software is Copyright © cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License
;; (https://web.archive.org/web/20241223191336/http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.thread-pool)

(defclass with-queue ()
  ((queue
    :initform (make-instance 'q:synchronized-queue)
    :initarg  :queue
    :accessor queue)))

(defclass pool (with-queue)
  ((workers
    :initform '()
    :initarg  :workers
    :accessor workers)
   (tasks
    :initform (make-instance 'q:synchronized-queue)
    :initarg  :tasks
    :accessor tasks)
   (results
    :initform (make-instance 'q:synchronized-queue)
    :initarg  :results
    :accessor results)))

(defclass worker (with-queue)
  ((pool
    :initform nil
    :initarg  :pool
    :accessor pool)))

(defparameter *log-lock* (u:make-lock))

(defun do-log (&rest args)
  (u:with-lock-held (*log-lock*)
    (apply #'format (append (list t) args))))

(defun start-worker (worker)
  (u:make-thread (lambda ()
                   (loop while t
                         do
                            (let* ((task   (q:pop-block (queue (pool worker))))
                                   (result (funcall task)))
                              (q:push-unblock (results (pool worker)) result)))))
  worker)

(defmethod initialize-instance :after ((object worker) &key &allow-other-keys)
  (start-worker object))

(defun receive-result (pool)
  (q:pop-block (results pool)))

(defun make-thread-pool (workers-number)
  (let ((pool (make-instance 'pool)))
    (setf (workers pool)
          (loop repeat workers-number
                collect (make-instance 'worker :pool pool)))
    pool))

(defun submit-task (pool task)
  (q:push-unblock (queue pool) task))

(defun fib (i &optional (acc1 0) (acc2 1))
  (declare (optimize (debug 0) (speed 3)))
  (if (= i 0)
      acc1
      (fib (1- i) acc2 (+ acc1 acc2))))

(defun %test ()
  (let ((pool (make-thread-pool 12)))
    (with-accessors ((workers workers)) pool
      (time
       (progn
         (loop repeat 23
               do (submit-task pool (lambda () (fib 490000))))
         (loop repeat 23 do (receive-result pool)))))))
