(in-package :test-non-blocking-queue)

(defsuite non-blocking-queue-suite (all-suite))

(defun fill-queue-collect-all ()
    (let ((queue (make-queue)))
      (loop for i from 0 below 19 do (push queue i))
      (push queue :a)
      (push queue :b)
      (push queue :c)
      (loop repeat 30 collect (pop queue))))

(defun fill-queue-empty-fill-again ()
  (let ((queue (make-queue)))
    (loop for i from 0 below 2 do (push queue i))
    (loop until (emptyp queue) do (pop queue))
    (push queue :a)
    (push queue :b)
    (push queue :c)
    (loop repeat 30 collect (pop queue))))

(defun fill-queue-1-element-and-empty ()
  (let ((queue (make-queue)))
    (push queue :a)
    (loop repeat 3 collect (pop queue))))

(defun overfill-than-empty-queue ()
  (let ((queue (make-queue :maximum-size 5)))
    (loop for i from 0 below 10 do (push queue i))
    (values (loop repeat 10 collect (pop queue))
            queue)))

(defun overfilled-queue ()
  (let ((queue (make-queue :maximum-size 5)))
    (loop for i from 0 below 10 do (push queue i))
    queue))

(deftest filling-and-collect (non-blocking-queue-suite)
  (assert-equality #'equalp
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 :A :B :C NIL NIL NIL NIL NIL NIL NIL NIL)
      (fill-queue-collect-all))
  (assert-equality #'equalp
      '(:A :B :C NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL)
      (fill-queue-empty-fill-again))
  (assert-equality #'equalp
      '(:A NIL NIL)
      (fill-queue-1-element-and-empty)))

(deftest overfill-tests (non-blocking-queue-suite)
  (assert-equality #'equalp
      '(0 1 2 3 4 nil nil nil nil nil)
      (nth-value 0 (overfill-than-empty-queue)))
  (let ((empty-queue (nth-value 1 (overfill-than-empty-queue))))
    (assert-equality #'= 0 (nodgui.non-blocking-queue::container-count empty-queue)))
  (let ((overfilled-queue (overfilled-queue)))
    (assert-equality #'= 5 (nodgui.non-blocking-queue::container-count overfilled-queue))))
