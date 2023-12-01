(in-package :test-non-blocking-queue)

(defsuite non-blocking-queue-suite (all-suite))

(defun fill-queue-collect-all ()
  (let ((*container-extend-size* 7))
    (let ((queue (make-queue)))
      (loop for i from 0 below 19 do (push queue i))
      (push queue 'a)
      (push queue 'b)
      (push queue 'c)
      (loop repeat 30 collect (pop queue)))))

(defun fill-queue-empty-fill-again ()
  (let ((*container-extend-size* 7))
    (let ((queue (make-queue)))
      (loop for i from 0 below 2 do (push queue i))
      (loop until (emptyp queue) do (pop queue))
      (push queue 'a)
      (push queue 'b)
      (push queue 'c)
      (loop repeat 30 collect (pop queue)))))

(deftest single-thread-filling-and-collect (non-blocking-queue-suite)
  (assert-equality #'equalp
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 A B C NIL NIL NIL NIL NIL NIL NIL NIL)
      (fill-queue-collect-all))
  (assert-equality #'equalp
      '(A B C NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL)
      (fill-queue-empty-fill-again)))
