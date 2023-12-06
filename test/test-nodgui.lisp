;; This software is Copyright © cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :test-nodgui)

(defsuite nodgui-suite (test-text-indices:text-indices-suite))

(defun regex-search-test ()
  (with-nodgui ()
    (let ((text (make-instance 'text))
          (b    (make-instance 'button :text "exit" :command (lambda () (exit-wish)))))
      (grid text 0 0)
      (grid b 1 0)
      (append-line text "foo bar")
      (multiple-value-list (search-regexp text "bar" (make-indices-start))))))

(deftest regex-search-text (nodgui-suite)
  (assert-equalp '((:line 1 :char 4) (+ (:line 1 :char 4) 3 :chars) nil 1 4 3)
      (regex-search-test)))

(defun modal-window-test ()
  (let ((results nil))
    (flet ((button-callback ()
             (with-modal-toplevel (modal-toplevel)
               (let ((toplevel-widget (modal-toplevel-root-widget modal-toplevel)))
                 (transient toplevel-widget (root-toplevel))
                 (let* ((button (make-instance 'button
                                               :master toplevel-widget
                                               :text "test"
                                               :command
                                               (lambda ()
                                                 (setf (modal-toplevel-results modal-toplevel) 5)
                                                 (exit-from-modal-toplevel modal-toplevel)))))
                   (grid button 0 0))))))
      (with-nodgui (:title "orig")
        (let ((button (make-instance 'button
                                     :text "open modal"
                                     :command (lambda ()
                                                (setf results (button-callback))))))
          (grid button 0 0)))
      results)))

(deftest test-modal-window (nodgui-suite)
  (assert-equalp 5 (modal-window-test)))
