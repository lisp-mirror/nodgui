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

(in-package :nodgui.sanitize)

(defun escape-~ (text)
  (when text
    (let ((res  (make-adjustable-string))
          (skip nil))
      (if (and (= (length text) 1)
               (char= (first-elt text) #\~))
          (setf res "~~")
          (loop
             for pos from 0 by 1
             for current-char across text  do
               (vector-push-extend current-char res)
               (if skip
                   (setf skip nil)
                   (when (char= #\~ current-char)
                     (if (or (= pos (1- (length text)))
                             (not (char= #\~ (elt text (1+ pos)))))
                         (vector-push-extend #\~ res)
                         (setf skip t))))))
      res)))

(defun %tkescape (text escapable-chars)
  (unless (stringp text)
    (setf text (format nil "~a" text)))
  (loop with result = (make-adjustable-string)
     for c across text do
       (when (member c escapable-chars)
         (vector-push-extend #\\ result))
       (vector-push-extend c result)
     finally (return result)))

;; Much faster version. For one test run it takes 2 seconds, where the
;; other implementation requires 38 minutes.
(defun tkescape (text)
  (%tkescape text '(#\\ #\$ #\[ #\] #\{ #\} #\")))

(defun tkescape2 (text)
  (%tkescape text '(#\\ #\$ #\[ #\] #\")))

;;; sanitizing strings: lisp -> tcl (format (wish-stream *wish*) "{~a}" string)
;;; in string escaped : {} mit \{ bzw \}  und \ mit \\

(defstruct protect-escape (data))

(defstruct (bypass-escape (:include protect-escape)))

(defun rem-trouble-chars-and-then-wrap (s)
  (make-protect-escape :data (sanitize-remove s)))

(defgeneric sanitize (object))

(defmethod sanitize (object)
  (sanitize (to-s object)))

(defmethod sanitize ((object string))
  (if (string= "{}" object)
      object
      (tkescape object)))

(defmethod sanitize ((object list))
  (map 'list #'sanitize object))

(defmethod sanitize ((object protect-escape))
  (format nil "{~a}" (protect-escape-data object)))

(defmethod sanitize ((object bypass-escape))
  (bypass-escape-data object))

(defmethod sanitize ((object (eql nil)))
  nil)

(defgeneric sanitize-remove (object))

(defmethod sanitize-remove ((object (eql nil)))
  nil)

(defmethod sanitize-remove ((object string))
  (cl-ppcre:regex-replace-all "(\")|(\\[)|(\\])" object ""))

(defmethod sanitize-remove ((object list))
  (map 'list #'sanitize-remove object))
