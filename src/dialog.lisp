;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui)

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(defun choose-color (&key parent title initial-color )
  (format-wish "senddatastring [tk_chooseColor ~@[ -parent ~A~]~@[ -title {~A}~]~@[ -initialcolor {~A}~]]"
               (when parent
                 (widget-path parent))
               title
               initial-color)
  (read-data))

(defun get-open-file (&key
                        (initial-file nil)
                        (file-types    '(("All Files" "*")))
                        (initial-dir   "")
                        multiple parent title)
  (let ((files (with-output-to-string (s)
                 (dolist (type file-types)
                   (let ((name (first type))
                         (wildcard (second type)))
                     (format s "{{~a} {~a}} " name wildcard))))))
    (if multiple
        (format-wish "senddatastrings [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~] -multiple 1 ~
                      ~@[ -initialfile \"~a\"~] ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
                     (rem-trouble-chars-and-then-wrap files)
                     initial-dir
                     initial-file
                     (and parent (widget-path parent)) title)
        (format-wish "senddatastring [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~]  ~
                      ~@[ -initialfile \"~a\"~] ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
                     (rem-trouble-chars-and-then-wrap files)
                     initial-dir
                     initial-file
                     (and parent (widget-path parent)) title))
    (read-data)))

(defun get-save-file (&key
                        (initial-file nil)
                        (file-types '(("All Files" "*")))
                        (title      "")
                        (parent     nil)
                        (initial-dir nil))
  (let ((files (with-output-to-string (s)
                 (dolist (type file-types)
                   (let ((name (first type))
                         (wildcard (second type)))
                     (format s "{{~a} {~a}} " name wildcard)))))
        (*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(senddatastring ["tk_getSaveFile "
                                          -filetypes  ,(rem-trouble-chars-and-then-wrap files) " "
                                          -title      \"+ ,title  \"
                                          -parent     ,(if parent
                                                           (widget-path parent)
                                                           (widget-path *tk*)) " "
                                          ,(empty-string-if-nil initial-file
                                               `(-initialfile  \"+ ,initial-file \" " "))
                                          ,(empty-string-if-nil initial-dir
                                               `(-initialdir \"+ ,initial-dir \"))
                                          ])))
    (read-data)))

(defun choose-directory (&key (initial-dir nil)
                              parent title mustexist)
  (format-wish "senddatastring [tk_chooseDirectory ~@[ -initialdir \"~a\"~]~@[ -parent ~a ~]~@[ -title {~a}~]~@[ -mustexist ~a~]]"
               initial-dir
               (and parent (widget-path parent))
               title
               (and mustexist 1))
  (read-data))
