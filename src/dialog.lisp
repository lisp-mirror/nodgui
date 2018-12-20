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

(defun choose-color (&key parent title initialcolor )
  (format-wish "senddatastring [tk_chooseColor ~@[ -parent ~A~]~@[ -title {~A}~]~@[ -initialcolor {~A}~]]" (when parent (widget-path parent)) title initialcolor)
  (read-data))

(defun get-open-file (&key
                        (filetypes '(("All Files" "*")))
                        (initialdir "")
                        multiple parent title)
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (if multiple
        (format-wish "senddatastrings [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~] -multiple 1 ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
                      files initialdir
                      (and parent (widget-path parent)) title)
        (format-wish "senddatastring [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~]  ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
                      files initialdir
                      (and parent (widget-path parent)) title))
    (read-data)))

(defun get-save-file (&key
                        (filetypes '(("All Files" "*")))
                        (title      "")
                        (parent     nil)
                        (initialdir ""))
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (format-wish (tclize `(senddatastring ["tk_getSaveFile "
                                          -filetypes  ,files " "
                                          -title      {+ ,title } " "
                                          -parent     ,(if parent
                                                           (widget-path parent)
                                                           (widget-path *tk*)) " "
                                          -initialdir {+ ,initialdir }])))
    (read-data)))

(defun choose-directory (&key (initialdir "")
                              parent title mustexist)
  (format-wish "senddatastring [tk_chooseDirectory ~@[ -initialdir \"~a\"~]~@[ -parent ~a ~]~@[ -title {~a}~]~@[ -mustexist ~a~]]" (tkescape2 initialdir) (and parent (widget-path parent)) title (and mustexist 1))
  (read-data))
