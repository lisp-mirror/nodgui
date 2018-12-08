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

(defargs combobox ()
  class
  cursor
  exportselection
  justify
  height
  postcommand
  state
  style
  takefocus
  textvariable
  values
  width)

(defwrapper combobox (tktextvariable widget) () "ttk::combobox")

(defgeneric (setf options) (value widget))

(defmethod (setf options) (values (combobox combobox))
  (format-wish "~a configure -values {~{ \{~a\}~}}" (widget-path combobox) values))
