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

(defargs classic-frame ()
  borderwidth
  class
  relief
  background
  colormap
  container
  cursor
  height
  highlightbackground
  highlightcolor
  highlightthickness
  padx
  pady
  takefocus
  visual
  width)

(defargs frame ()
  borderwidth
  class
  relief
  cursor
  height
  padding
  takefocus
  width)

(defwrapper classic-frame (widget) () "frame")

(defwrapper frame (widget) () "ttk::frame")

;(defun make-frame (master)
;  (make-instance 'frame :master master))
