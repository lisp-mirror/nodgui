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

(in-package :nodgui.constants)

(a:define-constant +wish-to-lisp-data-reply+      :data     :test #'eq)

(a:define-constant +wish-to-lisp-error-reply+     :error    :test #'eq)

(a:define-constant +wish-to-lisp-callback-reply+  :callback :test #'eq)

(a:define-constant +wish-to-lisp-keepalive-reply+ :keepalive :test #'eq)

(a:define-constant +wish-to-lisp-event-reply+     :event     :test #'eq)

(a:define-constant +2pi+                          (* 2 pi)   :test #'=)
