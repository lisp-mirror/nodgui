;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2019 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.fit-line)

(defun fit-line (data errors)
  "Fit  a line  (y =  m *  x +  q) on  data with  errors, the  results
are (values m q sigma-m^2 sigma-q^2 chi^2)."
  (assert (a:length= data errors))
  (flet ((sum (data)
           (loop
              for sigma across errors
              for datum across data
              sum (/ datum (expt sigma 2))))
         (calc-t-i (s s-x sigma x)
           (* (/ 1 sigma)
              (- x (/ s-x s)))))
    (let* ((s         (loop for sigma across errors sum (/ 1 (expt sigma 2))))
           (xs        (map 'vector #'vec2-x data))
           (ys        (map 'vector #'vec2-y data))
           (s-x       (sum xs))
           (s-y       (sum ys))
           (s-tt      (loop
                         for sigma across errors
                         for x     across xs
                         sum
                           (let ((t-i (calc-t-i s s-x sigma x)))
                             (expt t-i 2))))
           (b         (* (/ 1 s-tt)
                         (loop
                            for y     across ys
                            for x     across xs
                            for sigma across errors
                            sum
                              (let ((t-i (calc-t-i s s-x sigma x)))
                                (/ (* y t-i)
                                   sigma)))))
           (a         (/ (- s-y (* s-x b))
                         s))
           (sigma^2-a (/ (+ 1
                            (/ (expt s 2)
                               (* s s-tt)))
                         s))
           (sigma^2-b (/ 1 s-tt))
           (chi^2     (loop
                         for x     across xs
                         for y     across ys
                         for sigma across errors
                         sum
                           (expt (/ (- y a (* b x))
                                    sigma)
                                 2))))
      (values b a sigma^2-b sigma^2-a chi^2))))
