;;;; cl-gestures
;;;; ric@rmhsilva.com
;;;;
;;;; Gesture utilities

(in-package :cl-gestures)


(defun finger-separation (f1 f2)
  "Return a measure of the distance between `f1' and `f2'"
  ;; No sqrt because why bother?
  (+ (expt (- (mtif:finger-pos-x f1) (mtif:finger-pos-x f2))
           2)
     (expt (- (mtif:finger-pos-y f1) (mtif:finger-pos-y f2))
           2)))
