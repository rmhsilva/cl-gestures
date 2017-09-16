;;;; cl-gestures
;;;; ric@rmhsilva.com

(defpackage :cl-gestures
  (:use :cl :cffi)
  (:export
   ;; :handle-gesture
   :callback-for-mtif
   :with-gestures
   :defgesture
   :*gestures*

   ;; Gesture events
   :on-start
   :on-update
   :on-stop

   ;; Gestures
   :zoom
   :scroll-x
   :scroll-y

   ;; Gesture slot accessors
   :scroll-distance
   ))

