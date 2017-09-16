;;;; cl-gestures
;;;; ric@rmhsilva.com
;;;;
;;;; Example of X, Y scroll and zoom

(defpackage :cl-gestures-example-one
  (:use :cl :cl-gestures :sketch)
  (:export :run))

(in-package :cl-gestures-example-one)

(defparameter *gesture-frames* 0
  "Number of frames processed")

(defparameter *scroll-factor* 300.0
  "Trackpad scroll scaling factor")

;; Sketch Window
(defparameter *width* 400)
(defparameter *height* 400)

;; Circle
(defvar *radius* 50)
(defvar *pos-x* (/ *width* 2))
(defvar *pos-y* (/ *height* 2))

;; Helpers
(defun clamp (x min max)
  (cond ((> x max) max)
        ((< x min) min)
        (t x)))

(defun set-radius (r)
  (let ((r (clamp r 10 100)))
    (setf *radius* r)))

(defun set-pos-x (x)
  (let ((x (clamp x *radius* (- *width* *radius*))))
    (setf *pos-x* x)))

(defun set-pos-y (y)
  (let ((y (clamp y *radius* *height*)))
    (setf *pos-y* y)))


;; Debug vars
(defvar *scroll-x* 0)
(defvar *scroll-y* 0)
(defvar *zoom* 0)


;; The gesture 'state' is held in a closure, keeping things tidy

(let ((initial-radius))
  (defmethod on-start ((inst zoom))
    (declare (ignore inst))
    (setf initial-radius *radius*))
  (defmethod on-update ((inst zoom))
    (with-slots (zoom) inst
      (setf *zoom* zoom)
      (set-radius (* initial-radius zoom)))))

(let ((initial-x))
  (defmethod on-start ((inst scroll-x))
    (declare (ignore inst))
    (setf initial-x *pos-x*))
  (defmethod on-update ((inst scroll-x))
    (with-slots (scroll-distance) inst
      (setf *scroll-x* (* scroll-distance *scroll-factor*))
      (set-pos-x (+ *scroll-x* initial-x)))))

(let ((initial-y))
  (defmethod on-start ((inst scroll-y))
    (declare (ignore inst))
    (setf initial-y *pos-y*))
  (defmethod on-update ((inst scroll-y))
    (with-slots (scroll-distance) inst
      (setf *scroll-y* (* -1 scroll-distance *scroll-factor*))
      (set-pos-y (+ *scroll-y* initial-y)))))



(defsketch circle
    ((title "Gestures")
     (width *width*)
     (height *height*))
  (background (gray 0.8))
  (with-font (make-font :color (gray 0.4))
    (text (format nil "frames: ~d" mtif:*frame-count*) 20 20)
    (text (format nil "scroll x: ~2$" *scroll-x*) 20 40)
    (text (format nil "scroll y: ~2$" *scroll-y*) 20 60)
    (text (format nil "zoom: ~2$" *scroll-y*) 20 80)
    (with-pen (make-pen :fill (rgb-255 #x2e #x8b #x57))
      (circle *pos-x* *pos-y* *radius*)
      (text (format nil "(~2$, ~2$) ~2$" *pos-x* *pos-y* *radius*)
            (+ *pos-x* *radius* 5)
            (- *pos-y* 20)))))


(defun start-gestures ()
  (if mtif:*mtif-started* (mtif:stop))
  (setf *gestures* (mapcar #'make-instance '(zoom scroll-y scroll-x)))
  (mtif:start #'callback-for-mtif))


(defun run ()
  (make-instance 'circle)
  (start-gestures))
