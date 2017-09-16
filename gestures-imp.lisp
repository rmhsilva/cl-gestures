;;;; cl-gestures
;;;; ric@rmhsilva.com
;;;;
;;;; Implement some gestures!

(in-package :cl-gestures)


(defclass zoom (gesture)
  ((initial-separation :initform 0)
   (zoom :initform 1)
   (n-fingers :initform 2))
  (:documentation "Two finger zoom"))

(defmethod start :after ((inst zoom) finger-data)
  (destructuring-bind (f1 f2 &rest rest) finger-data
    (declare (ignore rest))
    (with-slots (initial-separation) inst
      (setf initial-separation (finger-separation f1 f2)))))

(defmethod update :before ((inst zoom) finger-data)
  (destructuring-bind (f1 f2 &rest rest) finger-data
    (declare (ignore rest))
    (with-slots (zoom initial-separation) inst
      (setf zoom (/ (finger-separation f1 f2)
                    initial-separation)))))


(defclass scroll-x (gesture)
  ((initial-x1 :initform 0)
   (initial-x2 :initform 0)
   (scroll-distance :initform 0)
   (n-fingers :initform 2))
  (:documentation "Two finger scroll horizontal"))

(defmethod start :after ((inst scroll-x) finger-data)
  (destructuring-bind (f1 f2 &rest rest) finger-data
    (declare (ignore rest))
    (with-slots (initial-x1 initial-x2) inst
      (setf initial-x1 (mtif:finger-pos-x f1))
      (setf initial-x2 (mtif:finger-pos-x f2)))))

(defmethod update :before ((inst scroll-x) finger-data)
  (destructuring-bind (f1 f2 &rest rest) finger-data
    (declare (ignore rest))
    (with-slots (initial-x1 initial-x2 scroll-distance) inst
      (setf scroll-distance
            (let ((a (- (mtif:finger-pos-x f1) initial-x1))
                  (b (- (mtif:finger-pos-x f2) initial-x2)))
              (+ a b))))))


(defgesture scroll-y
  "Two finger vertical scroll"
  :n-fingers 2
  :slots ((initial-y1 :initform 0)
          (initial-y2 :initform 0)
          (scroll-distance :initform 0))
  :start (lambda (f1 f2)
           (setf initial-y1 (mtif:finger-pos-y f1))
           (setf initial-y2 (mtif:finger-pos-y f2)))
  :update (lambda (f1 f2)
            (setf scroll-distance
                  (+ (- (mtif:finger-pos-y f1) initial-y1)
                     (- (mtif:finger-pos-y f2) initial-y2)))))
