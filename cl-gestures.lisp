;;;; cl-gestures
;;;; ric@rmhsilva.com
;;;;
;;;; Gesture backend and API

(in-package :cl-gestures)

(defvar *gestures* nil
  "List of gestures to handle in the MTIF callback")


(defclass gesture ()
  ((started :reader started-p
            :initform nil
            :documentation "Whether the gesture has been started or not")
   (finger-ids :initform nil
               :documentation "IDs of the fingers currently in the gesture")
   (n-fingers :reader n-fingers
              :initform (error "n-fingers Must be defined")))
  (:documentation "Generic gesture class"))

(defgeneric on-update (inst)
  (:documentation "Called when the gesture is updated")
  (:method ((inst gesture)) nil))

(defgeneric on-start (inst)
  (:documentation "Called when the gesture is started")
  (:method ((inst gesture)) nil))

(defgeneric on-stop (inst)
  (:documentation "Called when the gesture is stopped")
  (:method ((inst gesture)) nil))

(defgeneric update (inst finger-data)
  (:documentation "Update the gesture with the given data")
  (:method ((inst gesture) finger-data)
    (declare (ignore finger-data))
    (on-update inst)))

(defgeneric start (inst finger-data)
  (:documentation "Start gesture (must have enough fingers)")
  (:method ((inst t) finger-data)
    (setf (slot-value inst 'started) t)
    (setf (slot-value inst 'finger-ids)
          (mapcar #'mtif:finger-id finger-data))
    (on-start inst)))

(defgeneric stop (inst)
  (:documentation "Stop the gesture")
  (:method ((inst t))
    (setf (slot-value inst 'finger-ids) nil)
    (setf (slot-value inst 'started) nil)
    (on-stop inst)))


(defmacro defgesture (name doc &key n-fingers slots start update)
  "Helper macro to define simple gestures easily"
  `(progn
     (defclass ,name (gesture)
       ((n-fingers :initform ,n-fingers)
        ,@slots)
       (:documentation ,doc))
     (defmethod start :after ((inst ,name) finger-data)
       (with-slots ,(mapcar #'car slots) inst
         (apply ,start finger-data)))
     (defmethod update :before ((inst ,name) finger-data)
       (with-slots ,(mapcar #'car slots) inst
         (apply ,update finger-data)))))


;;
;; Helpers

(defun filter-fingers (ids finger-data)
  "Return the fingers with ids a deterministic order"
  (sort (loop for f in finger-data
              when (member (mtif:finger-id f) ids)
                collect f)
        #'<
        :key #'mtif:finger-id))


(defun ready-to-start (gesture finger-data)
  "Return T if there are enough fingers on the trackpad to start `gesture'"
  (>= (length finger-data) (n-fingers gesture)))


(defun callback-for-mtif (finger-data timestamp frame)
  (declare (ignore timestamp frame))
  ;; We need to keep track of fingers by ID, so that we always call gestures
  ;; with the same fingers. The ID stays the same, but they move around in the
  ;; finger-data array.
  (dolist (gesture *gestures*)
    (check-type gesture gesture)
    (if (started-p gesture)
        (let* ((gesture-finger-ids
                 (slot-value gesture 'finger-ids))
               (gesture-fingers
                 (filter-fingers gesture-finger-ids finger-data)))
          (if (= (length gesture-finger-ids) (length gesture-fingers))
              (update gesture gesture-fingers)
              (stop gesture)))
        (when (ready-to-start gesture finger-data)
          (start gesture (subseq finger-data 0 (n-fingers gesture)))))))


(defmacro with-gestures (instances &body body)
  "Run `body' with the specified gestures hooked in"
  `(let ((*gestures* ,instances))
     (mtif:start #'callback-for-mtif)
     ,@body))
