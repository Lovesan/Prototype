(in-package #:prototype)

;;Internal
(defparameter *direct-access* t)

(defclass prototype-object-ext (prototype-object)
  ((fn-no-inherit :initform nil :initarg :fn-no-inherit :allocation :instance))
  (:metaclass prototype-class)
  (:documentation "Extend base class for all prototype objects"))
;(setf tmp (make-instance 'prototype-object-ext))

(defgeneric fn-no-inherit (object)
  (:documentation "Obtain the predicate to skip values when they are searched into prototypes"))

(defgeneric (setf fn-no-inherit) (value object)
  (:documentation "Setting the predicate to skip values when they are searched into prototypes"))

(defmethod fn-no-inherit ((object prototype-object-ext))
  (slot-direct-value object 'fn-no-inherit))

(defmethod (setf fn-no-inherit) (value (object prototype-object-ext))
  (setf (slot-direct-value object 'fn-no-inherit) value))

(defmethod slot-value-using-class :around ((class prototype-class) (object prototype-object-ext) slotd &aux value cur-direct-access)
  (if (member (ensure-slot-symbol slotd) '(hash prototype))
      (call-next-method)
    (progn
      (setf cur-direct-access *direct-access*)
      (let ((*direct-access* nil))
        (if cur-direct-access
            (call-next-method)
          (if (not (fn-no-inherit object))
              (call-next-method)
            (progn
              (setf value (slot-direct-value object slotd))
              (if (funcall (fn-no-inherit object) value)
                  (let ((proto (prototype-of object)))
                    (when proto
                      (slot-value-using-class class proto slotd)))
                (call-next-method)))))))))

