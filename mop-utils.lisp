(in-package :prototype)

(defun find-slot (class slot)
  (when (typep slot 'slot-definition)
    (return-from find-slot slot))
  (find-if (lambda (slot-def) 
             (eq (slot-definition-name slot-def) slot))
           (class-slots class)))

(defun ensure-slot-symbol (slotd)
  (typecase slotd
    (symbol slotd)
    (slot-definition (slot-definition-name slotd))))

(defun get-direct-slot (object slot)
  (standard-instance-access object (slot-definition-location (find-slot (class-of object) slot))))

(defun (setf set-direct-slot) (value object slot)
  (setf (standard-instance-access object
                                  (slot-definition-location (find-slot (class-of object) slot)))
        value))

(defun slot-direct-value (object slotd)
  (if (find-slot (class-of object) slotd)
      (get-direct-slot object slotd)
    (gethash (ensure-slot-symbol slotd) (get-direct-slot object 'hash))))

(defun (setf slot-direct-value) (value object slotd)
  (if (find-slot (class-of object) slotd)
      (setf (set-direct-slot object slotd) value)
    (setf (gethash (ensure-slot-symbol slotd) (get-direct-slot object 'hash))
          value)))