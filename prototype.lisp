;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2014, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:cl-user)

(defpackage #:prototype
  #.(list* :shadowing-import-from
           '#:closer-mop
           (loop :for symbol :being :the :external-symbols :in '#:closer-mop
                 :collect symbol))
  (:use #:cl #:closer-mop)
  (:export
   #:prototype-class
   #:prototype-object
   #:prototype-of
   #:change-prototype
   #:*walk-prototype*
   #:remove-direct-slot
   #:clear-direct-slots))

(in-package #:prototype)

(defparameter *walk-prototype* t)

(defclass prototype-class (standard-class)
  ()
  (:documentation "Metaclass for all prototype classes"))

(defclass direct-hash-slot-definition (standard-direct-slot-definition)
  ()
  (:default-initargs :allocation :hash))

(defclass effective-hash-slot-definition (standard-effective-slot-definition)
  ()
  (:default-initargs :allocation :hash))

(defmethod validate-superclass ((class prototype-class) (super standard-class))
  t)

(defmethod validate-superclass ((class standard-class) (super prototype-class))
  t)

(defmethod direct-slot-definition-class 
    ((class prototype-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-hash-slot-definition))

(defmethod effective-slot-definition-class 
    ((class prototype-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-hash-slot-definition))

(define-condition prototype-missing (condition)
  ()
  (:documentation
   "Signalled when an object is not associated with a prototype."))

(defvar *prototype-handler* nil
  "Non-NIL when PROTOTYPE-MISSING handler is already installed on call stack.")

(defclass prototype-object ()
  ((hash :initform (make-hash-table :test #'eq)
         :reader hash
         :allocation :instance
         :documentation "Hash table holding :HASH object slots")
   (prototype :initarg :prototype
              :accessor prototype
              :allocation :instance
              :documentation "Object prototype or NIL."))
  (:metaclass prototype-class)
  (:default-initargs :prototype nil)
  (:documentation "Base class for all prototype objects"))

(defun fix-class-initargs (class &rest args
                                 &key ((:direct-superclasses dscs) '())
                                 &allow-other-keys)
"Fixup :DIRECT-SUPERCLASSES argument for [RE]INITIALIZE-INSTANCE gf
  specialized on prototype classes to include PROTOTYPE-OBJECT in
  superclass list"
  (remf args :direct-superclasses)
  (unless (or (eq class (find-class 'prototype-object))
              (find-if (lambda (c)
                         (unless (symbolp c) (setf c (class-name c)))
                         (subtypep c 'prototype-object))
                       dscs))
    (setf dscs (append dscs (list (find-class 'prototype-object)))))
  (list* :direct-superclasses dscs args))

(defmethod initialize-instance :around
    ((class prototype-class) &rest args &key &allow-other-keys)
  (apply #'call-next-method class (apply #'fix-class-initargs class args)))

(defmethod reinitialize-instance :around
    ((class prototype-class) &rest args &key &allow-other-keys)
  (apply #'call-next-method class (apply #'fix-class-initargs class args)))

(defun %prototype-of (class instance)
"Internal function to retreive prototype of an object"
  (if (typep class 'prototype-class)
    (or (prototype instance) (signal 'prototype-missing))
    (signal 'prototype-missing)))

(defun prototype-of (object)
  "Retrieves prototype of an OBJECT"
  (let ((class (class-of object)))
    (when (typep class 'prototype-class)
      (prototype object))))

(defun remove-direct-slot (slot object)
  (remhash slot (hash object)))

(defun clear-direct-slots (proto-obj &key excludes)
  (loop :for slot :being :the :hash-key :in (hash proto-obj) :using (:hash-value value)
        :if (not (member slot excludes)) :do (remove-direct-slot slot proto-obj)))

(defgeneric change-prototype (object new-prototype)
  (:documentation "Changes prototype of OBJECT to NEW-PROTOTYPE")
  (:method ((object prototype-object) new-prototype)
    (setf (prototype object) new-prototype)))

#+lispworks
(defun find-slot (class slot)
  (when (typep slot 'slot-definition)
    (return-from find-slot slot))
  (find-if (lambda (slot-def) 
             (eq (slot-definition-name slot-def) slot))
           (class-slots class)))

(defmethod slot-boundp-using-class ((class prototype-class) object slotd)
;  #+lispworks (setf slotd (find-slot class slotd))
  #+lispworks
  (let ((slot-obj (find-slot class slotd)))
    (if (not slot-obj)
        (return-from slot-boundp-using-class
          (slot-missing class object slotd 'slot-boundp))
      (setf slotd slot-obj)))
  (if (eq :hash (slot-definition-allocation slotd))
    (nth-value 1 (gethash (slot-definition-name slotd) (hash object)))
    (call-next-method)))

(defmethod slot-makunbound-using-class ((class prototype-class) object slotd)
;  #+lispworks (setf slotd (find-slot class slotd))
  #+lispworks
  (let ((slot-obj (find-slot class slotd)))
    (if (not slot-obj)
        (return-from slot-makunbound-using-class
          (slot-missing class object slotd 'slot-makunbound))
      (setf slotd slot-obj)))
  (if (eq :hash (slot-definition-allocation slotd))
    (remhash (slot-definition-name slotd) (hash object))
    (call-next-method)))

(defmethod slot-value-using-class ((class prototype-class) object slotd)
  #+lispworks
  (let ((slot-obj (find-slot class slotd)))
    (if (not slot-obj)
        (return-from slot-value-using-class
          (slot-missing class object slotd 'slot-value))
      (setf slotd slot-obj)))
  (if (eq :hash (slot-definition-allocation slotd))
    (values (gethash (slot-definition-name slotd) (hash object)))
    (standard-instance-access object (slot-definition-location slotd))))

(defmethod (setf slot-value-using-class)
    (new-value (class prototype-class) object slotd)
  #+lispworks
  (let ((slot-obj (find-slot class slotd)))
    (if (not slot-obj)
        (return-from slot-value-using-class
          (slot-missing class object slotd 'setf new-value))
      (setf slotd slot-obj)))
  (if (eq :hash (slot-definition-allocation slotd))
    (values (setf (gethash (slot-definition-name slotd) (hash object))
                  new-value))
    (setf (standard-instance-access object (slot-definition-location slotd))
          new-value)))

(defun %slot-missing (class instance slot op new-value)
"Internal function for performing hash-based slot lookup in case
of it is missing from class definition."
  (let ((hash (hash instance)))
    (symbol-macrolet ((prototype (%prototype-of class instance)))
      (case op
        (setf
         (setf (gethash slot hash) new-value))
        (slot-makunbound
         (remhash slot hash))
        (t (multiple-value-bind
                 (value present) (gethash slot hash)
             (ecase op
               (slot-value
                (if present
                  value
                  (when *walk-prototype* 
                    (slot-value prototype slot))))
               (slot-boundp
                (if present
                  t
                  (when *walk-prototype* 
                    (slot-boundp prototype slot)))))))))))

(defmethod slot-missing
    ((class prototype-class) instance slot op &optional new-value)
  (if *prototype-handler*
    (%slot-missing class instance slot op new-value)
    (handler-case
        (let ((*prototype-handler* t))
          (%slot-missing class instance slot op new-value))
      (prototype-missing () nil))))

;;;; vim: ft=lisp et
