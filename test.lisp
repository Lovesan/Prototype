(defpackage :prototype-test 
  (:use :cl :prototype :lift)
  (:export #:run-tests))

(in-package :prototype-test)
(deftestsuite prototype-test () ())

(defclass foo ()
  ((a :accessor foo-a))
  (:metaclass prototype-class))

(defclass quux ()
  ((the-slot :initform 'the-value))
  (:documentation "Simple standard class"))

(addtest main-prototype-test
  (ensure-same (let* ((proto (make-instance 'prototype-object))
                      (foo (make-instance 'foo :prototype proto))
                      (bar (make-instance 'prototype-object :prototype foo))
                      (quux (make-instance 'quux))
                      res)
                 (setf (slot-value proto 'x) 123)
                 (push (slot-value bar 'x) res) ;;res = '(123)
                 (setf (foo-a foo) 456)
                 (push (slot-value bar 'a) res) ;;res = '(456 123)
                 (setf (slot-value bar 'a) 789)
                 (push (slot-value bar 'a) res) ;;res = '(789 456 123)
                 (change-prototype bar quux)
                 (push (slot-value bar 'the-slot) res) ;;res = '(the-value 789 456 123)
                 (push (typep 
                        (nth-value 1 (ignore-errors (slot-value bar 'x)))
                        'condition) 
                       res) ;;res = '(t the-value 789 456 123)
                 )
               '(t the-value 789 456 123)))

