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

#|
(setf proto (make-instance 'prototype-object)
      foo (make-instance 'foo :prototype proto)
      bar (make-instance 'prototype-object :prototype foo)
      quux (make-instance 'quux)
      res nil)

|#
(addtest main-prototype-test
  (ensure-same (let* ((*walk-prototype* t)
                      (proto (make-instance 'prototype-object))
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

                 ;;Test remove-direct-slot
                 (remove-direct-slot 'a bar)
                 (push (slot-value bar 'a) res) ;;res = '(456 789 456 123)

                 ;;Test *walk-prototype*
                 (push (let ((*walk-prototype* nil))
                         (slot-value bar 'a))
                       res) ;;res = '(nil 456 789 456 123)

                 ;;Test clear-direct-slots
                 (setf (slot-value bar 'm) 0)
                 (setf (slot-value bar 'n) 1)
                 (push (list (slot-value bar 'm) (slot-value bar 'n)) res) ;;res = '((0 1) nil 456 789 456 123)
                 (clear-direct-slots bar)
                 (push (list (slot-value bar 'm) (slot-value bar 'n)) res) ;;res = '((nil nil) (0 1) nil 456 789 456 123)
                 
                 ;;Test change-prototype
                 (change-prototype bar quux)
                 (push (slot-value bar 'the-slot) res) ;;res = '(the-value (nil nil) (0 1) nil 456 789 456 123)
                 (push (typep 
                        (nth-value 1 (ignore-errors (slot-value bar 'x)))
                        'condition) 
                       res) ;;res = '(t the-value (nil nil) (0 1) nil 456 789 456 123)

                 ;;Test slot-boundp
                 (push (list (slot-boundp bar 'the-slot) 
                             (let ((*walk-prototype* nil))
                               (slot-boundp bar 'the-slot)))
                       res)  ;;res = '((t nil) t the-value (nil nil) (0 1) nil 456 789 456 123) 
               
                 ;;Test slot-makunbound
                 (setf (slot-value bar 'z) 'zzz)
                 (slot-makunbound bar 'z)
                 (push (let ((*walk-prototype* nil)) (slot-boundp bar 'z)) 
                       res) ;; '(nil (t nil) t the-value (nil nil) (0 1) nil 456 789 456 123)
                 )
               '(nil (t nil) t the-value (nil nil) (0 1) nil 456 789 456 123)))

(addtest skip-slots-test
  (ensure-same (let* ((*walk-prototype* t)
                      (proto (make-instance 'prototype-object-ext))
                      (foo (make-instance 'prototype-object-ext :prototype proto))
                      (bar (make-instance 'prototype-object-ext :prototype foo)))
                 (setf (slot-value proto 'slot1) "some-data")
                 (setf (slot-value proto 'slot2) "some-data2")
                 (setf (slot-value foo 'slot1) "other-data1" )
                 (setf (slot-value foo 'slot2) '(:skip "other-data2"))
                 (list (slot-value bar 'slot1)
                       (slot-value bar 'slot2)
                       (progn (setf (fn-no-inherit foo) (lambda (val)
                                                            (and (consp val)
                                                                 (eq :skip (first val)))))
                         (slot-value bar 'slot2))))
               '("other-data1" (:SKIP "other-data2") "some-data2")))
                         

