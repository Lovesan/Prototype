;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2015, Dmitry Ignatiev <lovesan.ru at gmail.com>

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

(in-package #:prototype)

(defvar *php-readtables* '())

(defun %elt (object idx)
  (etypecase object
    (hash-table (gethash idx object))
    (sequence (elt object idx))
    (standard-object (slot-value object idx))))

(defun (setf %elt) (newvalue object idx)
  (etypecase object
    (hash-table (setf (gethash idx object) newvalue))
    (sequence (setf (elt object idx) newvalue))
    (standard-object (setf (slot-value object idx)
                           newvalue))))

(defun terminating-char-p (c &optional (readtable *readtable*))
  (declare (type character c))
  (multiple-value-bind
        (exists nt)
      (get-macro-character c readtable)
    (and exists (not nt))))

(defun read-dollar (stream c)
  (declare (ignore c))
  (prog ((c nil)
         (name (make-array 0 :element-type 'character
                             :adjustable t
                             :fill-pointer 0))
         (args '())
         (access '()))
   :start
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "Name expected"))
           ((name-start-char-p c)
            (vector-push-extend c name)
            (read-char stream)
            (go :name))
           (t (error "Name expected")))
   :name
     (setf c (peek-char nil stream nil))
     (cond ((name-char-p c)
            (vector-push-extend c name)
            (read-char stream)
            (go :name))
           ((eql c #\.)
            (read-char stream)
            (setf access (intern-name name)
                  (fill-pointer name) 0)
            (go :after-dot))
           ((eql c #\[)
            (read-char stream)
            (setf access (intern-name name)
                  (fill-pointer name) 0)
            (go :after-open-bracket))
           ((eql c #\()
            (read-char stream)
            (setf access (intern-name name)
                  (fill-pointer name) 0)
            (setf args '())
            (go :after-open-paren))
           (t (return (intern-name name))))
   :after-dot
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "End of file after dot"))
           ((name-start-char-p c)
            (read-char stream)
            (vector-push-extend c name)
            (go :name-after-dot))
           (t (error "Name expected")))
   :name-after-dot
     (setf c (peek-char nil stream nil))
     (cond ((eql c #\.)
            (read-char stream)
            (setf access `(%elt ,access ',(intern-name name))
                  (fill-pointer name) 0)
            (go :after-dot))
           ((eql c #\[)
            (read-char stream)
            (setf access `(%elt ,access ',(intern-name name))
                  (fill-pointer name) 0)
            (go :after-open-bracket))
           ((eql c #\()
            (read-char stream)
            (setf access `(%elt ,access ',(intern-name name))
                  (fill-pointer name) 0
                  args '())
            (go :after-open-paren))
           ((name-char-p c)
            (read-char stream)
            (vector-push-extend c name)
            (go :name-after-dot))
           (t (return `(%elt ,access ',(intern-name name)))))
   :after-open-bracket
     (setf access `(%elt ,access ,(read stream t nil t)))
     (setf c (peek-char t stream nil))
     (unless (eql c #\])
       (error "Unmatched close bracket"))
     (read-char stream)
     (setf c (peek-char nil stream nil))
     (cond ((eql c #\[)
            (read-char stream)
            (go :after-open-bracket))
           ((eql c #\.)
            (read-char stream)
            (go :after-dot))
           ((eql c #\()
            (read-char stream)
            (setf args '())
            (go :after-open-paren))
           (t (return access)))
   :after-open-paren
     (setf c (peek-char t stream nil))
     (cond ((eql c #\))
            (read-char stream)
            (go :after-close-paren)))
     (push (read-preserving-whitespace stream t nil t) args)
   :next-arg
     (setf c (peek-char t stream nil))
     (cond ((eql c #\))
            (read-char stream)
            (go :after-close-paren))
           ((eql c #\,)
            (read-char stream)
            (push (read-preserving-whitespace stream t nil t) args)
            (go :next-arg))
           (t (error "Argument expected")))
   :after-close-paren
     (setf access `(funcall ,access ,@(nreverse args)))
     (setf c (peek-char nil stream nil))
     (cond ((eql c #\[)
            (read-char stream)
            (go :after-open-bracket))
           ((eql c #\.)
            (read-char stream)
            (go :after-dot))
           ((eql c #\()
            (read-char stream)
            (setf args '())
            (go :after-open-paren))
           (t (return access)))))

(defun %enable-php-syntax ()
  (push *readtable* *php-readtables*)
  (setf *readtable* (copy-readtable))
  (set-macro-character #\$ 'read-dollar)
  (set-syntax-from-char #\] #\))
  (values))

(defun %disable-php-syntax ()
  (let ((previous (pop *php-readtables*)))
    (when previous
      (setf *readtable* previous)))
  (values))

(defmacro enable-php-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-php-syntax)))

(defmacro disable-php-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-php-syntax)))

;;; vim: ft=lisp et
