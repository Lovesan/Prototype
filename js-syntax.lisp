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

(in-package :prototype)

(defvar *previous-readtables* '())
(defvar *default-readtable* (copy-readtable nil))

(defun char-invert (c)
  (declare (type character c))
  (cond ((upper-case-p c) (char-downcase c))
        ((lower-case-p c) (char-upcase c))
        (t c)))

(defun intern-name (name)
  (declare (type string name))
  (intern (case (readtable-case *readtable*)
            (:upcase (string-upcase name))
            (:downcase (string-downcase name))
            (:invert (map 'string #'char-invert name))
            (t name))))

(defun name-start-char-p (c)
  (and c (or (eql c #\_)
             (eql c #\+)
             (eql c #\-)
             (eql c #\=)
             (eql c #\!)
             (eql c #\/)
             (eql c #\*)
             (alpha-char-p c))))

(defun name-char-p (c)
  (and c (or (name-start-char-p c)
             (alphanumericp c))))

(defun ws-char-p (c)
  (and c (or (eql c #\Space)
             (eql c #\Tab)
             (eql c #\Return)
             (eql c #\Linefeed)
             (eql c #\Page))))

(defun nl-char-p (c)
  (and c (or (eql c #\Return)
             (eql c #\Linefeed)
             (eql c #\Page))))

(defun make-object-form (slots)
  (let ((object (gensym)))
    `(let ((,object (make-instance 'prototype-object)))
       ,@(loop :for (name . value) :in slots
               :collect `(setf (slot-value ,object ',name) ,value))
       ,object)))

(defun read-open-brace (stream c)
  (declare (ignore c))
  (prog ((c nil)
         (name (make-array 0 :element-type 'character
                             :adjustable t
                             :fill-pointer 0))
         (value nil)
         (slots '()))
   :start
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "End of file instead of name start"))
           ((eql c #\})
            (read-char stream)
            (return (make-object-form (nreverse slots))))
           ((name-start-char-p c)
            (read-char stream)
            (vector-push-extend c name)
            (go :name))
           (t (error "Name expected")))
   :name
     (setf c (peek-char nil stream nil))
     (cond ((null c)
            (error "End of file instead of name/value separator"))
           ((name-char-p c)
            (read-char stream)
            (vector-push-extend c name)
            (go :name))
           ((eql c #\:)
            (read-char stream)
            (go :value))
           ((ws-char-p c)
            (read-char stream)
            (go :separator))
           (t (error "Name/value separator expected")))
   :separator
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "End of file. Name/value separator expected."))
           ((eql c #\:)
            (read-char stream)
            (go :value))
           (t (error "Name/value separator expected")))
   :value
     (setf value (read stream t nil t))
     (push (cons (intern-name name) value) slots)
     (setf (fill-pointer name) 0)
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "Comma or close brace expected"))
           ((eql c #\,)
            (read-char stream)
            (go :start))
           (t (go :start)))))

(defun read-close-brace (s c)
  (declare (ignore s c))
  (error "Unmatched close brace"))

(defun read-open-bracket (stream c)
  (declare (ignore c))
  (prog ((values '())
         (c nil))
   :start
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "End of file inside vector literal"))
           ((eql c #\])
            (read-char stream)
            (return `(vector ,@(nreverse values))))
           (t (go :element)))
   :element
     (push (read stream t nil t) values)
     (setf c (peek-char t stream nil))
     (cond ((eql c #\,)
            (read-char stream)
            (go :start))
           (t (go :start)))))

(defun read-close-bracket (s c)
  (declare (ignore s c))
  (error "Unmatched close bracket"))

(defun %new (object)
  (cond ((typep object 'prototype-object)
         (make-instance 'prototype-object :prototype object))
        ((typep object 'sequence)
         (copy-seq object))
        ((typep object '(or symbol class))
         (make-instance object))
        ((functionp object)
         (funcall object))
        (t object)))

(defun %%new (symbol)
  (case symbol
    (cons (lambda () (cons nil nil)))
    (symbol (lambda () (gensym)))
    (integer (lambda () (random #.most-positive-fixnum)))
    (t (let ((class (find-class symbol)))
         (if (null class)
           (let ((fun (ignore-errors (symbol-function symbol))))
             fun)
           (lambda () (make-instance class)))))))

(defun make-new-form (from)
  (let ((constructor (gensym)))
    (if (symbolp from)
      `(let ((,constructor (%%new ',from)))
         #+sbcl
         (declare (sb-ext:muffle-conditions warning))
         (if ,constructor
           (funcall ,constructor)
           (%new ,from)))
      `(%new ,from))))

(defun read-n (stream c)
  (declare (ignore c))
  (prog ((c nil))
     (setf c (peek-char nil stream nil))
     (unless (eql c #\e)
       (unread-char #\n stream)
       (go :read-default))
     (read-char stream)
     (setf c (peek-char nil stream nil))
     (unless (eql c #\w)
       (unread-char #\e stream)
       (unread-char #\n stream)
       (go :read-default))
     (read-char stream)
     (setf c (peek-char nil stream nil))
     (when (or (null c) (eql c #\_) (alphanumericp c))
       (unread-char #\w stream)
       (unread-char #\e stream)
       (unread-char #\n stream)
       (go :read-default))
     (return (make-new-form (read stream t nil t)))
   :read-default
     (let ((*readtable* *default-readtable*))
       (return (read stream t nil t)))))

(defun read-v (stream c)
  (declare (ignore c))
  (prog ((c nil)
         (name (make-array 0 :element-type 'character
                             :adjustable t
                             :fill-pointer 0))
         (vars '()))
     (setf c (peek-char nil stream nil))
     (unless (eq c #\a)
       (unread-char #\v stream)
       (go :read-default))
     (read-char stream)
     (setf c (peek-char nil stream nil))
     (unless (eq c #\r)
       (unread-char #\a stream)
       (unread-char #\v stream)
       (go :read-default))
     (read-char stream)
     (setf c (peek-char nil stream nil))
     (unless (ws-char-p c)
       (unread-char #\r stream)
       (unread-char #\a stream)
       (unread-char #\v stream)
       (go :read-default))
   :name-start
     (setf (fill-pointer name) 0)
     (setf c (peek-char t stream nil))
     (unless (name-start-char-p c)
       (error "Expected variable name"))
     (read-char stream)
     (vector-push-extend c name)
   :name
     (setf c (peek-char nil stream nil))
     (cond ((null c)
            (error "Body forms expected"))
           ((alphanumericp c)
            (read-char stream)
            (vector-push-extend c name)
            (go :name)))
   :maybe-value
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "Body forms expected"))
           ((eql c #\,)
            (read-char stream)
            (push (intern-name name) vars)
            (go :name-start))
           ((eql c #\=)
            (read-char stream)
            (go :value))
           ((eql c #\;)
            (read-char stream)
            (push (intern-name name) vars)
            (return (nreverse vars)))
           (t (error "Variable, comma or semicolon expected")))
   :value
     (push (list (intern-name name)
                 (read-preserving-whitespace stream t nil t))
           vars)
     (setf c (peek-char t stream nil))
     (cond ((null c)
            (error "Body forms expected"))
           ((eql c #\,)
            (read-char stream)
            (go :name-start))
           ((eql c #\;)
            (read-char stream)
            (return (nreverse vars)))
           (t (error "Variable, comma or semicolon expected")))
   :read-default
     (let ((*readtable* *default-readtable*))
       (return (read stream t nil t)))))

(defun %enable-js-syntax ()
  (push *readtable* *previous-readtables*)
  (setf *readtable* (copy-readtable))
  (set-macro-character #\{ 'read-open-brace)
  (set-macro-character #\} 'read-close-brace)
  (set-macro-character #\[ 'read-open-bracket)
  (set-macro-character #\] 'read-close-bracket)
  (set-macro-character #\n 'read-n t)
  (set-macro-character #\v 'read-v t)
  (values))

(defun %disable-js-syntax ()
  (let ((readtable (pop *previous-readtables*)))
    (when readtable
      (setf *readtable* readtable))
    (values)))

(defmacro enable-js-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-js-syntax)))

(defmacro disable-js-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-js-syntax)))

;;; vim: ft=lisp et
