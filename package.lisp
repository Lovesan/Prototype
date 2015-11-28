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
   #:prototype-object-ext
   #:prototype-of
   #:change-prototype
   #:*walk-prototype*
   #:remove-direct-slot
   #:clear-direct-slots
   #:reset-slots
   #:direct-slots-alist
   #:prototype-object-ext
   #:fn-no-inherit

   #:enable-js-syntax
   #:disable-js-syntax
   #:enable-php-syntax
   #:disable-php-syntax))
