(cl:in-package #:common-lisp-user)

(defpackage mito-validate
  (:use :cl)
  (:export
   #:deftablev
   #:set-validation
   #:validation-type
   #:validation-function
   #:mito-validate-metaclass
   #:skip-object-validation
   #:skip-slot-validations
   #:skip-validation))
