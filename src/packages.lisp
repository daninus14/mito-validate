(cl:in-package #:common-lisp-user)

(defpackage #:mito-validate
  (:use :cl)
  (:export
   :valid-type
   :valid-function
   :mito-validate-metaclass
   :skip-object-validation
   :skip-slot-validations
   :skip-validation
   :*development-mode-p*))
