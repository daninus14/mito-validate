(in-package #:mito-validate)

;;; parameters

(defparameter *development-mode-p* NIL)

;;; mito methods added to call validation before DB writes

(defmethod mito:insert-dao :before (obj)
  (validate-if-needed obj))

(defmethod mito:create-dao :before (class &rest initargs)
  (validate-if-needed obj))

(defmethod mito:save-dao :before (obj)
  (validate-if-needed obj))

;;; validation functionality


(defun validate-if-needed (obj)
  (validate-if-needed-helper obj (class-of obj)))

(defgeneric validate-if-needed-helper (obj class)
  (:documentation "Helper to only check validation if class is an instance of mito-validate-metaclass.

The functionality should be as follows:
 - Check here the class of the object
 - The class should be an instance of the metaclass mito-validate
 - it should have the slots for :skip-object-validation and :skip-validation
 - if those are false, then do the validations. Otherwise skip them.
 - Validation should be to call function stored in :valid-function slot of class
 - Then call validate-slots
"))

(defmethod validate-if-needed-helper (obj class)
  NIL)

(defmethod validate-if-needed-helper (obj (class mito-validate-metaclass))
  (unless skip-validation
    (unless :skip-object-validation
      (validate-object-level obj))
    (unless :skip-slot-validations
      (validate-slots obj))))

(defun validate-object-level (obj)
  ;; Validation should be to call function stored in :valid-function slot of class
  (when :valid-function (funcall :valid-function obj))
  )

(defun validate-slots (obj)
  ;; Will get all slots of class, check if :skip-validation is NIL
  ;; and call validate-slot on each one
  (let ((slots (if *development-mode-p*
                   (closer-mop:compute-slots (class-of obj))
                   (closer-mop:class-slots (class-of obj)))))
    (lop for slot in slots)))

(defun validate-slot (slot)
  ;; Will first check if there's a :valid-type in the class of the slot
  ;; assert based on that type if it's present.
  ;; Then check if there's a :valid-function and call the function on the slot
  ;; If neither of :valid-function and :valid-type are present then call
  ;; validate-slot-from-inferred-type
  )

(defun validate-slot-from-inferred-type (slot)
  ;; This will get the mito type of the slot
  ;; It will then try to make a CL equivalent type to the DB type
  ;; Then it will try to assert the slot's data against the CL Type
  )
