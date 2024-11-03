(in-package :mito-validate)

;;; mito methods added to call validation before DB writes

(defmethod mito:insert-dao :before (obj)
  (validate-if-needed obj))

(defmethod mito:save-dao :before (obj)
  (validate-if-needed obj))

;;; mop and mito helper functions

(defun get-class-columns (given-class)
  (mito.dao::table-column-slots
   (ensure-class given-class)))


(defun ensure-class (class-or-class-name)
  (etypecase class-or-class-name
    (symbol (find-class class-or-class-name))
    (standard-class class-or-class-name)))

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
 - Validation should be to call function stored in :validation-function slot of class
 - Then call validate-slots"))

(defmethod validate-if-needed-helper (obj class)
  NIL)

(defmethod validate-if-needed-helper (obj (class mito-validate-metaclass))
  (unless (skip-validation class)
    (unless (skip-object-validation class)
      (validate-object-level obj))
    (unless (skip-slot-validations class)
      (validate-slots obj))))

(defun validate-object-level (obj)
  ;; Validation should be to call function stored in :validation-function slot of class
  (when (validation-function (class-of obj))
    (funcall (validation-function (class-of obj)) obj)))

(defun validate-slots (obj)
  ;; Will get all slots of class, check if :skip-validation is NIL
  ;; and call validate-slot on each one  
  (let ((slots (closer-mop:compute-slots (class-of obj))
               ;; (get-class-columns (class-of obj))
               ))
    (loop for slot in slots
          unless (skip-validation-slot-value slot)
            ;; here add conditional for infer-validation of the slot and class.
            do (validate-slot obj slot))))

(defun validate-slot (obj slot)
  ;; Will first check if there's a :validation-type in the class of the slot
  ;; assert based on that type if it's present.
  ;; Then check if there's a :validation-function and call the function on the slot
  ;; If neither of :validation-function and :validation-type are present then call
  ;; validate-slot-from-inferred-type
  ;; Or maybe just check if the class level infer-validation is set, then only
  ;; do infer-validation if no specific slot-level validation was provided
  ;; However, if infer-validation was specified in the sot, then it will
  ;; be performed regardless of other validation approaches specified
  (when (slot-boundp obj (closer-mop:slot-definition-name slot))
    (when (validation-type-slot-value slot)
      (unless (typep
               (slot-value obj
                           (closer-mop:slot-definition-name slot))
               (validation-type-slot-value slot))
        (error
         'type-error
         :expected-type (validation-type-slot-value slot)
         :datum (slot-value obj
                            (closer-mop:slot-definition-name slot)))))
    (when (validation-function-slot-value slot)
      (funcall (eval (validation-function-slot-value slot))
               (slot-value obj
                           (closer-mop:slot-definition-name slot))
               ))))

(defun validate-slot-from-inferred-type (slot)
  ;; This will get the mito type of the slot
  ;; It will then try to make a CL equivalent type to the DB type
  ;; Then it will try to assert the slot's data against the CL Type
  )
