(in-package #:mito-validate)


(defgeneric validation-type-slot-value (given-object))
(defmethod validation-type-slot-value (given-object) NIL)
(defgeneric validation-function-slot-value (given-object))
(defmethod validation-function-slot-value (given-object) NIL)
(defgeneric skip-validation-slot-value (given-object))
(defmethod skip-validation-slot-value (given-object) NIL)


(defclass mito-validate-custom-standard-direct-slot-definition
    (c2mop:standard-direct-slot-definition)
  ((skip-validation :initform nil
                    :initarg :skip-validation
                    :type boolean
                    :accessor skip-validation-slot-value
                    :documentation "This provides the option to specify the slot with a skip-validation property.")
   (validation-function :initform nil
                        :initarg :validation-function
                        :accessor validation-function-slot-value
                        :documentation "This provides the option to specify the slot with a validation-function property.")
   (validation-type :initform nil
                    :initarg :validation-type
                    :accessor validation-type-slot-value
                    :documentation "This provides the option to specify the slot with a validation-type property.")))

(defclass mito-validate-custom-standard-effective-slot-definition
    (closer-mop:standard-effective-slot-definition)
  ((skip-validation :initform nil
                    :initarg :skip-validation
                    :type boolean
                    :accessor skip-validation-slot-value
                    :documentation "This provides the option to specify the slot with a skip-validation property.")
   (validation-function :initform nil
                        :initarg :validation-function
                        :accessor validation-function-slot-value
                        :documentation "This provides the option to specify the slot with a validation-function property.")
   (validation-type :initform nil
                    :initarg :validation-type
                    :accessor validation-type-slot-value
                    :documentation "This provides the option to specify the slot with a validation-type property.")))

(defclass mito-validate-standard-direct-slot-definition (mito.dao.column:dao-table-column-class  mito-validate-custom-standard-direct-slot-definition)
  ())

(defclass mito-validate-standard-effective-slot-definition
    (mito-validate-custom-standard-effective-slot-definition)
  ())

(defclass mito-validate-metaclass (mito:dao-table-class)
  ((skip-validation
    :initform NIL
    :accessor skip-validation
    :documentation "If T will skip validation for this object entirely.")
   (skip-slot-validations
    :initform NIL
    :accessor skip-slot-validations
    :documentation "If T will skip slot level validations for this object. Note however that if a validation-function was provided, the object level validation will be executed unless otherwise marked to be skipped.")
   (skip-object-validation
    :initform NIL
    :accessor skip-object-validation
    :documentation "If T will skip the function to validate the entire object. Note however that slot level validations *will* be executed unless skip-validation or skip-slot-validations are T")
   (validation-function
    :initform NIL
    :accessor validation-function
    :documentation "An optional function which will receive the an instance of the class as its argument. In case the object is not valid, a condition should be signaled. Returned values will be ignored.")))

(defmethod closer-mop:direct-slot-definition-class ((class mito-validate-metaclass)
                                                    &rest initargs)
  (declare (ignorable initargs))
  (find-class 'mito-validate-standard-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class mito-validate-metaclass)
                                                       &rest initargs)
  (declare (ignorable initargs))
  (find-class 'mito-validate-standard-effective-slot-definition))

(defmethod closer-mop:validate-superclass ((class mito-validate-metaclass)
                                           (superclass closer-mop:standard-class))
  t)

(defmethod closer-mop:compute-effective-slot-definition
    :around ((class mito-validate-metaclass) name direct-slot-definitions)
  (declare (ignore name))
  (let ((result (call-next-method)))
    (when result
      (setf (skip-validation-slot-value result)
            (some #'skip-validation-slot-value direct-slot-definitions))
      (setf (validation-type-slot-value result)
            (some #'validation-type-slot-value direct-slot-definitions))
      (setf (validation-function-slot-value result)
            (some #'validation-function-slot-value direct-slot-definitions)))
    result))
