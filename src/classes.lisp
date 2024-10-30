(in-package #:mito-validate)


(defgeneric valid-type-slot-value (given-object))
(defmethod valid-type-slot-value (given-object) NIL)
(defgeneric valid-function-slot-value (given-object))
(defmethod valid-function-slot-value (given-object) NIL)
(defgeneric skip-validation-slot-value (given-object))
(defmethod skip-validation-slot-value (given-object) NIL)


(defclass mito-validate-custom-standard-direct-slot-definition (c2mop:standard-direct-slot-definition)
  ((skip-validation :initform nil
                    :initarg :skip-validation
                    :type boolean
                    :accessor skip-validation-slot-value
                    :documentation "This provides the option to specify the slot with a skip-validation property.")
   (valid-function :initform nil
                   :initarg :valid-function
                   :accessor valid-function-slot-value
                   :documentation "This provides the option to specify the slot with a valid-function property.")
   (valid-type :initform nil
               :initarg :valid-type
               :accessor valid-type-slot-value
               :documentation "This provides the option to specify the slot with a valid-type property.")))

(defclass mito-validate-custom-standard-effective-slot-definition
    (closer-mop:standard-effective-slot-definition)
  ((skip-validation :initform nil
                    :initarg :skip-validation
                    :type boolean
                    :accessor skip-validation-slot-value
                    :documentation "This provides the option to specify the slot with a skip-validation property.")
   (valid-function :initform nil
                   :initarg :valid-function
                   :accessor valid-function-slot-value
                   :documentation "This provides the option to specify the slot with a valid-function property.")
   (valid-type :initform nil
               :initarg :valid-type
               :accessor valid-type-slot-value
               :documentation "This provides the option to specify the slot with a valid-type property.")))

(defclass mito-validate-standard-direct-slot-definition (mito.dao.column:dao-table-column-class  mito-validate-custom-standard-direct-slot-definition)
  ())

(defclass mito-validate-metaclass (mito:dao-table-class)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class mito-validate-metaclass)
                                                    &rest initargs)
  (find-class 'mito-validate-standard-direct-slot-definition))

(defmethod closer-mop:validate-superclass ((class mito-validate-metaclass)
                                           (superclass closer-mop:standard-class))
  t)

