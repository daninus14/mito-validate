(in-package #:mito-validate)


(defgeneric valid-type-slot-value (given-object))
(defmethod valid-type-slot-value (given-object) NIL)
(defgeneric valid-function-slot-value (given-object))
(defmethod valid-function-slot-value (given-object) NIL)
(defgeneric skip-validation-slot-value (given-object))
(defmethod skip-validation-slot-value (given-object) NIL)


(defclass mito-validate-custom-standard-direct-slot-definition
    (c2mop:standard-direct-slot-definition)
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

(defclass mito-validate-standard-effective-slot-definition (mito.dao.column:dao-table-column-class  mito-validate-custom-standard-effective-slot-definition)
  ())

(defclass mito-validate-metaclass (mito:dao-table-class)
  ((skip-validation
    :initform NIL
    :accessor skip-validation
    :documentation "If T will skip validation for this object entirely.")
   (skip-slot-validations
    :initform NIL
    :accessor skip-slot-validations
    :documentation "If T will skip slot level validations for this object. Note however that if a valid-function was provided, the object level validation will be executed unless otherwise marked to be skipped.")
   (skip-object-validation
    :initform NIL
    :accessor skip-object-validation
    :documentation "If T will skip the function to validate the entire object. Note however that slot level validations *will* be executed unless skip-validation or skip-slot-validations are T")
   (valid-function
    :initform NIL
    :accessor valid-function
    :documentation "An optional function which will receive the an instance of the class as its argument. In case the object is not valid, a condition should be signaled. Returned values will be ignored.")))

(defmethod closer-mop:direct-slot-definition-class ((class mito-validate-metaclass)
                                                    &rest initargs)
  (declare (ignorable initargs))
  (find-class 'mito-validate-standard-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class mito-validate-metaclass)
                                                       &rest initargs)
  (declare (ignorable initargs))
  (find-class 'mito-validate-standard-effective-slot-definition))


;; (defmethod closer-mop:compute-slots ((class mito-validate-metaclass))
;;   (let ((slots (call-next-method)))
;;     (push (make-instance 'closer-mop:slot-definition
;;                          :name 'skip-validation
;;                          :initform NIL
;;                          :accessor 'skip-validation-slot-value
;;                          :documentation "This provides the option to specify the slot with a valid-type property.")
;;           slots)
;;     (push (make-instance 'closer-mop:slot-definition
;;                          :name 'valid-function
;;                          :initform nil
;;                          :accessor 'valid-function-slot-value
;;                          :documentation "This provides the option to specify the slot with a valid-function property.")
;;           slots)
;;     (push (make-instance 'closer-mop:slot-definition
;;                          :name 'valid-type
;;                          :initform nil
;;                          :accessor 'valid-type-slot-value
;;                          :documentation "This provides the option to specify the slot with a valid-type property.")
;;           slots)
;;     slots))

(defmethod closer-mop:validate-superclass ((class mito-validate-metaclass)
                                           (superclass closer-mop:standard-class))
  t)

