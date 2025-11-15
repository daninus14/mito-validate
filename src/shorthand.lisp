(in-package :mito-validate)

(defun ensure-class (class-or-class-name)
  (etypecase class-or-class-name
    (symbol (find-class class-or-class-name))
    (standard-class class-or-class-name)))

(defun get-class-columns (given-class)
  (mito.dao::table-column-slots
   (ensure-class given-class)))

(defun get-metaclass-slots (class-name)
  (closer-mop:class-slots (class-of (find-class class-name))))

(defun get-slot-name-string (slot)
  (symbol-name (closer-mop:slot-definition-name slot)))

(defun slot-name= (name-string slot)
  (string= name-string
           (get-slot-name-string slot)))

(defun find-slot-form-symbol (symbol class-name)
  (find (symbol-name symbol) (get-metaclass-slots class-name) :test #'slot-name=))

(defun get-slot-from-symbol (symbol class-name)
  (closer-mop:slot-definition-name
   (find-slot-form-symbol symbol class-name)))

(defmacro set-validation (validation-key validation-value class-name)
  `(setf
    ;; specified the package below since this macro may be used elsewhere, in order for it to be correct 
    (slot-value (find-class ,class-name) (mito-validate::get-slot-from-symbol ,validation-key ,class-name))
    ,validation-value))

(defmacro deftablev (class-name superclasses slot-definitions class-validations &rest options)
  `(progn
     (defclass ,class-name ,superclasses
       ,slot-definitions
       (:metaclass mito-validate:mito-validate-metaclass)
       ,@options)
     (progn ,@(loop for key in class-validations by #'cddr
                    for value in (cdr class-validations) by #'cddr
                    collect `(mito-validate::set-validation ,key ,value ',class-name)))))
