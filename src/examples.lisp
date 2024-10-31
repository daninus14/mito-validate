(in-package :mito-validate)
(ql:quickload "mito")
(ql:quickload "mito-validate")
(setf mito:*auto-migration-mode* T)
(mito:dao-table-mixin)

(mito:dao-table-class)

(mito:connect-toplevel :postgres
                       :database-name "prueba"
                       :username "pruebauser"
                       :password "pruebauser")
                                        ;=> #<DBD.MYSQL:<DBD-MYSQL-CONNECTION> {100691BFF3}>

(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))
                                        ;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(mito:table-definition 'user)
                                        ;=> (#<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128))>)

(mito:deftable tweet ()
  ((status :col-type :text)
   (user :col-type user)))
                                        ;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::TWEET>

(mito:table-definition 'tweet)
                                        ;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user_id BIGINT UNSIGNED NOT NULL, created_at TIMESTAMP, updated_at TIMESTAMP)>)


(defclass validatable-user ()
  ((name :col-type (:varchar 64)
         :valid-type string
         :accessor user-name
         :skip-validation T)
   (email :col-type (or (:varchar 128) :null)
          :accessor user-email))
  (:metaclass mito-validate:mito-validate-metaclass))

(make-instance 'validatable-user :name "hello")

(closer-mop:compute-slots (class-of (make-instance 'validatable-user :name "hello")))
;; slot -> class -> (or "slots" "direct slots") -> slot (again?) -> here's valid-type, valid-function, skip-validation, col-type, and ghost


(mito:ensure-table-exists 'validatable-user)

(mito:insert-dao (make-instance 'validatable-user :name "hello"))
(mito:create-dao (make-instance 'validatable-user :name "hello"))
(mito:save-dao)


(setf (my-class-data (find-class 'my-class)) 10)

;; make a macro for defining validatable classes easily.

(make-instance 'validatable-user :name "hello")
(class-of (make-instance 'validatable-user :name "hello"))

(defclass c1 ()
  ((name :col-type (:varchar 64)
         :validation-type string
         :accessor name))
  (:metaclass mito-validate:mito-validate-metaclass))

(deftype legal-age ()
  '(and integer (>= x 18)))

(defclass c2 (c1)
  ((email :col-type (or (:varchar 128) :null)
          :accessor user-email)
   (age :col-type (or :null :integer)
        :accessor age
        :validation-function (lambda (x) (when (< x 0) (error "Age cannot be negative!"))))
   (age-claimed :col-type (or :null :integer)
                :accessor age-claimed
                :validation-type (integer 18)))
  (:metaclass mito-validate:mito-validate-metaclass))

(defclass c3 ()
  ((nickname :col-type (:varchar 64)
             :validation-type string
             :accessor nickname
             :skip-validation T))
  (:metaclass mito-validate:mito-validate-metaclass))

(defun get-class-columns (given-class)
  (mito.dao::table-column-slots
   (ensure-class given-class)))


(defun ensure-class (class-or-class-name)
  (etypecase class-or-class-name
    (symbol (find-class class-or-class-name))
    (standard-class class-or-class-name)))


(mito:ensure-table-exists 'c2)

(mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com"))
(mito:insert-dao (make-instance 'c2 :name 28 :email "ron@fig.com"))

(closer-mop:compute-slots (class-of (make-instance 'c2 :name "hello")))

(closer-mop:compute-slots (class-of (make-instance 'c2 :name "hello")))
(closer-mop:class-slots (class-of (make-instance 'c2 :name "hello")))


(mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age-claimed 17))
(mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age -18))
(mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age 18))

;;; Class Slots for Validation
#||
26. INFER-VALIDATION: NIL
27. SKIP-VALIDATION: NIL
28. SKIP-SLOT-VALIDATIONS: NIL
29. SKIP-OBJECT-VALIDATION: NIL
30. VALIDATION-FUNCTION: NIL
||#

(mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age-claimed 17))

(skip-validation (find-class 'c2))
(setf (skip-validation (find-class 'c2)) T)

#||
MITO-VALIDATE> (skip-validation (find-class 'c2))
NIL
MITO-VALIDATE> (setf (skip-validation (find-class 'c2)) T)
T
MITO-VALIDATE> (mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age-claimed 17))
#<C2 {100410B213}>
MITO-VALIDATE> (setf (skip-validation (find-class 'c2)) NIL)
NIL
MITO-VALIDATE> (mito:insert-dao (make-instance 'c2 :name "ron" :email "ron@fig.com" :age-claimed 17))
; Debugger entered on #<TYPE-ERROR expected-type: (INTEGER 18) datum: 17> ; ; ; ; ; ; ; ; ;
[1] MITO-VALIDATE> 
; Evaluation aborted on #<TYPE-ERROR expected-type: (INTEGER 18) datum: 17> ; ; ; ; ; ; ; ; ;
||#


(defclass purchase ()
  ((items
    :accessor items
    :col-type (or :null :integer))
   (price
    :accessor price
    :col-type (or :null :integer)))
  (:metaclass mito-validate-metaclass))

(setf (validation-function (find-class 'purchase))
      NIL)

(mito:insert-dao (make-instance 'purchase :items 3 :price 4))

(validation-function (find-class 'purchase))
(setf (validation-function (find-class 'purchase))
      (lambda (x)
        (when (< 10 (* (price x)
                       (items x)))
          (error "Purchase total cannot exceed 10!"))))

(mito:insert-dao (make-instance 'purchase :items 3 :price 4))
