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

(mito:connect-toplevel :sqlite3
                       :database-name #P":memory:")

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

(defmacro deftablev (class-name superclasses slot-definitions class-validations &rest options)
  `(progn
     (defclass ,class-name ,superclasses
       ,slot-definitions
       (:metaclass mito-validate:mito-validate-metaclass)
       ,@options)
     ;; over here loop over property list and generate (setf) forms
     ;; Need to get the accessor from the provided symbol
     ;; Probably with slot-definition-name
     ;; Or maybe can just do (slot-value class name) instead of getting the accessor
     ;; don't forget to do (find-class ,class-name) to get the actual class object
     (progn ,@(loop for key in class-validations by #'cddr
                    for value in (cdr class-validations) by #'cddr
                    collect `(set-validation ,key ,value ',class-name)))))

(defmacro set-validation (validation-key validation-value class-name)
  `(setf
    (slot-value (find-class ,class-name) (get-slot-from-symbol ,validation-key ,class-name))
    ,validation-value))

(let ((plist '(:name "Alice" :age 30 :city "Wonderland")))
  (loop for key in plist by #'cddr
        for value in (cdr plist) by #'cddr
        do (format t "Key: ~a, Value: ~a~%" key value)))


(symbol-name :validation-function)
(class-of (find-class 'c2))
(closer-mop:slot-definition-name (nth 30 (closer-mop:class-slots (class-of (find-class 'c2)))))
(string=
 (symbol-name :validation-function)
 (symbol-name (closer-mop:slot-definition-name
               (nth 30 (closer-mop:class-slots (class-of (find-class 'c2)))))))

(defun get-metaclass-slots (class-name)
  (closer-mop:class-slots (class-of (find-class class-name))))

(defun get-slot-name-string (slot)
  (symbol-name (closer-mop:slot-definition-name slot)))

(defun slot-name= (name-string slot)
  (string= name-string
           (get-slot-name-string slot)))

(find (symbol-name :validation-function) (get-metaclass-slots 'c2) :test #'slot-name=)

(defun find-slot-form-symbol (symbol class-name)
  (find (symbol-name symbol) (get-metaclass-slots class-name) :test #'slot-name=))

(find-slot-form-symbol :validation-function 'c2)

(closer-mop:slot-definition-name (find-slot-form-symbol :validation-function 'c2))

(defun get-slot-from-symbol (symbol class-name)
  (closer-mop:slot-definition-name
   (find-slot-form-symbol symbol class-name)))

(setf (slot-value
       (find-class 'c2)
       (closer-mop:slot-definition-name
        (find-slot-form-symbol :validation-function 'c2)))
      2)

(setf
 (slot-value (find-class 'c2) (get-slot-from-symbol :validation-function 'c2))
 (lambda (x) (* 9 9)))

;; TODO now probably use that slot, or slot definition name with slot-value to set the value
;; finish the macro set-validation
;; then finish the previous macro which should call set-validation
;; make sure the macro expansion generates the right code 
(deftablev c4 ()
  ((items
    :accessor items
    :col-type (or :null :integer))
   (price
    :accessor price
    :col-type (or :null :integer)))
  (:validation-function (lambda (x)
                          (when (< 10 (* (price x)
                                         (items x)))
                            (error "Purchase total cannot exceed 10!"))))
  (:documentation "This is a sample table"))

(setf (validation-function (find-class 'purchase))
      (lambda (x)
        (when (< 10 (* (price x)
                       (items x)))
          (error "Purchase total cannot exceed 10!"))))

(mito:insert-dao (make-instance 'c4 :items 3 :price 4))

(mito:deftable)
