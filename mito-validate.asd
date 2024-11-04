(defsystem "mito-validate"
  :version "0.1.0"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :depends-on ("mito"
               "closer-mop")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "classes")
                 (:file "mito-validate")
                 (:file "validate")                 
                 (:file "shorthand"))))
  :description "Validation System for mito ORM")
