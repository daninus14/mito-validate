(defsystem "mito-validate"
           :version "0.1.0"
           :author "Daniel Nussenbaum"
           :license "MIT"
           :depends-on ("mito"
                        "closer-mop")
           :components ((:module "src"
                                 :components
                                 ((:file "mito-validate")
                                  (:file "packages"))))
           :description "Validation System for mito ORM")
