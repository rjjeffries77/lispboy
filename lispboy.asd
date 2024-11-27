;;;; lispboy.asd

(asdf:defsystem #:lispboy
  :description "Describe lispboy here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-json :cl-ppcre)
  :components ((:file "package")
               (:file "lispboy")
               (:file "opcodes")
               (:file "memory")
               (:file "cpu")))
