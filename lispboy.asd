;;;; lispboy.asd

(asdf:defsystem #:lispboy
  :description "A gameboy emulator in commmon lisp"
  :author "Richard Jeffries <richardjjeffries@googlemail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-json :cl-ppcre :sdl2 :bordeaux-threads)
  :components ((:file "package")
               (:file "timings")
               (:file "memory")
               (:file "opcodes")
               (:file "cpu")
               (:file "cartridge")
               (:file "display")
               (:file "lispboy")))
