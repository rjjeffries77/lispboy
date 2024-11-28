;;;; lispboy.lisp

(in-package #:lispboy)

(defstruct gameboy 
 (cpu (make-cpu)
 (mmu (make-memory))))

(defun load-cartridge (gb filename)
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
            (load-rom (gb-mmu gb) stream))
    (execute (gameboy-cpu cpu) #x100))