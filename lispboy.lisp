;;;; lispboy.lisp

(in-package #:lispboy)

(defstruct gameboy 
 (cpu (make-cpu))
 (mmu (make-mmu)))

(defun load-cartridge (gb filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (load-rom (gameboy-mmu gb) stream))
        (let ((pc #x100)           ; Start at 0x100 (entry point)
                (end-address #x700)) ; End at 0x150 (end of boot ROM area)
        (loop while (< pc end-address)
                do (setf pc (execute (gameboy-cpu gb) (gameboy-mmu gb) pc)))))
; (defun load-cartridge (gb filename)
;   (with-open-file (stream filename :element-type '(unsigned-byte 8))
;     (load-rom (gameboy-mmu gb) stream))
;   (format t "~%Starting disassembly from #x100:~%")
;   (let ((pc #x100))
;     (dotimes (i 20)  ; Try first 20 bytes for debugging
;       (if (< pc #x150)  ; Stay within header area
;           (setf pc (debug-execute (gameboy-cpu gb) (gameboy-mmu gb) pc))
;           (return)))))
(defparameter a-gameboy (make-gameboy))
; testing
(export '(a-gameboy load-cartridge))