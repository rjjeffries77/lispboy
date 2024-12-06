;;;; lispboy.lisp

(in-package #:lispboy)

(defstruct gameboy 
 (cpu (make-cpu))
 (mmu (make-mmu)))

(defun load-cartridge (gb filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
        (load-rom (gameboy-mmu gb) stream))
   (let ((ppu (make-ppu))
         (mmu (gameboy-mmu gb))
         (pc #x100)
         (end-address #x700)) ; Test with a small ROM
        ;; Initialize SDL display
        (init-display)
      
        ;; Set up Nintendo logo
        (init-nintendo-logo ppu mmu )
      
        ;; Run display for logo animation
        (loop for frame from 0 below 60  ; Show logo for ~1 second
            do
               ;; Draw all scanlines
               (loop for ly from 0 below +screen-height+
                     do (draw-scanline ppu mmu ly))
               ;; Update display
               (update-display ppu)
               (sleep 0.016))  ; ~60fps
        ; Start cartridge
        (loop while (< pc end-address)
                do (multiple-value-bind (next-pc cycles)
                                (execute (gameboy-cpu gb) (gameboy-mmu gb) pc)
                                (progn 
                                        (setf pc next-pc)
                                        (wait-cycles (cpu-cycle-manager (gameboy-cpu gb)) cycles)
                                        (handle-interrupts (gameboy-cpu gb) (gameboy-mmu gb)))))))

(defparameter a-gameboy (make-gameboy))
; testing
(export '(a-gameboy load-cartridge))