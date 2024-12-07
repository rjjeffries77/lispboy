;;;; lispboy.lisp
(in-package #:lispboy)

(defun on-mac-os? ()
  "Returns true if running on macOS/Darwin"
  (or (member :darwin *features*)
      (member :macos *features*)))

(defstruct gameboy 
  (cpu (make-cpu))
  (mmu (make-mmu))
  (ppu (make-ppu)))

(defun load-cartridge (gb filename)
  (format t "~%Starting cartridge load: ~A~%" filename)
  (finish-output)  ; Force output to be displayed
  
  (format t "~%Initializing ROM...~%")
  (finish-output)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (load-rom (gameboy-mmu gb) stream))
  
  (let ((ppu (gameboy-ppu gb))
        (mmu (gameboy-mmu gb))
        (pc #x100)
        (end-address #x700))
    
    (if (on-mac-os?)
      (progn 
        (format t "Running on macOS - using make-this-thread-main~%")
        (finish-output)
        (sdl2:make-this-thread-main #'init-display))
      (progn
        (init-display)))
    (format t "Display initialized successfully~%")
    (finish-output)
    
    (format t "~%Setting up Nintendo logo...~%")
    (finish-output)
    ;(init-nintendo-logo ppu mmu)
    (format t "Logo setup complete~%")
    (finish-output)
    
    ;; Run display for logo animation
    (format t "~%Starting logo animation loop...~%")
    (finish-output)
    (loop for frame from 0 below 60
          do (format t "Frame ~D/60~%" frame)
             (finish-output)
             (loop for ly from 0 below +screen-height+
                   do (draw-scanline ppu mmu ly))
             (update-display ppu)
             (sleep 0.016))
    
    (format t "~%Logo animation complete. Starting main execution...~%")
    (finish-output)
    
    (loop while (< pc end-address)
          do (format t "PC: ~4,'0X~%" pc)
             (finish-output)
             (multiple-value-bind (next-pc cycles)
                 (execute (gameboy-cpu gb) (gameboy-mmu gb) pc)
               (setf pc next-pc)
               (wait-cycles (cpu-cycle-manager (gameboy-cpu gb)) cycles)
               (handle-interrupts (gameboy-cpu gb) (gameboy-mmu gb))))))

(defparameter a-gameboy (make-gameboy))

(defun run-cpu-test-rom ()
  (format t "~%Starting CPU test ROM execution...~%")
  (finish-output)
  (handler-case
      (load-cartridge a-gameboy "./roms/cpu_instrs.gb")
    (error (c)
      (format t "~%Error occurred: ~A~%" c)
      (finish-output))))

(export '(run-cpu-test-rom))