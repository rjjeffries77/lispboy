;;;; lispboy.lisp
(in-package #:lispboy)

(defun on-mac-os? ()
  "Returns true if running on macOS/Darwin"
  (or (member :darwin *features*)
      (member :macos *features*)))

(defstruct gameboy 
  (cpu (make-cpu))
  (mmu (make-mmu))
  (ppu (make-ppu-with-framebuffer))
  (clock-state (make-cycle-state)))


(defun reset-cpu (cpu)
  "Reset CPU to power-on state"
  ;; Set registers to initial values
  (setf (cpu-a cpu) #x01)    ; A starts at 01
  (setf (cpu-f cpu) #xB0)    ; F starts at B0 (1011 0000)
  (setf (cpu-b cpu) #x00)    ; B starts at 00
  (setf (cpu-c cpu) #x13)    ; C starts at 13
  (setf (cpu-d cpu) #x00)    ; D starts at 00
  (setf (cpu-e cpu) #xD8)    ; E starts at D8
  (setf (cpu-h cpu) #x01)    ; H starts at 01
  (setf (cpu-l cpu) #x4D)    ; L starts at 4D
  
  ;; Set PC and SP
  (setf (cpu-pc cpu) #x0100) ; PC starts at 0100 (after boot ROM)
  (setf (cpu-sp cpu) #xFFFE) ; SP starts at FFFE
  
  ;; Reset interrupt flag
  (setf (cpu-ime cpu) nil)   ; IME starts disabled
  
  ;; Reset cycle manager
  (setf (cpu-cycle-manager cpu) (make-cycle-manager))
  
  cpu) ; Return the reset CPU

;; Example usage to reset the whole system:
(defun reset-system (gb)
  "Reset the entire system to power-on state"
  ;; Reset CPU
  (reset-cpu (gameboy-cpu gb))
  
  ;; Reset MMU memory regions
  (let ((mmu (gameboy-mmu gb)))
    ;; Reset IE register
    (setf (mmu-ie mmu) #x00)
    
    ;; Reset IF register
    (setf (mmu-if mmu) #xE1)
    
    ;; Setup initial IO register values
    (write-memory mmu #xFF05 #x00) ; TIMA
    (write-memory mmu #xFF06 #x00) ; TMA
    (write-memory mmu #xFF07 #x00) ; TAC
    (write-memory mmu #xFF10 #x80) ; NR10
    (write-memory mmu #xFF11 #xBF) ; NR11
    (write-memory mmu #xFF12 #xF3) ; NR12
    (write-memory mmu #xFF14 #xBF) ; NR14
    (write-memory mmu #xFF16 #x3F) ; NR21
    (write-memory mmu #xFF17 #x00) ; NR22
    (write-memory mmu #xFF19 #xBF) ; NR24
    (write-memory mmu #xFF1A #x7F) ; NR30
    (write-memory mmu #xFF1B #xFF) ; NR31
    (write-memory mmu #xFF1C #x9F) ; NR32
    (write-memory mmu #xFF1E #xBF) ; NR34
    (write-memory mmu #xFF20 #xFF) ; NR41
    (write-memory mmu #xFF21 #x00) ; NR42
    (write-memory mmu #xFF22 #x00) ; NR43
    (write-memory mmu #xFF23 #xBF) ; NR44
    (write-memory mmu #xFF24 #x77) ; NR50
    (write-memory mmu #xFF25 #xF3) ; NR51
    (write-memory mmu #xFF26 #xF1) ; NR52
    (write-memory mmu #xFF40 #x91) ; LCDC
    (write-memory mmu #xFF42 #x00) ; SCY
    (write-memory mmu #xFF43 #x00) ; SCX
    (write-memory mmu #xFF45 #x00) ; LYC
    (write-memory mmu #xFF47 #xFC) ; BGP
    (write-memory mmu #xFF48 #xFF) ; OBP0
    (write-memory mmu #xFF49 #xFF) ; OBP1
    (write-memory mmu #xFF4A #x00) ; WY
    (write-memory mmu #xFF4B #x00)) ; WX
  
  ;; Reset PPU state
  (let ((ppu (gameboy-ppu gb)))
    (setf (ppu-lcdc ppu) #x91)  ; LCD Control
    (setf (ppu-stat ppu) #x85)  ; STAT register initial value
    (setf (ppu-ly ppu) 0)       ; Current scanline
    (setf (ppu-lyc ppu) 0)      ; Scanline compare
    (setf (ppu-bgp ppu) #xFC)   ; Background palette
    (setf (ppu-obp0 ppu) #xFF)  ; Object palette 0
    (setf (ppu-obp1 ppu) #xFF)  ; Object palette 1
    (setf (ppu-scx ppu) 0)      ; Scroll X
    (setf (ppu-scy ppu) 0)      ; Scroll Y
    (setf (ppu-wx ppu) 0)       ; Window X
    (setf (ppu-wy ppu) 0))      ; Window Y
  
  gb) 

(defun load-cartridge (gb filename)
  (format t "~%Starting cartridge load: ~A~%" filename)
  (finish-output)

  (format t "~%Initializing ROM...~%")
  (finish-output)
  (reset-system gb)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (load-rom (gameboy-mmu gb) stream))

  ;; Run init-display on the main thread
  (if (on-mac-os?)
      (progn
        (format t "Running on macOS - using make-this-thread-main~%")
        (finish-output)
        (Sdl2:make-this-thread-main (lambda () (emulator-main gb))))
      (progn
        (emulator-main gb)))
  (format t "Display initialized successfully~%")
  (finish-output))

(defparameter a-gameboy (make-gameboy))
(defparameter *emulator-running* T)

(defparameter *debug-timing* t)          ; Enable/disable timing debug logs
(defparameter *emulation-speed* 0.1)     ; 10% normal speed
(defparameter *log-every-n-cycles* 100)  ; Log every 100 CPU cycles

(defconstant +gameboy-clock-speed+ 4194304) ; Hz
(defconstant +nanosecs-per-cycle+ (/ 1000000000 +gameboy-clock-speed+))

(defun log-timing (format-string &rest args)
  "Log timing information if debug is enabled"
  (when *debug-timing*
    (format t "~&[TIMING] ~?" format-string args)
    (finish-output)))

(defun run-clock-thread (gb)
  "Main clock thread that signals CPU/PPU at Game Boy clock speed"
  (let* ((state (make-cycle-state))
         (cycle-duration (/ 1.0 4194304))) ; Duration in seconds for one cycle
    
    (loop while *emulator-running* do
          ;; Signal both CPU and PPU
          (sb-thread:signal-semaphore (cycle-state-cpu-semaphore state))
          (sb-thread:signal-semaphore (cycle-state-ppu-semaphore state))          
          ;; Wait for next cycle
          (sleep cycle-duration))))

(defun run-cpu-thread (gb)
  "CPU thread with its own semaphore"
  (let* ((cpu (gameboy-cpu gb))
         (mmu (gameboy-mmu gb))
         (cycle-state (gameboy-clock-state gb))
         (cycles-needed 0))
    
    (loop while *emulator-running* do
          ;; Wait for available cycle on CPU semaphore
          (sb-thread:wait-on-semaphore (cycle-state-cpu-semaphore cycle-state))
          ;; Handle instruction cycles
          (if (> cycles-needed 0)
              (decf cycles-needed)
              (multiple-value-bind (next-pc cycles) 
                  (execute cpu mmu (cpu-pc cpu))
                (setf (cpu-pc cpu) next-pc
                      cycles-needed (1- cycles)))))))

(defun run-ppu-thread (gb)
  "PPU thread that waits for full dots (4 cycles) at a time"
  (let* ((ppu (gameboy-ppu gb))
         (mmu (gameboy-mmu gb))
         (cycle-state (gameboy-clock-state gb)))
    
    (loop while *emulator-running* do
          ;; Process each scanline
          (dotimes (scanline 154)  ; 144 visible + 10 vblank
            (dotimes (dot 456)
              ;; Wait for 4 cycles that make up this dot
              (dotimes (i 4)
                (sb-thread:wait-on-semaphore (cycle-state-ppu-semaphore cycle-state)))
              
              ;; Update PPU state with current scanline and dot
              (update-ppu-state ppu mmu scanline dot))))))

(defun dummy-thread (gb))
(defun emulator-main (gb)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "LISPBOY" :w 800 :h 600)
      (sdl2:with-renderer (renderer win)
        ;; Start game logic thread
        (let* ((ppu (gameboy-ppu gb))
               (texture (sdl2:create-texture renderer :rgb888 :streaming 
                                             +screen-width+ +screen-height+))
               (timing-thread (bt:make-thread 
                               (lambda () (run-clock-thread gb))
                               :name "timing-thread"))
               (display-thread (bt:make-thread (lambda () (run-ppu-thread gb)) :name "display-thread"))
               (cpu-thread (bt:make-thread (lambda () (run-cpu-thread gb)) :name "cpu-thread")))

          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (sdl2:update-texture texture nil
                                  (ppu-framebuffer ppu)
                                  (* 4 +screen-width+))
             
             ;; Clear renderer and copy texture
             (sdl2:set-render-draw-color renderer 0 0 0 255)
             (sdl2:render-clear renderer)
             (sdl2:render-copy renderer texture)
             (sdl2:render-present renderer))
            (:quit
             ()
             (setf *emulator-running* nil) t)))))))


(defun run-cpu-test-rom ()
    (let ((source-dir (asdf:system-source-directory (asdf:find-system :lispboy))))
      (format t "~%Starting CPU test ROM execution...~%")
      (finish-output)
      (handler-case
          (load-cartridge a-gameboy (merge-pathnames #P"./roms/snake.gb" source-dir))
        (error (c)
          (format t "~%Error occurred: ~A~%" c)
          (finish-output)))))

(export '(run-cpu-test-rom))
