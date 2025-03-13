(in-package :lispboy)

(defconstant +cpu-cycles-per-frame+ 70224)  ; 4194304 Hz / ~59.73 fps
(defconstant +ppu-dots-per-frame+ 17556)    ; CPU cycles / 4
(defconstant +cycles-per-second+ 4194304)   ; Game Boy CPU frequency
(defconstant +target-fps+ 59.73)            ; Game Boy refresh rate
(defconstant +nanoseconds-per-cycle+ (floor (* 1000000000 (/ 1.0 +cycles-per-second+))))
(defconstant +nanoseconds-per-frame+ (floor (* 1000000000 (/ 1.0 +target-fps+))))

;; Interrupts
(defconstant +vblank-int+ #x01)
(defconstant +lcd-stat-int+ #x02) 
(defconstant +timer-int+ #x04)
(defconstant +serial-int+ #x08)
(defconstant +joypad-int+ #x10)

;; Enhanced cycle state structure
(defstruct cycle-state
  (cpu-cycles 0 :type integer)              ; Current CPU cycle count
  (frame-cycles 0 :type integer)            ; Cycles in current frame
  (last-sync-time 0 :type integer)          ; Last sync point in nanoseconds
  (frame-start-time 0 :type integer)        ; Frame start time in nanoseconds
  (frame-count 0 :type integer)             ; Total frames processed
  (cpu-semaphore (sb-thread:make-semaphore :count 0))
  (ppu-semaphore (sb-thread:make-semaphore :count 0)))

(defun sync-to-cycle-boundary (state)
  "Synchronize execution to cycle boundary using high-resolution timer"
  (let* ((current-time (get-nanoseconds))
         (expected-time (+ (cycle-state-last-sync-time state)
                          (* (cycle-state-cpu-cycles state) +nanoseconds-per-cycle+)))
         (delta (- expected-time current-time)))
    (when (> delta 0)
      ;; If we're ahead of schedule, spin-wait for precise timing
      (loop while (> (- expected-time (get-nanoseconds)) 0)))
    (setf (cycle-state-last-sync-time state) (get-nanoseconds))))

(defun sync-frame-timing (state)
  "Ensure frame timing matches Game Boy refresh rate"
  (let* ((current-time (get-nanoseconds))
         (frame-time (- current-time (cycle-state-frame-start-time state)))
         (target-time +nanoseconds-per-frame+)
         (delta (- target-time frame-time)))
    (when (> delta 0)
      (loop while (> (- target-time (- (get-nanoseconds) 
                                      (cycle-state-frame-start-time state))) 0)))
    (setf (cycle-state-frame-start-time state) (get-nanoseconds))
    (setf (cycle-state-frame-cycles state) 0)
    (incf (cycle-state-frame-count state))))

(defun run-cycle (state)
  "Execute a single CPU cycle with accurate timing"
  (sync-to-cycle-boundary state)
  (incf (cycle-state-cpu-cycles state))
  (incf (cycle-state-frame-cycles state))
  
  ;; Signal CPU and PPU
  (sb-thread:signal-semaphore (cycle-state-cpu-semaphore state))
  
  ;; PPU runs at 1/4 the CPU speed
  (when (zerop (mod (cycle-state-cpu-cycles state) 4))
    (sb-thread:signal-semaphore (cycle-state-ppu-semaphore state)))
  
  ;; Check if we've completed a frame
  (when (>= (cycle-state-frame-cycles state) +cpu-cycles-per-frame+)
    (sync-frame-timing state)))

(defun handle-interrupts (cpu mmu)
  "Check and handle any pending interrupts"
  (when (cpu-ime cpu)  ; Only handle interrupts if IME is set
    (let* ((ie (mmu-ie mmu))         ; Get enabled interrupts
           (if (mmu-if mmu))         ; Get pending interrupts  
           (pending (logand ie if))) ; Get interrupts that are both enabled and pending
      
      (when (not (zerop pending))
        ;; Handle interrupts in priority order
        (cond
          ;; V-Blank
          ((not (zerop (logand pending +vblank-int+)))
           (handle-interrupt cpu mmu #x40 +vblank-int+))
          
          ;; LCD STAT
          ((not (zerop (logand pending +lcd-stat-int+)))
           (handle-interrupt cpu mmu #x48 +lcd-stat-int+))
          
          ;; Timer
          ((not (zerop (logand pending +timer-int+)))
           (handle-interrupt cpu mmu #x50 +timer-int+))
          
          ;; Serial
          ((not (zerop (logand pending +serial-int+)))
           (handle-interrupt cpu mmu #x58 +serial-int+))
          
          ;; Joypad
          ((not (zerop (logand pending +joypad-int+)))
           (handle-interrupt cpu mmu #x60 +joypad-int+)))))))

(defun handle-interrupt (cpu mmu vector int-bit)
  "Handle a specific interrupt"
  (setf (cpu-ime cpu) nil)  ; Disable interrupts
  (setf (mmu-if mmu) 
        (logand (mmu-if mmu) (lognot int-bit)))
  (push-word cpu mmu (cpu-pc cpu))
  (setf (cpu-pc cpu) vector))

(defun print-timing-stats (state)
  "Print detailed timing statistics"
  (let* ((elapsed-ns (- (get-nanoseconds) 
                       (cycle-state-frame-start-time state)))
         (elapsed-seconds (/ elapsed-ns 1000000000.0))
         (frames (cycle-state-frame-count state))
         (actual-fps (/ frames elapsed-seconds))
         (cycle-accuracy (/ (cycle-state-cpu-cycles state) 
                          (* elapsed-seconds +cycles-per-second+))))
    (format t "~&Timing Statistics:~%")
    (format t "Frames: ~D~%" frames)
    (format t "Time: ~,2f seconds~%" elapsed-seconds)
    (format t "FPS: ~,2f~%" actual-fps)
    (format t "Target FPS: ~,2f~%" +target-fps+)
    (format t "FPS Accuracy: ~,2f%~%" (* 100 (/ actual-fps +target-fps+)))
    (format t "Cycle Accuracy: ~,2f%~%" (* 100 cycle-accuracy))))
