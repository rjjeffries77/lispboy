(in-package :lispboy)

(defstruct cycle-manager
  (cycle-count 0 :type integer)
  (running t :type boolean)
  (last-frame-time (get-nanoseconds) :type integer)
  ;; Separate tracking for CPU and PPU
  (cpu-cycles 0 :type integer)
  (ppu-dots 0 :type integer)  ; PPU dots (1 dot = 4 CPU cycles)
  (frame-count 0 :type integer)
  ;; Synchronization
  (frame-lock (bt:make-lock "frame-lock"))
  (frame-condition (bt:make-condition-variable :name "frame-condition"))
  (cpu-lock (bt:make-lock "cpu-lock"))
  (ppu-lock (bt:make-lock "ppu-lock"))
  (cpu-condition (bt:make-condition-variable :name "cpu-condition"))
  (ppu-condition (bt:make-condition-variable :name "ppu-condition")))


(defconstant +cycles-per-second+ 4194304)
(defconstant +cycles-per-frame+ 70224)  ; 4194304 / 59.73
(defconstant +target-fps+ 59.73)
(defconstant +nanoseconds-per-frame+ (floor (* 1000000000 (/ 1.0 +target-fps+))))
(defconstant +internal-time-units-per-cycle+ 
  (/ internal-time-units-per-second +cycles-per-second+))

;interrupts
(defconstant +vblank-int+ #x01)
(defconstant +lcd-stat-int+ #x02) 
(defconstant +timer-int+ #x04)
(defconstant +serial-int+ #x08)
(defconstant +joypad-int+ #x10)

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

(defun wait-for-next-frame (timer)
  "Wait until it's time for the next frame"
  (let* ((current-time (get-nanoseconds))
         (target-time (+ (frame-timer-last-frame-time timer) 
                         +nanoseconds-per-frame+)))
    (when (< current-time target-time)
      ;; Sleep for most of the remaining time
      (let ((sleep-ns (- target-time current-time 100000))) ; Leave 100μs for precision
        (when (> sleep-ns 0)
          (sleep (/ sleep-ns 1000000000.0))))
      ;; Spin-wait for the remainder
      (loop while (< (get-nanoseconds) target-time)))
    (setf (frame-timer-last-frame-time timer) target-time)
    (incf (frame-timer-frame-count timer))
    (setf (frame-timer-cycles-this-frame timer) 0)))

(defun print-timing-stats (timer)
  (let* ((elapsed-ns (- (get-nanoseconds) 
                        (frame-timer-last-frame-time timer)))
         (elapsed-seconds (/ elapsed-ns 1000000000.0))
         (frames (frame-timer-frame-count timer))
         (actual-fps (/ frames elapsed-seconds)))
    (format t "Frames: ~D~%" frames)
    (format t "Time: ~,2f seconds~%" elapsed-seconds)
    (format t "FPS: ~,2f~%" actual-fps)
    (format t "Target FPS: ~,2f~%" +target-fps+)
    (format t "FPS Accuracy: ~,2f%~%" (* 100 (/ actual-fps +target-fps+)))))
