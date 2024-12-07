(in-package :lispboy)

(defstruct cycle-manager
  (cycle-count 0 :type integer)
  (target-cycles 0 :type integer)
  (running nil :type boolean)
  (lock (bt:make-lock))
  (condition (bt:make-condition-variable)))

(defparameter *cycles-per-second* 4194304) ; 4.194304 MHz
(defparameter *nanoseconds-per-cycle* (/ 1000000000 *cycles-per-second*))

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

(defun cycle-counter-loop (manager)
  "Main loop for cycle counting thread"
  (let ((last-time (get-internal-real-time)))
    (loop while (cycle-manager-running manager) do
      (let* ((current-time (get-internal-real-time))
             (elapsed-time (- current-time last-time))
             (elapsed-cycles (round (* elapsed-time
                                    (/ *cycles-per-second*
                                       internal-time-units-per-second)))))
        (when (> elapsed-cycles 0)
          (bt:with-lock-held ((cycle-manager-lock manager))
            (incf (cycle-manager-cycle-count manager) elapsed-cycles)
            ; Notify any waiting threads if we've hit target
            (when (>= (cycle-manager-cycle-count manager)
                     (cycle-manager-target-cycles manager))
              (bt:condition-notify (cycle-manager-condition manager))))
          (setf last-time current-time)))
      ; Small sleep to prevent spinning
      (sleep 0.000001)))) ; 1 microsecond

(defun start-cycle-manager ()
  "Create and start a new cycle manager"
  (let ((manager (make-cycle-manager)))
    (setf (cycle-manager-running manager) t)
    ; Start cycle counting thread
    (bt:make-thread
     (lambda ()
       (cycle-counter-loop manager))
     :name "CPU Cycle Counter")
    manager))

(defun wait-cycles (manager cycles)
  "Wait for a specified number of cycles to elapse"
  (bt:with-lock-held ((cycle-manager-lock manager))
    (let ((target (+ (cycle-manager-cycle-count manager) cycles)))
      (setf (cycle-manager-target-cycles manager) target)
      (loop while (< (cycle-manager-cycle-count manager) target) do
        (bt:condition-wait (cycle-manager-condition manager)
                          (cycle-manager-lock manager))))))