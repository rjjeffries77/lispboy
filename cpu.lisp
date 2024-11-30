(in-package :lispboy)

(defmacro defcpu (name (&rest registers))
  `(defstruct ,name
     ,@(loop for reg in registers 
           collect `(,reg 0 :type (unsigned-byte 8)))
           (PC 0 :type (unsigned-byte 16))
           (SP 0 :type (unsigned-byte 16))))

(defcpu cpu (A B C D E F H L))

;(defvar gameboy-cpu (make-cpu))

(defun read-byte-at (mmu address)
  "Read byte at specific address without changing PC"
  (read-memory mmu address))

(defun read-word-at (mmu address)
  "Read 16-bit word at address"
  (let ((low-byte (read-memory mmu address))
        (high-byte (read-memory mmu (1+ address))))
    (logior (ash high-byte 8) low-byte)))

(defun write-byte-at (mmu address value) 
  "Write byte at specific address without changing PC"
  (write-memory mmu address value))

;; get an address from a register pair.
(defun get-address-from-register-pair (cpu reg-high reg-low)
  (let ((high-byte (funcall (intern (format nil "CPU-~A" reg-high)) cpu))
        (low-byte (funcall (intern (format nil "CPU-~A" reg-low)) cpu)))
    (logior (ash high-byte 8) low-byte)))

;; Get the mnemonic associative array for the instruction.
(defun get-opcode (mmu pc)
   (let ((byte-at-pc (read-byte-at mmu pc)))
    (if (= byte-at-pc #xCB)
      (gethash (read-byte-at mmu (1+ pc)) cbprefixed-opcodes)
       (or (gethash byte-at-pc unprefixed-opcodes)
          ;; Handle illegal opcodes
          `((:mnemonic . "ILLEGAL")
            (:bytes . 1)
            (:operands))))))

(defun read-operand-value (mmu pc operand)
  "Read operand value from memory based on operand type"
  (let ((bytes (cdr (assoc :bytes operand))))
    (case bytes
      (1 (format nil "#x~2,'0X" (read-byte-at mmu (1+ pc))))
      (2 (format nil "#x~4,'0X" (read-word-at mmu (1+ pc))))
      (otherwise nil))))

(defun format-instruction (mmu pc opcode operands)
  "Format a complete instruction with its operands and values"
  (let* ((mnemonic (cdr (assoc :mnemonic opcode)))
         (formatted-operands 
           (loop for op in operands
                 for value = (read-operand-value mmu pc op)
                 collect (if value
                           (format nil "~A" value)
                           (format-operand op)))))
    (format nil "~A ~{~A~^, ~}" 
            mnemonic 
            formatted-operands)))

(defun disassemble-instruction (mmu pc)
  "Disassemble single instruction at PC, return formatted string and instruction length"
  (let* ((opcode (get-opcode mmu pc))
         (bytes (cdr (assoc :bytes opcode)))
         (operands (get-operands opcode)))
    (values
     (format-instruction mmu pc opcode operands)
     bytes)))

(defun execute (cpu mmu pc)
  "Execute instruction at PC, return next PC value"
  (setf (cpu-pc cpu) pc) ; This is the only place where the PC is set,
  (let* ((opcode (get-opcode mmu pc))
         (bytes (cdr (assoc :bytes opcode)))
         (operands (get-operands opcode)))
    
    ;; Debug output
    (multiple-value-bind (instruction-text _)
        (disassemble-instruction mmu pc)
      (format t "~4,'0X: ~A~%" pc instruction-text))
     (execute-instruction cpu mmu opcode operands)))

(defun execute-instruction (cpu mmu opcode operands)
  "Execute a single instruction"
  (let* ((mnemonic (cdr (assoc :mnemonic opcode)))
         (bytes (cdr (assoc :bytes opcode)))
         (current-pc (cpu-pc cpu))
         (next-pc (+ current-pc bytes))) ; Default next PC is current + instruction length

    (case (intern mnemonic :keyword)
      (:NOP next-pc)
      (:LD (execute-ld cpu mmu operands)
           next-pc)
      (:LDH (execute-ldh cpu mmu operands) 
            next-pc)
      (:INC (execute-inc cpu mmu operands)
            next-pc)
      (:DEC (execute-dec cpu mmu operands)
            next-pc)
      (:PUSH (execute-push cpu mmu operands) next-pc)
      (:POP (execute-pop cpu mmu operands) next-pc)
      (:DI next-pc) ; disable interrupts
      ;; JP/Call return their own 'next pc'
      (:JP (execute-jp cpu mmu operands)) 
      (:CALL (execute-call cpu mmu operands))
      (:RET (execute-ret cpu mmu operands))
      (:JR (execute-jr cpu mmu operands))
      (otherwise 
       (format t "Warning: Unimplemented instruction: ~A~%" mnemonic)
       next-pc))))

(defun execute-ld (cpu mmu operands)
  "Handle LD instructions"
  (let* ((dest (first operands))
         (src (second operands))
         (dest-name (cdr (assoc :name dest)))
         (src-name (cdr (assoc :name src))))
    ;; Handle different LD variants
    (cond
      ;; LD r,n - Load immediate 8-bit value into register
       ((and (register-p dest-name) 
            (equal src-name "n8"))
       (setf (cpu-register cpu dest-name)
             (read-byte-at mmu (1+ (cpu-pc cpu)))))
      
      ;; LD r,r - Load register to register
      ((and (register-p dest-name)
            (register-p src-name))
       (setf (cpu-register cpu dest-name)
             (cpu-register cpu src-name)))
      
      ;; Other LD variants to be implemented
      (t (format t "Unhandled LD variant~%")))))


(defun execute-ldh (cpu mmu operands)
  "Handle LDH instructions"
  (let* ((dest (first operands))
         (src (second operands))
         (dest-name (cdr (assoc :name dest)))
         (src-name (cdr (assoc :name src)))
         (address (+ #xff00 (read-byte-at mmu (1+ (cpu-pc cpu))))))
    ;; Only 2 possibilities here into or out of a register
    (if (register-p dest-name) 
      (setf (cpu-register cpu dest-name) (read-byte-at mmu address))
      (write-byte-at mmu address (cpu-register cpu src-name)))))

(defun execute-inc (cpu mmu operands) 
  "Handle INC instructions"
  (let* ((target (first operands))
         (target-name (cdr (assoc :name target))))
    (when (register-p target-name)
      (let ((result (logand (1+ (cpu-register cpu target-name)) #xFF)))
        (setf (cpu-register cpu target-name) result)
        ;; Set flags
        (setf (cpu-flag cpu :z) (zerop result))
        (setf (cpu-flag cpu :n) nil)
        (setf (cpu-flag cpu :h) (= (logand result #x0F) 0))))))

(defun execute-dec (cpu mmu operands)
  "Handle DEC instructions"
  (let* ((target (first operands))
         (target-name (cdr (assoc :name target))))
    (when (register-p target-name)
      (let ((result (logand (1- (cpu-register cpu target-name)) #xFF)))
        (setf (cpu-register cpu target-name) result)
        ;; Set flags
        (setf (cpu-flag cpu :z) (zerop result))
        (setf (cpu-flag cpu :n) t)
        (setf (cpu-flag cpu :h) (= (logand result #x0F) #xF))))))

(defun execute-jp (cpu mmu operands)
  "Handle JP instructions"
  (let ((condition (if (= (length operands) 2) (first operands) nil))
        (next-pc (+ 3 (cpu-pc cpu)))
        (next-addr (read-word-at mmu (1+ (cpu-pc cpu)))))
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
         next-addr
         next-pc)))

(defun execute-jr (cpu mmu operands)
  "Handle JR instructions"
  (let ((condition (if (= (length operands) 2) (first operands) nil))
         (next-pc (+ 3 (cpu-pc cpu)))
         (relative-addr (read-word-at mmu (1+ (cpu-pc cpu)))))
     (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        (+ (cpu-pc cpu) relative-addr)
        next-pc)))
        
(defun execute-pop (cpu mmu operands)
  "Handle POP instructions"
  (let* ((target (first operands))
         (target-name (cdr (assoc :name target))))
    ;; Get value from stack
    (let ((value (pop-word cpu mmu)))
      ;; Increment SP by 2
      (setf (cpu-sp cpu) (logand (+ (cpu-sp cpu) 2) #xFFFF))
      ;; Store value in register pair
      (case (intern target-name :keyword)
        (:BC (setf (cpu-register cpu "B") (ash value -8)
                  (cpu-register cpu "C") (logand value #xFF)))
        (:DE (setf (cpu-register cpu "D") (ash value -8)
                  (cpu-register cpu "E") (logand value #xFF)))
        (:HL (setf (cpu-register cpu "H") (ash value -8)
                  (cpu-register cpu "L") (logand value #xFF)))
        (:AF (setf (cpu-register cpu "A") (ash value -8)
                  ;; Only upper 4 bits of F are used
                  (cpu-register cpu "F") (logand value #xF0)))))))

(defun execute-push (cpu mmu operands)
  "Handle PUSH instructions"
  (let* ((source (first operands))
         (source-name (cdr (assoc :name source)))
         (value (case (intern source-name :keyword)
                 (:BC (logior (ash (cpu-register cpu "B") 8)
                            (cpu-register cpu "C")))
                 (:DE (logior (ash (cpu-register cpu "D") 8)
                            (cpu-register cpu "E")))
                 (:HL (logior (ash (cpu-register cpu "H") 8)
                            (cpu-register cpu "L")))
                 (:AF (logior (ash (cpu-register cpu "A") 8)
                            (cpu-register cpu "F"))))))
        (push-word cpu mmu value)))

(defun execute-call (cpu mmu operands)
  "Handle CALL instructions"
  (let* ((condition (if (= (length operands) 2) (first operands) nil))
         (next-pc (+ 3 (cpu-pc cpu)))
         (target-addr (read-word-at mmu (1+ (cpu-pc cpu)))))
    
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        (progn
          (push-word cpu mmu next-pc)  ; Save return address
          target-addr)                  ; Jump to target
        nil)))                     ; Skip if condition false

(defun execute-ret (cpu mmu operands)
  "Handle RET instructions"
  (let ((condition (if operands (first operands) nil)))
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        (pop-word cpu mmu)
        nil)))  ; Skip 

;; Helper functions
(defun register-p (name)
  "Check if name refers to a single register"
  (member name '("A" "B" "C" "D" "E" "H" "L" "F") :test #'string=))

(defun cpu-register (cpu reg-name)
  "Get register value by name"
  (slot-value cpu (intern reg-name :lispboy)))

(defun (setf cpu-register) (value cpu reg-name)
  "Set register value by name"
  (setf (slot-value cpu (intern reg-name :lispboy)) value))

(defun check-condition (cpu cond-name)
  "Check jump/call condition based on CPU flags
   NZ - Not Zero
   Z  - Zero
   NC - Not Carry
   C  - Carry"
  (case (intern cond-name :keyword)
    (:NZ (not (cpu-flag cpu :z)))  ; Not Zero
    (:Z  (cpu-flag cpu :z))        ; Zero
    (:NC (not (cpu-flag cpu :c)))  ; Not Carry
    (:C  (cpu-flag cpu :c))        ; Carry
    (otherwise t)))        

(defun cpu-flag (cpu flag)
  "Get flag value (stored in F register bits)"
  (case flag
    (:z (logbitp 7 (cpu-f cpu)))
    (:n (logbitp 6 (cpu-f cpu)))
    (:h (logbitp 5 (cpu-f cpu)))
    (:c (logbitp 4 (cpu-f cpu)))))

(defun (setf cpu-flag) (value cpu flag)
  "Set flag value"
  (let ((mask (case flag
                (:z #x80)
                (:n #x40)
                (:h #x20)
                (:c #x10))))
    (setf (cpu-f cpu)
          (if value
              (logior (cpu-f cpu) mask)
              (logand (cpu-f cpu) (lognot mask))))))

(defun push-word (cpu mmu value)
  "Push a 16-bit value onto the stack"
  (setf (cpu-sp cpu) (logand (- (cpu-sp cpu) 2) #xFFFF))
  (write-memory mmu (cpu-sp cpu) (logand value #xFF))
  (write-memory mmu (1+ (cpu-sp cpu)) (ash value -8)))

(defun pop-word (cpu mmu)
  "Pop a 16-bit value from the stack"
  (let ((value (read-word-at mmu (cpu-sp cpu))))
    (setf (cpu-sp cpu) (logand (+ (cpu-sp cpu) 2) #xFFFF))
    value))
; testing