(in-package :lispboy)

(defmacro defcpu (name (&rest registers))
  `(defstruct ,name
     ,@(loop for reg in registers 
           collect `(,reg 0 :type (unsigned-byte 8)))
           (PC 0 :type (unsigned-byte 16))
           (SP 0 :type (unsigned-byte 16))
           (cycle-manager (make-cycle-manager))
           (ime nil :type boolean)))

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
  (let ((high-byte (funcall (intern (format nil "CPU-~A" reg-high) :lispboy)  cpu))
        (low-byte (funcall (intern (format nil "CPU-~A" reg-low) :lispboy)  cpu)))
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

(defun format-operand (operand mmu pc)
  (let ((name (cdr (assoc :name operand))))
    (case name
      ("n8" (read-byte-at mmu (1+ pc)))
      ("n16" (read-word-at mmu (1+ pc)))
      ("d8" (read-byte-at mmu (1+ pc)))
      ("a8" (read-byte-at mmu (1+ pc)))
      ("a16" (read-word-at mmu (1+ pc)))
      (otherwise name))))

(defun format-instruction (mmu pc opcode operands)
  "Format a complete instruction with its operands and values"
  (let* ((mnemonic (cdr (assoc :mnemonic opcode)))
         (formatted-operands 
           (loop for op in operands
                 for value = (read-operand-value mmu pc op)
                 collect (if value
                           (format nil "~A" value)
                           (format-operand op mmu pc)))))
    (format nil "~A ~{~A~^, ~}" 
            mnemonic 
            formatted-operands)))

(defun disassemble-instruction (mmu pc)
  "Disassemble single instruction at PC, return formatted string and instruction length"
  (let* ((opcode (get-opcode mmu pc))
         (operands (get-operands opcode)))
     (format-instruction mmu pc opcode operands)))

(defun execute (cpu mmu pc)
  "Execute instruction at PC, return next PC value"
  (setf (cpu-pc cpu) pc) ; This is the only place where the PC is set,
  (let* ((opcode (get-opcode mmu pc))
         (operands (get-operands opcode)))
    
    ;; Debug output
    (let ((instruction-text (disassemble-instruction mmu pc)))
      (format t "~4,'0X: ~A~%" pc instruction-text))
     (execute-instruction cpu mmu opcode operands)))

(defun execute-instruction (cpu mmu opcode operands)
  "Execute a single instruction"
  (let* ((mnemonic (cdr (assoc :mnemonic opcode)))
         (bytes (cdr (assoc :bytes opcode)))
         (current-pc (cpu-pc cpu))
         (cycles (first (cdr (assoc :cycles opcode))))
         (next-pc (+ current-pc bytes))) ; Default next PC is current + instruction length

    (case (intern mnemonic :keyword)
      (:NOP (values next-pc cycles))
      (:LD (execute-ld cpu mmu operands)
           (values next-pc cycles))
      (:LDH (execute-ldh cpu mmu operands) 
            (values next-pc cycles))
      (:INC (execute-inc cpu operands)
       (values next-pc cycles))
      (:RRA (execute-rra cpu operands)
       (values next-pc cycles))
      (:DEC (execute-dec cpu operands) (values next-pc cycles))
      (:OR (execute-or cpu operands) (values next-pc cycles))
      (:ADD (execute-add cpu mmu operands) (values next-pc cycles))
      (:ADC (execute-adc cpu mmu operands) (values next-pc cycles))
      (:CP (execute-cp cpu mmu operands) (values next-pc cycles))
      (:SUB (execute-sub cpu mmu operands) (values next-pc cycles))
      (:SBC (execute-sbc cpu mmu operands) (values next-pc cycles))
      (:PUSH (execute-push cpu mmu operands) (values next-pc cycles))
      (:POP (execute-pop cpu mmu operands) (values next-pc cycles))
      (:DI (setf (cpu-ime cpu) nil)  ; Disable interrupts
           (values next-pc cycles))
      
      (:EI (setf (cpu-ime cpu) t)    ; Enable interrupts 
           (values next-pc cycles))
      (:RETI (setf (cpu-ime cpu) t)    ; Re-enable interrupts
             (values (pop-word cpu mmu) cycles)) 
      ;; JP/Call return their own 'next pc'
      (:JP (execute-jp cpu mmu operands (cdr (assoc :cycles opcode)))) 
      (:CALL (execute-call cpu mmu operands (cdr (assoc :cycles opcode))))
      (:RET (execute-ret cpu mmu operands (cdr (assoc :cycles opcode))))
      (:JR (execute-jr cpu mmu operands (cdr (assoc :cycles opcode))))
      (otherwise 
       (format t "Warning: Unimplemented instruction: ~A~%" mnemonic)
       (values next-pc 1)))))

(defun execute-ld (cpu mmu operands)
  "Handle LD instructions"
  (let* ((dest (first operands))
         (src (second operands))
         (dest-name (cdr (assoc :name dest)))
         (src-name (cdr (assoc :name src)))
         (dest-immediate (cdr (assoc :immediate dest)))
         (src-immediate (cdr (assoc :immediate src)))
         (dest-increment (cdr (assoc :increment dest)))
         (src-increment (cdr (assoc :increment src)))
         (dest-decrement (cdr (assoc :decrement dest)))
         (src-decrement (cdr (assoc :decrement src))))
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
      
      ;; LD r,[HL] - Load from memory into register
      ((and (register-p dest-name)
            (equal src-name "HL")
            (not src-immediate))
       (setf (cpu-register cpu dest-name)
             (read-byte-at mmu (get-address-from-register-pair cpu "H" "L"))))
      
      ;; Ld [HL],r - Load register into memory
      ((and (equal dest-name "HL")
            (not dest-immediate)
            (register-p src-name))
       (write-byte-at mmu 
                      (get-address-from-register-pair cpu "H" "L")
                      (cpu-register cpu src-name)))
      
      ;; LD [HL],n - Load immediate value into memory
      ((and (equal dest-name "HL")
            (not dest-immediate)
            (equal src-name "n8"))
       (write-byte-at mmu
                     (get-address-from-register-pair cpu "H" "L")
                     (read-byte-at mmu (1+ (cpu-pc cpu)))))
      
      ;; LD [BC/DE],A - Load A into memory pointed by BC or DE
      ((and (member dest-name '("BC" "DE") :test #'equal)
            (not dest-immediate)
            (equal src-name "A"))
       (let ((addr (get-address-from-register-pair cpu 
                                                  (char dest-name 0)
                                                  (char dest-name 1))))
         (write-byte-at mmu addr (cpu-register cpu "A"))))
      
      ;; LD A,[BC/DE] - Load from memory pointed by BC or DE into A
      ((and (equal dest-name "A")
            (member src-name '("BC" "DE") :test #'equal)
            (not src-immediate))
       (let ((addr (get-address-from-register-pair cpu 
                                                  (char src-name 0)
                                                  (char src-name 1))))
         (setf (cpu-register cpu "A")
               (read-byte-at mmu addr))))
      
      ;; LD [HL+],A or LD [HLI],A - Load A into memory and increment HL
      ((and (equal dest-name "HL")
            dest-increment
            (equal src-name "A"))
       (let ((addr (get-address-from-register-pair cpu 'H 'L)))
         (write-byte-at mmu addr (cpu-register cpu "A"))
         (let ((new-hl (logand (1+ addr) #xFFFF)))
           (setf (cpu-register cpu "H") (ash new-hl -8)
                 (cpu-register cpu "L") (logand new-hl #xFF)))))
      
      ;; LD [HL-],A or LD [HLD],A - Load A into memory and decrement HL
      ((and (equal dest-name "HL")
            dest-decrement
            (equal src-name "A"))
       (let ((addr (get-address-from-register-pair cpu 'H 'L)))
         (write-byte-at mmu addr (cpu-register cpu "A"))
         (let ((new-hl (logand (1- addr) #xFFFF)))
           (setf (cpu-register cpu "H") (ash new-hl -8)
                 (cpu-register cpu "L") (logand new-hl #xFF)))))
      
      ;; LD A,[HL+] or LD A,[HLI] - Load from memory into A and increment HL
      ((and (equal dest-name "A")
            (equal src-name "HL")
            src-increment)
       (let ((addr (get-address-from-register-pair cpu 'H 'L)))
         (setf (cpu-register cpu "A") (read-byte-at mmu addr))
         (let ((new-hl (logand (1+ addr) #xFFFF)))
           (setf (cpu-register cpu "H") (ash new-hl -8)
                 (cpu-register cpu "L") (logand new-hl #xFF)))))
      
      ;; LD A,[HL-] or LD A,[HLD] - Load from memory into A and decrement HL
      ((and (equal dest-name "A")
            (equal src-name "HL")
            src-decrement)
       (let ((addr (get-address-from-register-pair cpu "H" "L")))
         (setf (cpu-register cpu "A") (read-byte-at mmu addr))
         (let ((new-hl (logand (1- addr) #xFFFF)))
           (setf (cpu-register cpu "H") (ash new-hl -8)
                 (cpu-register cpu "L") (logand new-hl #xFF)))))
      
      ;; LD rr,nn - Load 16-bit immediate value into register pair
      ((and (member dest-name '("BC" "DE" "HL" "SP") :test #'equal)
            (equal src-name "n16"))
       (let ((value (read-word-at mmu (1+ (cpu-pc cpu)))))
         (if (equal dest-name "SP")
             (setf (cpu-sp cpu) value)
             (let ((high (char dest-name 0))
                   (low (char dest-name 1)))
               (setf (cpu-register cpu (string high)) (ash value -8)
                     (cpu-register cpu (string low)) (logand value #xFF))))))

      ((and (equal dest-name "HL")
            (equal src-name "SP")
            (assoc :increment src))  ; This identifies the SP+e8 variant
       (let* ((offset (read-byte-at mmu (1+ (cpu-pc cpu))))
              ;; Convert to signed byte (-128 to 127)
              (signed-offset (if (> offset 127)
                                 (- offset 256)
                                 offset))
              (result (+ (cpu-sp cpu) signed-offset)))
         ;; Store result in HL
         (setf (cpu-register cpu "H") (ash (logand result #xFFFF) -8)
               (cpu-register cpu "L") (logand result #xFF))
         ;; Set flags according to GB CPU manual for this instruction
         (setf (cpu-flag cpu :z) nil      ; Reset
               (cpu-flag cpu :n) nil      ; Reset
               ;; Half carry if carry from bit 3
               (cpu-flag cpu :h) (> (logand (logxor (cpu-sp cpu) 
                                                    signed-offset 
                                                    result)
                                            #x10)
                                    0)
               ;; Carry if carry from bit 7
               (cpu-flag cpu :c) (> (logand (logxor (cpu-sp cpu) 
                                                    signed-offset 
                                                    result)
                                            #x100)
                                    0))))

      ;; LD [nn], r - Load 8 bit value at [nn] into register r
      ((and (equal dest-name "a16")
            (register-p src-name))
       (let* ((address (read-word-at mmu (1+ (cpu-pc cpu))))
             (value (read-byte-at mmu address)))
         (setf (cpu-register cpu src-name) value)))
      ;; Other LD variants to be implemented
      (t (format t "Unhandled LD variant, operands: ~A~%" operands)))))


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

(defun execute-inc (cpu operands) 
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

(defun execute-dec (cpu operands)
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

(defun execute-jp (cpu mmu operands cycles)
  "Handle JP instructions"
  (let ((condition (if (= (length operands) 2) (first operands) nil))
        (next-pc (+ 3 (cpu-pc cpu)))
        (next-addr (read-word-at mmu (1+ (cpu-pc cpu)))))
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
         (values next-addr (first cycles))  ; Jump to target
         (values next-pc (second cycles)))))

(defun execute-jr (cpu mmu operands cycles)
  "Handle JR instructions"
  (let* ((condition (if (= (length operands) 2) (first operands) nil))
         (next-pc (+ 2 (cpu-pc cpu)))  ; JR is only 2 bytes
         (offset (read-byte-at mmu (1+ (cpu-pc cpu))))  ; Read single byte offset
         ;; Convert to signed byte (-128 to 127)
         (relative-offset (if (> offset 127)
                              (- offset 256)
                              offset)))
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        ;; If condition is met, add offset to current PC + 2
        (values (logand #xFFFF (+ next-pc relative-offset)) (first cycles))
        ;; If condition not met, go to next instruction
        (values next-pc (second cycles)))))
        
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

(defun execute-call (cpu mmu operands cycles)
  "Handle CALL instructions"
  (let* ((condition (if (= (length operands) 2) (first operands) nil))
         (next-pc (+ 3 (cpu-pc cpu)))
         (target-addr (read-word-at mmu (1+ (cpu-pc cpu)))))
    
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        (progn
          (push-word cpu mmu next-pc)  ; Save return address
          (values target-addr (first cycles)))                  ; Jump to target
        (values next-pc (second cycles)))))                    ; Skip if condition false

(defun execute-ret (cpu mmu operands cycles)
  "Handle RET instructions"
  (let ((condition (if operands (first operands) nil))
        (next-pc (+ 1 (cpu-pc cpu))))
    (if (or (null condition)
            (check-condition cpu (cdr (assoc :name condition))))
        (values (pop-word cpu mmu) (first cycles))
        (values next-pc (second cycles)))))  ; Skip 

(defun execute-or (cpu operands)
  "Execute OR instruction"
  (let* ((source (second operands))
         (source-name (cdr (assoc :name source)))
         (source-value (if (register-p source-name)
                           (cpu-register cpu source-name)
                           source-name)))
    ;; OR always targets register A
    (let ((result (logior (cpu-register cpu "A") source-value)))
      (setf (cpu-register cpu "A") (logand result #xFF))
      (setf (cpu-flag cpu :z) (zerop result)
            (cpu-flag cpu :n) nil
            (cpu-flag cpu :h) nil
            (cpu-flag cpu :c) nil))))

(defun execute-add (cpu mmu operands)
  "Execute ADD instruction"
  (let* ((target (first operands))
         (source (second operands))
         (target-name (cdr (assoc :name target)))
         (source-name (cdr (assoc :name source))))

    (cond
      ;; ADD A,r8 or ADD A,n8 - 8-bit addition to A register
      ((equal target-name "A")
       (let* ((source-value (cond
                              ((register-p source-name)
                               (cpu-register cpu source-name))
                              ((equal source-name "n8")
                               (read-byte-at mmu (1+ (cpu-pc cpu))))
                              ((equal source-name "HL")
                               (read-byte-at mmu 
                                             (get-address-from-register-pair cpu 'H 'L)))
                              (t source-name)))
              (result (+ (cpu-register cpu "A") source-value))
              (half-carry (> (logand (logxor (cpu-register cpu "A") 
                                             source-value 
                                             result) 
                                     #x10) 
                             0)))
         ;; Store result
         (setf (cpu-register cpu "A") (logand result #xFF))
         ;; Set flags
         (setf (cpu-flag cpu :z) (zerop (logand result #xFF))
               (cpu-flag cpu :n) nil
               (cpu-flag cpu :h) half-carry
               (cpu-flag cpu :c) (> result #xFF))))

      ;; ADD HL,rr - 16-bit addition to HL register
      ((equal target-name "HL")
       (let* ((hl-value (get-address-from-register-pair cpu 'H 'L))
              (source-value (cond
                              ((member source-name '("BC" "DE" "HL") :test #'equal)
                               (get-address-from-register-pair cpu 
                                                               (char source-name 0)
                                                               (char source-name 1)))
                              ((equal source-name "SP")
                               (cpu-sp cpu))
                              (t source-name)))
              (result (+ hl-value source-value))
              (half-carry (> (logand (logxor hl-value source-value result) 
                                     #x1000) 
                             0)))
         ;; Store result
         (setf (cpu-register cpu "H") (logand (ash result -8) #xFF)
               (cpu-register cpu "L") (logand result #xFF))
         ;; Set flags (preserves Z flag)
         (setf (cpu-flag cpu :n) nil
               (cpu-flag cpu :h) half-carry
               (cpu-flag cpu :c) (> result #xFFFF))))

      ;; ADD SP,e8 - Add signed 8-bit immediate to SP
      ((equal target-name "SP")
       (let* ((offset (read-byte-at mmu (1+ (cpu-pc cpu))))
              ;; Convert to signed
              (signed-offset (if (> offset 127)
                                 (- offset 256)
                                 offset))
              (result (+ (cpu-sp cpu) signed-offset))
              (half-carry (> (logand (logxor (cpu-sp cpu) 
                                             signed-offset 
                                             result)
                                     #x10)
                             0)))
         ;; Store result
         (setf (cpu-sp cpu) (logand result #xFFFF))
         ;; Set all flags
         (setf (cpu-flag cpu :z) nil
               (cpu-flag cpu :n) nil
               (cpu-flag cpu :h) half-carry
               (cpu-flag cpu :c) (> result #xFFFF)))))))

(defun execute-cp (cpu mmu operands)
  "Execute CP (Compare) instruction"
  (let* ((target (first operands))
         (source (second operands))
         (a-value (cpu-register cpu "A"))
         (source-value 
           (cond
             ;; CP with register
             ((register-p (cdr (assoc :name source)))
              (cpu-register cpu (cdr (assoc :name source))))
             
             ;; CP with immediate value (n8)
             ((equal (cdr (assoc :name source)) "n8")
              (read-byte-at mmu (1+ (cpu-pc cpu))))
             
             ;; CP with [HL]
             ((and (equal (cdr (assoc :name source)) "HL")
                   (not (cdr (assoc :immediate source))))
              (read-byte-at mmu (get-address-from-register-pair cpu "H" "L")))
             
             (t (error "Unknown source for CP instruction"))))
         
         ;; Calculate result (but don't store it)
         (result (- a-value source-value)))
    
    (setf (cpu-flag cpu :z) (zerop (logand result #xFF))
          (cpu-flag cpu :n) t
          (cpu-flag cpu :h) (> (logand (logxor a-value source-value result) #x10) 0)
          (cpu-flag cpu :c) (< a-value source-value))))

(defun execute-sub (cpu mmu operands)
  "Execute SUB instruction"
  (let* ((target (first operands))
         (source (second operands))
         (a-value (cpu-register cpu "A"))
         (source-value 
           (cond
             ;; SUB with register
             ((register-p (cdr (assoc :name source)))
              (cpu-register cpu (cdr (assoc :name source))))
             
             ;; SUB with immediate value (n8)
             ((equal (cdr (assoc :name source)) "n8")
              (read-byte-at mmu (1+ (cpu-pc cpu))))
             
             ;; SUB with [HL]
             ((and (equal (cdr (assoc :name source)) "HL")
                   (not (cdr (assoc :immediate source))))
              (read-byte-at mmu (get-address-from-register-pair cpu "H" "L")))
             
             (t (error "Unknown source for SUB instruction"))))
         
         ;; Calculate result
         (result (- a-value source-value)))
    
    ;; Store result in A register
    (setf (cpu-register cpu "A") (logand result #xFF))
    
    (setf (cpu-flag cpu :z) (zerop (logand result #xFF))
          (cpu-flag cpu :n) t
          (cpu-flag cpu :h) (> (logand (logxor a-value source-value result) #x10) 0)
          (cpu-flag cpu :c) (< a-value source-value))))

(defun execute-adc (cpu mmu operands)
  "Execute ADC (Add with Carry) instruction"
  (let* ((target (first operands))
         (source (second operands))
         (a-value (cpu-register cpu "A"))
         (carry-value (if (cpu-flag cpu :c) 1 0))
         (source-value 
           (cond
             ;; ADC with register
             ((register-p (cdr (assoc :name source)))
              (cpu-register cpu (cdr (assoc :name source))))
             
             ;; ADC with immediate value (n8)
             ((equal (cdr (assoc :name source)) "n8")
              (read-byte-at mmu (1+ (cpu-pc cpu))))
             
             ;; ADC with [HL]
             ((and (equal (cdr (assoc :name source)) "HL")
                   (not (cdr (assoc :immediate source))))
              (read-byte-at mmu (get-address-from-register-pair cpu "H" "L")))
             
             (t (error "Unknown source for ADC instruction"))))
         
         ;; Calculate result including carry
         (result (+ a-value source-value carry-value))
         
         ;; Check for half carry: carry from bit 3 to bit 4
         (half-carry (> (logand (+ (logand a-value #xF) 
                                   (logand source-value #xF)
                                   carry-value) 
                                #x10) 
                        0)))
    
    ;; Store result in A register
    (setf (cpu-register cpu "A") (logand result #xFF))
    (setf (cpu-flag cpu :z) (zerop (logand result #xFF))
          (cpu-flag cpu :n) nil
          (cpu-flag cpu :h) half-carry
          (cpu-flag cpu :c) (> result #xFF))))


(defun execute-sbc (cpu mmu operands)
  "Execute SBC (Subtract with Carry) instruction"
  (let* ((target (first operands))
         (source (second operands))
         (a-value (cpu-register cpu "A"))
         (carry-value (if (cpu-flag cpu :c) 1 0))
         (source-value 
           (cond
             ;; SBC with register
             ((register-p (cdr (assoc :name source)))
              (cpu-register cpu (cdr (assoc :name source))))
             
             ;; SBC with immediate value (n8)
             ((equal (cdr (assoc :name source)) "n8")
              (read-byte-at mmu (1+ (cpu-pc cpu))))
             
             ;; SBC with [HL]
             ((and (equal (cdr (assoc :name source)) "HL")
                   (not (cdr (assoc :immediate source))))
              (read-byte-at mmu (get-address-from-register-pair cpu "H" "L")))
             
             (t (error "Unknown source for SBC instruction"))))
         
         ;; Calculate result including borrow (carry)
         (result (- a-value source-value carry-value))
         
         ;; Check for half-borrow: borrow from bit 4
         (half-carry (> (logand (logxor a-value source-value result) #x10) 0)))
    
    ;; Store result in A register
    (setf (cpu-register cpu "A") (logand result #xFF))
    
    ;; Set flags according to GB CPU manual:
    ;; Z - Set if result is zero
    ;; N - Set (indicating subtraction)
    ;; H - Set if borrow from bit 4
    ;; C - Set if borrow needed (result < 0)
    (setf (cpu-flag cpu :z) (zerop (logand result #xFF))
          (cpu-flag cpu :n) t
          (cpu-flag cpu :h) half-carry
          (cpu-flag cpu :c) (< result 0))))

(defun execute-rra (cpu operands)
  "Execute RRA (Rotate Right through Accumulator) instruction"
  (let* ((a-value (cpu-register cpu "A"))
         (old-carry (if (cpu-flag cpu :c) 1 0))
         ;; Get bit 0 to become new carry
         (new-carry (logand a-value 1))
         ;; Rotate right, putting old carry in bit 7
         (result (logior (ash (logand a-value #xFF) -1)
                         (ash old-carry 7))))
    
    ;; Store result in A
    (setf (cpu-register cpu "A") result)
    
    (setf (cpu-flag cpu :z) nil
          (cpu-flag cpu :n) nil
          (cpu-flag cpu :h) nil
          (cpu-flag cpu :c) (= new-carry 1))))


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
