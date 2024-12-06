(in-package :lispboy)

(defun test-get-opcode ()
  (format t "~%Testing get-opcode...~%")
  
  ;; Create a test MMU with some known opcodes
  (let ((test-mmu (make-mmu)))
    ;; Test case 1: NOP (0x00)
    (write-memory test-mmu 0 #x00)
    (let ((opcode (get-opcode test-mmu 0)))
      (assert (string= (cdr (assoc :mnemonic opcode)) "NOP")
              nil
              "Expected NOP for opcode 0x00, got ~A" 
              (cdr (assoc :mnemonic opcode))))
    
    ;; Test case 2: LD BC,nn (0x01)
    (write-memory test-mmu 1 #x01)
    (let ((opcode (get-opcode test-mmu 1)))
      (assert (string= (cdr (assoc :mnemonic opcode)) "LD")
              nil
              "Expected LD for opcode 0x01, got ~A"
              (cdr (assoc :mnemonic opcode))))
    
    ;; Test case 3: CB prefix
    (write-memory test-mmu 2 #xCB)
    (write-memory test-mmu 3 #x00)  ;; RLC B
    (let ((opcode (get-opcode test-mmu 2)))
      (assert (string= (cdr (assoc :mnemonic opcode)) "RLC")
              nil
              "Expected RLC for CB-prefixed opcode 0x00, got ~A"
              (cdr (assoc :mnemonic opcode))))
    
    (format t "get-opcode tests passed!~%")))

(defun test-format-operand ()
  (format t "~%Testing format-operand...~%")
  
  ;; Test immediate operand
  (let ((immediate-op '((:name . "A") (:immediate . t))))
    (assert (string= (format-operand immediate-op) "A")
            nil
            "Expected 'A', got ~A" 
            (format-operand immediate-op)))
  
  ;; Test memory reference operand
  (let ((memory-op '((:name . "HL") (:immediate . nil))))
    (assert (string= (format-operand memory-op) "[HL]")
            nil
            "Expected '[HL]', got ~A"
            (format-operand memory-op)))
  
  (format t "format-operand tests passed!~%"))

(defun test-format-instruction ()
  (format t "~%Testing format-instruction...~%")
  
  ;; Test NOP (no operands)
  (let ((nop-opcode '((:mnemonic . "NOP")))
        (nop-operands nil))
    (assert (string= (format-instruction nop-opcode nop-operands) 
                    "NOP ")
            nil
            "Expected 'NOP ', got ~A"
            (format-instruction nop-opcode nop-operands)))
  
  ;; Test LD A,B
  (let ((ld-opcode '((:mnemonic . "LD")))
        (ld-operands '(((:name . "A") (:immediate . t))
                      ((:name . "B") (:immediate . t)))))
    (assert (string= (format-instruction ld-opcode ld-operands)
                    "LD A, B")
            nil
            "Expected 'LD A, B', got ~A"
            (format-instruction ld-opcode ld-operands)))
  
  ;; Test LD A,(HL)
  (let ((ld-mem-opcode '((:mnemonic . "LD")))
        (ld-mem-operands '(((:name . "A") (:immediate . t))
                          ((:name . "HL") (:immediate . nil)))))
    (assert (string= (format-instruction ld-mem-opcode ld-mem-operands)
                    "LD A, [HL]")
            nil
            "Expected 'LD A, [HL]', got ~A"
            (format-instruction ld-mem-opcode ld-mem-operands)))
  
  (format t "format-instruction tests passed!~%"))

(defun test-disassemble-instruction ()
  (format t "~%Testing disassemble-instruction...~%")
  
  (let ((test-mmu (make-mmu)))
    ;; Test NOP
    (write-memory test-mmu 0 #x00)
    (multiple-value-bind (instruction bytes)
        (disassemble-instruction test-mmu 0)
      (assert (string= instruction "NOP ")
              nil
              "Expected 'NOP ', got ~A" 
              instruction)
      (assert (= bytes 1)
              nil
              "Expected 1 byte for NOP, got ~A"
              bytes))
    
    ;; Test LD BC,nn
    (write-memory test-mmu 1 #x01)
    (write-memory test-mmu 2 #x34)
    (write-memory test-mmu 3 #x12)
    (multiple-value-bind (instruction bytes)
        (disassemble-instruction test-mmu 1)
      (assert (string= instruction "LD BC, n16")
              nil
              "Expected 'LD BC, n16', got ~A"
              instruction)
      (assert (= bytes 3)
              nil
              "Expected 3 bytes for LD BC,nn, got ~A"
              bytes))
    
    (format t "disassemble-instruction tests passed!~%")))

(defun test-instruction-lengths ()
  (let ((test-mmu (make-mmu)))
    ;; Write a few known instructions
    (write-memory test-mmu #x100 #x00)  ; NOP
    (write-memory test-mmu #x101 #x01)  ; LD BC,nn
    (write-memory test-mmu #x102 #x34)  ; (nn = #x1234)
    (write-memory test-mmu #x103 #x12)
    
    (format t "Testing instruction lengths:~%")
    (let ((pc #x100))
      (dotimes (i 3)
        (multiple-value-bind (instruction bytes)
            (disassemble-instruction test-mmu pc)
          (format t "At #x~4,'0X: ~A (length: ~A)~%" 
                  pc instruction bytes)
          (setf pc (+ pc bytes)))))))

(defun test-cpu-execution ()
  (let ((cpu (make-cpu))
        (mmu (make-mmu)))
    ;; Test program: Load values and increment
    (write-memory mmu #x100 #x3E)  ; LD A,n
    (write-memory mmu #x101 #x42)  ; n = #x42
    (write-memory mmu #x102 #x3C)  ; INC A
    
    (let ((pc #x100))
      (dotimes (i 2)
        (setf pc (execute cpu mmu pc)))
      
      (format t "~%Register A: #x~2,'0X~%" (cpu-a cpu)))))

(defun debug-cpu-slots (cpu)
  "Print all slots and their values in the CPU"
  (format t "CPU Slots:~%")
  (loop for slot in (sb-mop:class-slots (class-of cpu))
        for slot-name = (sb-mop:slot-definition-name slot)
        do (format t "~A: ~A~%" slot-name 
                   (if (slot-boundp cpu slot-name)
                       (slot-value cpu slot-name)
                       "UNBOUND"))))

(defun debug-register-access (cpu reg-name)
  "Debug register access attempt"
  (let ((attempted-slot (intern (format nil "CPU-~A" reg-name) :lispboy)))
    (format t "Attempting to access:~%")
    (format t "  Register name: ~A~%" reg-name)
    (format t "  Slot symbol: ~A~%" attempted-slot)
    (format t "  Symbol package: ~A~%" (package-name (symbol-package attempted-slot)))
    (format t "  Available slots: ~{~A~^, ~}~%" 
            (mapcar #'sb-mop:slot-definition-name 
                    (sb-mop:class-slots (class-of cpu))))))

(defun test-register-operations ()
  "Test basic register operations"
  (let ((cpu (make-cpu)))
    (format t "Initial CPU state:~%")
    (debug-cpu-slots cpu)
    
    (format t "~%Trying to set register A to 42...~%")
    (handler-case
        (progn
          (debug-register-access cpu "A")
          (setf (cpu-register cpu "A") 42)
          (format t "Success! New value: ~A~%" (cpu-register cpu "A")))
      (error (c)
        (format t "Error: ~A~%" c)))))

(defun run-all-tests ()
   (format t "Running all CPU tests...~%")
      (test-register-operations)
  (test-get-opcode)
  (test-format-operand)
  (test-format-instruction)
  (test-disassemble-instruction)
  (test-instruction-lengths)
  (test-cpu-execution)
  (format t "~%All tests completed!~%"))

(export '(run-all-tests))