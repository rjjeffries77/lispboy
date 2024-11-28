(in-package :lispboy)

(defmacro defcpu (name (&rest registers))
  `(defstruct ,name
     ,@(loop for reg in registers 
           collect `(,reg 0 :type (unsigned-byte 8)))
           (PC 0 :type (unsigned-byte 16))
           (SP 0 :type (unsigned-byte 16))))

(defcpu cpu (A B C D E H L))

;(defvar gameboy-cpu (make-cpu))

;; reads the byte at the PC, adjusts the PC after. 
(defun read-byte-at-pc (cpu mmu)
  (let ((byte (read-byte-mm mmu (cpu-PC cpu))))
    (incf (cpu-pc cpu))
    byte))

;; reads the word at the PC, adjusting the PC.
(defun read-word-at-pc (cpu mmu)
  (let ((low-byte (read-byte-at-pc cpu mmu))
        (high-byte (read-byte-at-pc cpu mmu)))
    (logior (ash high-byte 8) low-byte)))

;; get an address from a register pair.
(defun get-address-from-register-pair (cpu reg-high reg-low)
  (let ((high-byte (funcall (intern (format nil "CPU-~A" reg-high)) cpu))
        (low-byte (funcall (intern (format nil "CPU-~A" reg-low)) cpu)))
    (logior (ash high-byte 8) low-byte)))

;; Get the mnemonic associative array for the instruction.
(defun get-opcode (cpu mmu))
  (let ((byte-at-pc (read-byte-at-pc cpu mmc))))
    (if (= byte-at-pc #xCB)
      (cdr (assoc (read-byte-at-pc) prefixed-opcodes)
      (cdr (assoc byte-at-pc unprefixed-opcodes))))

(defun expand-operand (operand)
  (if (> 0 (assoc :bytes operand))))

(defun get-operands (opcode)
  (loop for operand in (get-operands opcode)
    collect (expand-operand operand)))

(defun execute (cpu start)
  (setf (cpu-PC cpu) start)
  ; now run
  )