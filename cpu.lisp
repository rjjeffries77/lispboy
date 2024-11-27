(in-package :lispboy)

(defmacro defcpu (name (&rest registers))
  '(defstruct ,name
     ,@(loop for reg in registers 
           collect `(,reg 0 :type (unsigned-byte 8)))
           (PC 0: type (unsigned-byte 16)
           (SP 0: type (unsigned-byte 16)))))

(defcpu cpu (A B C D E H L))

(defun execute-opcode (cpu opcode)
  (match opcode
    (#x00 nil) ; NOP
    (#x3E (let ((value (read-byte-at-pc cpu)))  ; LD A,n
           (setf (cpu-a cpu) value)
           (incf (cpu-pc cpu))))
    (#x47 (setf (cpu-b cpu) (cpu-a cpu)))       ; LD B,A
    ;; Add more opcodes as needed
    (t (error "Unknown opcode: ~X" opcode))))

;; Helper functions
(defun read-byte-at-pc (cpu)
  (incf (cpu-PC cpu))
  (aref *memory* (1- (cpu-pc cpu))))

(defun read-word-at-pc (cpu)
  (let ((low-byte (read-byte-at-pc cpu))
        (high-byte (read-byte-at-pc cpu)))
    (+ low-byte (ash high-byte 8))))

;; Emulation step
(defun step-cpu (cpu)
  (let ((opcode (read-byte-at-pc cpu)))
    (execute-opcode cpu opcode)))