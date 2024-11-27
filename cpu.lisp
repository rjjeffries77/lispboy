(in-package :lispboy)

(defmacro defcpu (name (&rest registers))
  `(defstruct ,name
     ,@(loop for reg in registers 
           collect `(,reg 0 :type (unsigned-byte 8)))
           (PC 0 :type (unsigned-byte 16))
           (SP 0 :type (unsigned-byte 16))))

(defcpu cpu (A B C D E H L))

(defvar gameboy-cpu (make-cpu))

;; reads the byte at the PC, adjusts the PC after 
(defun read-byte-at-pc (cpu mmu)
  (let ((byte (read-byte-mm mmu (cpu-PC cpu))))
    (incf (cpu-pc cpu))
    byte))

;; reads the word at the PC, adjusting the PC
(defun read-word-at-pc (cpu mmu)
  (let ((low-byte (read-byte-at-pc cpu mmu))
        (high-byte (read-byte-at-pc cpu mmu)))
    (logior (ash high-byte 8) low-byte)))

;; get an address from a register pair
(defun get-address-from-register-pair (cpu reg-high reg-low)
  (let ((high-byte (funcall (intern (format nil "CPU-~A" reg-high)) cpu))
        (low-byte (funcall (intern (format nil "CPU-~A" reg-low)) cpu)))
    (logior (ash high-byte 8) low-byte)))


