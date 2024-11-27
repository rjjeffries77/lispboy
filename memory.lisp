(in-package :lispboy)

(defstruct mmu
  (memory (make-array #x10000 :element-type '(unsigned-byte 8) :initial-element 0)))

(defun read-byte-mm (mmu addr)
  (aref (mmu-memory mmu) addr))

(defun write-byte-mm (mmu addr value)
  (setf (aref (mmu-memory mmu) addr) value))