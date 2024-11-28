(in-package :lispboy)

(defstruct mmu
  (data (make-array #10000 :element-type '(unsigned-byte 8))))

(defun read-memory (mmu address)
  ; ... perform address translation or any MMU logic ...
  (aref (mmu-data mmu) address))

(defun write-memory (mmu address value)
  ; ... perform address translation or any MMU logic ...
  (setf (aref (mmu-data mmu) address) value))

(defun load-rom (mmu stream))
    (let ((rom-size (file-length stream))
        (when (> rom-size #x8000)
            (error "ROM too large or banked ROM not supported")))
        (read-sequence (mmu-data mmu) stream :end rom-size)))