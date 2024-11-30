(in-package :lispboy)

(defstruct mmu
  (data (make-array #x10000 :element-type '(unsigned-byte 8))))

(defun read-memory (mmu address)
  ; ... perform address translation or any MMU logic ...
  (aref (mmu-data mmu) address))

(defun write-memory (mmu address value)
  ; ... perform address translation or any MMU logic ...
  (setf (aref (mmu-data mmu) address) value))

(defun dump-rom-section (mmu start end)
  "Dump a section of ROM bytes for verification"
  (format t "~%ROM contents from ~4,'0X to ~4,'0X:~%" start end)
  (loop for addr from start below end by 16 do
    (format t "~4,'0X: " addr)
    (loop for offset from 0 below 16
          for byte = (read-memory mmu (+ addr offset))
          do (format t "~2,'0X " byte))
    (format t "~%")))

(defun load-rom (mmu stream)
    (let ((rom-size (file-length stream)))
        (when (> rom-size #x10000)
            (error "ROM too large or banked ROM not supported"))
        (read-sequence (mmu-data mmu) stream :end rom-size))
        (dump-rom-section mmu #x100 #x150))