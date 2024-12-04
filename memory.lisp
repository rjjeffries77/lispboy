(in-package :lispboy)

(defstruct mmu
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))  ; 32KB ROM space
  (vram (make-array #x2000 :element-type '(unsigned-byte 8))) ; 8KB VRAM
  (wram (make-array #x2000 :element-type '(unsigned-byte 8))) ; 8KB Work RAM
  (oam (make-array #x100 :element-type '(unsigned-byte 8)))   ; Object Attribute Memory
  (io (make-array #x80 :element-type '(unsigned-byte 8)))     ; I/O Registers
  (hram (make-array #x7F :element-type '(unsigned-byte 8)))   ; High RAM
  (ie 0 :type (unsigned-byte 8)))                             ; Interrupt Enable Register

(defun read-memory (mmu address)
  "Read a byte from memory at the given address"
  (cond
    ;; ROM Banks 0-1 ($0000-$7FFF)
    ((< address #x8000)
     (aref (mmu-rom mmu) address))
    
    ;; VRAM ($8000-$9FFF)
    ((< address #xA000)
     (aref (mmu-vram mmu) (- address #x8000)))
    
    ;; Work RAM ($C000-$DFFF)
    ((< address #xE000)
     (aref (mmu-wram mmu) (- address #xC000)))
    
    ;; Echo RAM ($E000-$FDFF) - mirrors $C000-$DDFF
    ((< address #xFE00)
     (aref (mmu-wram mmu) (- address #xE000)))
    
    ;; OAM ($FE00-$FE9F)
    ((< address #xFEA0)
     (aref (mmu-oam mmu) (- address #xFE00)))
    
    ;; I/O Registers ($FF00-$FF7F)
    ((< address #xFF80)
     (aref (mmu-io mmu) (- address #xFF00)))
    
    ;; High RAM ($FF80-$FFFE)
    ((< address #xFFFF)
     (aref (mmu-hram mmu) (- address #xFF80)))
    
    ;; Interrupt Enable Register ($FFFF)
    ((= address #xFFFF)
     (mmu-ie mmu))
    
    ;; Unused memory ranges return $FF
    (t #xFF)))

(defun write-memory (mmu address value)
  "Write a byte to memory at the given address"
  (cond
    ;; ROM Banks 0-1 ($0000-$7FFF) - Read Only
    ((< address #x8000)
     nil)  ; Ignore writes to ROM
    
    ;; VRAM ($8000-$9FFF)
    ((< address #xA000)
     (setf (aref (mmu-vram mmu) (- address #x8000)) value))
    
    ;; Work RAM ($C000-$DFFF)
    ((< address #xE000)
     (setf (aref (mmu-wram mmu) (- address #xC000)) value))
    
    ;; Echo RAM ($E000-$FDFF) - mirrors $C000-$DDFF
    ((< address #xFE00)
     (setf (aref (mmu-wram mmu) (- address #xE000)) value))
    
    ;; OAM ($FE00-$FE9F)
    ((< address #xFEA0)
     (setf (aref (mmu-oam mmu) (- address #xFE00)) value))
    
    ;; I/O Registers ($FF00-$FF7F)
    ((< address #xFF80)
     (setf (aref (mmu-io mmu) (- address #xFF00)) value))
    
    ;; High RAM ($FF80-$FFFE)
    ((< address #xFFFF)
     (setf (aref (mmu-hram mmu) (- address #xFF80)) value))
    
    ;; Interrupt Enable Register ($FFFF)
    ((= address #xFFFF)
     (setf (mmu-ie mmu) value))))

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
  "Load a full memory dump into appropriate MMU arrays"
  (let ((dump-size (file-length stream)))
    (when (/= dump-size #x10000)
      (error "Expected 64KB memory dump, got ~D bytes" dump-size))
    
    ;; Read the full dump into a temporary array
    (let ((full-dump (make-array #x10000 :element-type '(unsigned-byte 8))))
      (read-sequence full-dump stream)
      
      ;; Copy each section to appropriate MMU array
      ;; ROM ($0000-$7FFF)
      (replace (mmu-rom mmu) full-dump :end1 #x8000)
      
      ;; VRAM ($8000-$9FFF)
      (replace (mmu-vram mmu) 
              full-dump 
              :start2 #x8000 
              :end2 #xA000)
      
      ;; Work RAM ($C000-$DFFF)
      (replace (mmu-wram mmu) 
              full-dump 
              :start2 #xC000 
              :end2 #xE000)
      
      ;; OAM ($FE00-$FE9F)
      (replace (mmu-oam mmu) 
              full-dump 
              :start2 #xFE00 
              :end2 #xFEA0)
      
      ;; I/O Registers ($FF00-$FF7F)
      (replace (mmu-io mmu) 
              full-dump 
              :start2 #xFF00 
              :end2 #xFF80)
      
      ;; High RAM ($FF80-$FFFE)
      (replace (mmu-hram mmu) 
              full-dump 
              :start2 #xFF80 
              :end2 #xFFFF)
      
      ;; Interrupt Enable Register ($FFFF)
      (setf (mmu-ie mmu) (aref full-dump #xFFFF)))))