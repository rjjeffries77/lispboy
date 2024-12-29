(in-package :lispboy)

(defstruct mmu
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))  ; 32KB ROM space
  (vram (make-array #x2000 :element-type '(unsigned-byte 8))) ; 8KB VRAM
  (wram (make-array #x2000 :element-type '(unsigned-byte 8))) ; 8KB Work RAM
  (oam (make-array #x100 :element-type '(unsigned-byte 8)))   ; Object Attribute Memory
  (io (make-array #x80 :element-type '(unsigned-byte 8)))     ; I/O Registers
  (hram (make-array #x7F :element-type '(unsigned-byte 8)))   ; High RAM
  (if 0 :type (unsigned-byte 8))
  (ie 0 :type (unsigned-byte 8))
  (interrupt-lock (bt:make-lock "interrupt-flag-lock")))

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

    ;; Interrupt Flag Register ($FF0F) 
    ((= address #xFF0F)
     (bt:with-lock-held ((mmu-interrupt-lock mmu))
       (mmu-if mmu)))

    ;; Interrupt Enable Register ($FFFF)
    ((= address #xFFFF)
     (bt:with-lock-held ((mmu-interrupt-lock mmu))
       (mmu-ie mmu)))
    
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
    
     ((= address #xFF0F)
     (bt:with-lock-held ((mmu-interrupt-lock mmu))
       (setf (mmu-if mmu) value)))
    
    ;; Interrupt Enable Register ($FFFF)
    ((= address #xFFFF)
     (bt:with-lock-held ((mmu-interrupt-lock mmu))
       (setf (mmu-ie mmu) value)))))

(defun request-interrupt (mmu interrupt-bit)
  "Request interrupt by setting the corresponding bit in IF"
  (bt:with-lock-held ((mmu-interrupt-lock mmu))
    (setf (mmu-if mmu) 
          (logior (mmu-if mmu) interrupt-bit))))

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
    "Load a ROM into appropriate MMU arrays"
    (let ((rom-size (file-length stream)))
      ;; Check for valid ROM sizes (32KB or 64KB)
      (unless (or (= rom-size #x8000)   ; 32KB
                  (= rom-size #x10000))  ; 64KB
        (error "Invalid ROM size: ~D bytes. Expected 32KB or 64KB" rom-size))
      
      (cond
        ;; 32KB ROM
        ((= rom-size #x8000)
         ;; Read the ROM data into a temporary buffer
         (let ((rom-data (make-array rom-size :element-type '(unsigned-byte 8))))
           (read-sequence rom-data stream)
           ;; Copy ROM data to MMU ROM space (first 32KB)
           (replace (mmu-rom mmu) rom-data)))
        
        ;; 64KB full memory dump
        ((= rom-size #x10000)
         (let ((full-dump (make-array rom-size :element-type '(unsigned-byte 8))))
           (read-sequence full-dump stream)
           
           ;; Copy each section to appropriate MMU array
           ;; ROM ($0000-$7FFF)
           (replace (mmu-rom mmu) full-dump :end1 #x8000)
           
           ;; VRAM ($8000-$9FFF)
           (replace (mmu-vram mmu) full-dump :start2 #x8000 :end2 #xA000)
           
           ;; Work RAM ($C000-$DFFF)
           (replace (mmu-wram mmu) full-dump :start2 #xC000 :end2 #xE000)
           
           ;; OAM ($FE00-$FE9F)
           (replace (mmu-oam mmu) full-dump :start2 #xFE00 :end2 #xFEA0)
           
           ;; I/O Registers ($FF00-$FF7F)
           (replace (mmu-io mmu) full-dump :start2 #xFF00 :end2 #xFF80)
           
           ;; High RAM ($FF80-$FFFE)
           (replace (mmu-hram mmu) full-dump :start2 #xFF80 :end2 #xFFFF)
           
           ;; Interrupt Enable Register ($FFFF)
           (setf (mmu-ie mmu) (aref full-dump #xFFFF)))))))
