
(in-package :lispboy)

(defconstant +screen-width+ 160)
(defconstant +screen-height+ 144)
(defconstant +scale+ 3)
(defconstant +framebuffer-size+ (* +screen-width+ +screen-height+))
 

(defconstant +color-darkest+ #x0F380F)
(defconstant +color-dark+ #x306230)
(defconstant +color-light+ #x8BAC0F)
(defconstant +color-lightest+ #x9BBC0F)

(defconstant +color-map+ (vector +color-lightest+  ; Lightest (off) - GameBoy green
                           +color-light+  ; Light
                           +color-dark+   ; Dark
                           +color-darkest+)) ; Darkest (on)

;; Sprite flags
(defconstant +sprite-priority+ #x80)  ; 0=Above BG, 1=Behind BG
(defconstant +sprite-y-flip+ #x40)    ; Vertical flip
(defconstant +sprite-x-flip+ #x20)    ; Horizontal flip
(defconstant +sprite-palette+ #x10)   ; 0=OBP0, 1=OBP1

(defstruct sprite
  x y tile-num attributes)
(defconstant +lcdc-display-enable+ #x80)
(defconstant +lcdc-window-tile-map+ #x40)
(defconstant +lcdc-window-enable+ #x20)
(defconstant +lcdc-bg-tile-data+ #x10)
(defconstant +lcdc-bg-tile-map+ #x08)
(defconstant +lcdc-obj-size+ #x04)
(defconstant +lcdc-obj-enable+ #x02)
(defconstant +lcdc-bg-enable+ #x01)

;; PPU timing constants
(defconstant +vblank-start+ 144)    ; First VBlank line
(defconstant +vblank-end+ 153)  
(defconstant +mode-2-cycles+ 80)    ; OAM search - 80 cycles
(defconstant +mode-3-cycles+ 172)   ; Pixel transfer - 172-289 cycles
(defconstant +mode-0-cycles+ 208)   ; H-Blank - 87-204 cycles
(defconstant +scanline-cycles+ 456) ; Total cycles per scanline
(defconstant +vblank-scanlines+ 10) ; Number of V-Blank scanlines

;; STAT register bit masks
(defconstant +stat-mode-flag+ #x03)      ; Current PPU mode
(defconstant +stat-coincidence+ #x04)    ; LYC=LY coincidence flag
(defconstant +stat-mode-0-int+ #x08)     ; Mode 0 H-Blank interrupt
(defconstant +stat-mode-1-int+ #x10)     ; Mode 1 V-Blank interrupt
(defconstant +stat-mode-2-int+ #x20)     ; Mode 2 OAM interrupt
(defconstant +stat-coincidence-int+ #x40) ; LY=LYC interrupt
(cffi:defcstruct framebuffer-array
  (pixels :uint32 :count 23040))


(defstruct fetcher
  "Pixel fetcher state"
  (state :get-tile)        ; Current state of fetch
  tile-number              ; Current tile number
  tile-data-low           ; Low byte of tile data
  tile-data-high          ; High byte of tile data
  (fifo (make-fifo))      ; Pixel FIFO
  (x 0)                     ; Current X position
  (pushed 0))             ; Number of pixels pushed to FIFO

(defstruct fifo
  (data (make-array 8 :initial-element 0))
  (head 0)
  (size 0))

(defstruct ppu
  (lcdc 0 :type (unsigned-byte 8))
  (stat 0 :type (unsigned-byte 8))
  (ly 0 :type (unsigned-byte 8))
  (lyc 0 :type (unsigned-byte 8))
  (bgp 0 :type (unsigned-byte 8))
  (obp0 0 :type (unsigned-byte 8))
  (obp1 0 :type (unsigned-byte 8))
  (scx 0 :type (unsigned-byte 8))
  (scy 0 :type (unsigned-byte 8))
  (wx 0 :type (unsigned-byte 8))
  (wy 0 :type (unsigned-byte 8))
  (mode 2 :type (integer 0 3))
  (window-line 0 :type integer)
  (bg-fifo (make-fifo))
  (sprite-fifo (make-fifo))
  (fetcher (make-fetcher))
  (framebuffer (cffi:null-pointer) :type cffi:foreign-pointer))

(defun make-ppu-with-framebuffer ()
  (let ((ppu (make-ppu)))
    (setf (ppu-framebuffer ppu)
          (cffi:foreign-alloc '(:struct framebuffer-array)))
    (loop for i from 0 below (* +screen-width+ +screen-height+)
          do (setf (cffi:mem-aref (ppu-framebuffer ppu) :uint32 i)
                   +color-lightest+))
    ppu))

; Data for Nintendo logo
(defconstant +nintendo-logo-width+ 12)
(defconstant +nintendo-logo-height+ 8)
(defconstant +boot-scroll-steps+ 8)
;; Nintendo Logo tile data (stored in ROM at $0104-$0133)

(defconstant +nintendo-logo-data+
  #(#xCE #xED #x66 #x66 #xCC #x0D #x00 #x0B #x03 #x73 #x00 #x83 #x00 #x0C #x00 #x0D
    #x00 #x08 #x11 #x1F #x88 #x89 #x00 #x0E #xDC #xCC #x6E #xE6 #xDD #xDD #xD9 #x99
    #xBB #xBB #x67 #x63 #x6E #x0E #xEC #xCC #xDD #xDC #x99 #x9F #xBB #xB9 #x33 #x3E))

(defun init-nintendo-logo (mmu)
  "Initialize VRAM with Nintendo logo data"
  ;; Set up the tiles - each tile is 16 bytes (8x8 pixels, 2bpp)
  ;; The logo data starts at $0104 in ROM and we'll copy it to the first tile pattern in VRAM
  (loop for i from 0 below (length +nintendo-logo-data+)
        do (write-memory mmu (+ #x8010 i) (aref +nintendo-logo-data+ i)))
  
  ;; Set up the tile map - the logo uses tiles 1-19
  ;; We'll place it at $9904 (near top of background map)
  (loop for i from 0 below 19
        do (write-memory mmu (+ #x9904 i) (1+ i)))

  ;; Now set up PPU registers for correct display
  ;; LCDC - Enable LCD and BG
  (write-memory mmu #xFF40 #x91)  ; LCD on, BG on, Rest off
  ;; BGP - Normal palette
  (write-memory mmu #xFF47 #xFC)  ; 11111100 - Normal colors
  ;; Scroll registers to center the logo
  (write-memory mmu #xFF42 48)    ; SCY - Vertical scroll
  (write-memory mmu #xFF43 44))   ; SCX - Horizontal scroll

(defun fifo-push (fifo value)
  (when (< (fifo-size fifo) 8)
    (let ((pos (mod (+ (fifo-head fifo) (fifo-size fifo)) 8)))
      (setf (aref (fifo-data fifo) pos) value)
      (incf (fifo-size fifo)))))

(defun fifo-pop (fifo)
  (when (> (fifo-size fifo) 0)
    (let ((value (aref (fifo-data fifo) (fifo-head fifo))))
      (setf (fifo-head fifo) (mod (1+ (fifo-head fifo)) 8))
      (decf (fifo-size fifo))
      value)))

(defun fifo-clear (fifo)
  (setf (fifo-head fifo) 0
        (fifo-size fifo) 0))

(defun get-color (palette color-num)
  "Convert 2-bit color through palette to final color"
  (aref +color-map+ 
        (logand (ash palette (- (* color-num 2))) #x3)))

;; Helper function to set pixel in foreign framebuffer
(defun set-pixel (ppu x y color)
  (when (and (>= x 0) (< x +screen-width+)
             (>= y 0) (< y +screen-height+))
    (setf (cffi:mem-aref (ppu-framebuffer ppu) :uint32 
                         (+ (* y +screen-width+) x))
          color)))

(defun request-vblank-interrupt (mmu)
  "Request VBlank interrupt through MMU"
  (request-interrupt mmu #x01)) ; VBlank is bit 0

(defun request-lcd-stat-interrupt (mmu)
  "Request LCD STAT interrupt through MMU"
  (request-interrupt mmu #x02)) 

(defun update-stat-register (ppu mmu)
  "Update LCD STAT register (FF41) and trigger STAT interrupts if enabled"
  (let ((stat (ppu-stat ppu))
        (current-mode (ppu-mode ppu)))
    
    ;; Clear previous mode bits and set new mode
    (setf stat (logand stat #xFC))        ; Clear bottom 2 bits
    (setf stat (logior stat current-mode)) ; Set new mode
    
    ;; Set coincidence flag (Bit 2) if LY = LYC
    (if (= (ppu-ly ppu) (ppu-lyc ppu))
        (setf stat (logior stat #x04))    ; Set bit 2
        (setf stat (logand stat #xFB)))   ; Clear bit 2
    
    ;; Check if we should trigger STAT interrupt
    (let ((interrupt-requested nil))
      ;; Check each interrupt source if enabled
      (cond
        ;; Mode 0 (H-Blank) interrupt
        ((and (= current-mode 0)
              (logbitp 3 stat))
         (setf interrupt-requested t))
        
        ;; Mode 1 (V-Blank) interrupt
        ((and (= current-mode 1)
              (logbitp 4 stat))
         (setf interrupt-requested t))
        
        ;; Mode 2 (OAM) interrupt
        ((and (= current-mode 2)
              (logbitp 5 stat))
         (setf interrupt-requested t))
        
        ;; LY=LYC coincidence interrupt
        ((and (= (ppu-ly ppu) (ppu-lyc ppu))
              (logbitp 6 stat))
         (setf interrupt-requested t)))
      
      ;; If any interrupt was requested, set LCD STAT bit in IF
      (when interrupt-requested
        (let ((if-reg (mmu-if mmu)))
          (setf (mmu-if mmu) (logior if-reg #x02)))))
    
    ;; Update STAT register in memory
    (write-memory mmu #xFF41 stat)
    (setf (ppu-stat ppu) stat)))

(defun lcdc-flag-set-p (ppu flag)
  "Check if a specific LCDC flag is set"
  (not (zerop (logand (ppu-lcdc ppu) flag))))

(defun lcdc-display-enabled-p (ppu)
  "Check if LCD display is enabled (bit 7 of LCDC)"
  (lcdc-flag-set-p ppu +lcdc-display-enable+))

(defun update-ppu-state (ppu mmu scanline dot)
  "Update PPU state based on current scanline and dot position"
  (let ((old-mode (ppu-mode ppu)))
    
    ;; Update mode based on dot position in scanline
    (setf (ppu-mode ppu)
          (cond 
            ((>= scanline +vblank-start+) 1)  ; VBlank
            ((<= dot +mode-2-cycles+) 2)      ; OAM Search
            ((<= dot (+ +mode-2-cycles+ +mode-3-cycles+)) 3) ; Pixel Transfer
            (t 0)))                           ; H-Blank

    ;; Handle mode transitions
    (when (/= old-mode (ppu-mode ppu))
      (case (ppu-mode ppu)
        (0  ; Entering H-Blank
         (fifo-clear (ppu-bg-fifo ppu))
         (fifo-clear (ppu-sprite-fifo ppu)))
        (1  ; Entering V-Blank
         (when (= scanline +vblank-start+)
           (format t "Requesting vblank interrupt")
           (request-vblank-interrupt mmu)))
        (2  ; Starting new scanline
         (when (zerop dot)
           (write-memory mmu #xFF44 scanline)
           ;; Reset window line counter at start of frame
           (when (zerop scanline)
             (setf (ppu-window-line ppu) 0))))))

    ;; Mode 3
    (when (= (ppu-mode ppu) 3)
      (let* ((x (- dot +mode-2-cycles+)) ; Current x position relative to start of Mode 3
             (fetcher (ppu-fetcher ppu)))
        
        ;; Update fetcher state
        (case (fetcher-state fetcher)
          (:get-tile
           ;; Get tile number from tile map
           (let* ((lcdc (ppu-lcdc ppu))
                  (map-base (if (zerop (logand lcdc #x08))
                                #x9800 #x9C00))
                  (tile-y (floor (/ (+ scanline (ppu-scy ppu)) 8)))
                  (tile-x (floor (/ (+ (fetcher-x fetcher) (ppu-scx ppu)) 8)))
                  (map-addr (+ map-base (* 32 (mod tile-y 32)) (mod tile-x 32))))
             (setf (fetcher-tile-number fetcher) 
                   (read-memory mmu map-addr))
             (setf (fetcher-state fetcher) :get-tile-data-low)))
          
          (:get-tile-data-low
           ;; Get low byte of tile data
           (let* ((lcdc (ppu-lcdc ppu))
                  (tile-base (if (zerop (logand lcdc #x10))
                                 #x8800 #x8000))
                  (tile-num (fetcher-tile-number fetcher))
                  (fine-y (mod (+ scanline (ppu-scy ppu)) 8))
                  (tile-addr (if (= tile-base #x8800)
                                 (+ tile-base (* (if (> tile-num 127)
                                                     (- tile-num 256)
                                                     tile-num)
                                                 16))
                                 (+ tile-base (* tile-num 16))))
                  (data-addr (+ tile-addr (* fine-y 2))))
             (setf (fetcher-tile-data-low fetcher)
                   (read-memory mmu data-addr))
             (setf (fetcher-state fetcher) :get-tile-data-high)))
          
          (:get-tile-data-high
           ;; Get high byte of tile data
           (let* ((lcdc (ppu-lcdc ppu))
                  (tile-base (if (zerop (logand lcdc #x10))
                                 #x8800 #x8000))
                  (tile-num (fetcher-tile-number fetcher))
                  (fine-y (mod (+ scanline (ppu-scy ppu)) 8))
                  (tile-addr (if (= tile-base #x8800)
                                 (+ tile-base (* (if (> tile-num 127)
                                                     (- tile-num 256)
                                                     tile-num)
                                                 16))
                                 (+ tile-base (* tile-num 16))))
                  (data-addr (+ tile-addr (* fine-y 2) 1)))
             (setf (fetcher-tile-data-high fetcher)
                   (read-memory mmu data-addr))
             (setf (fetcher-state fetcher) :push-to-fifo)))
          
          (:push-to-fifo
           ;; Push pixels to FIFO if it has space
           (when (< (fifo-size (ppu-bg-fifo ppu)) 8)
             (let ((low (fetcher-tile-data-low fetcher))
                   (high (fetcher-tile-data-high fetcher)))
               ;; Push 8 pixels to FIFO
               (dotimes (i 8)
                 (let* ((bit (- 7 i))
                        (color-low (logand (ash low (- bit)) 1))
                        (color-high (ash (logand (ash high (- bit)) 1) 1))
                        (color-num (logior color-high color-low)))
                   (fifo-push (ppu-bg-fifo ppu)
                              (get-color (ppu-bgp ppu) color-num))))
               ;; Move to next tile
               (incf (fetcher-x fetcher) 8)
               (setf (fetcher-state fetcher) :get-tile)))))
        
        ;; Output pixel if we have one in the FIFO
        (when (and (>= x 0) (< x +screen-width+))
          (let ((color (fifo-pop (ppu-bg-fifo ppu))))
            (when color
              (set-pixel ppu x scanline color))))))
    
    ;; Update STAT register
    (update-stat-register ppu mmu)))
