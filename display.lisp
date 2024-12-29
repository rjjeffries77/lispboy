
(in-package :lispboy)

(defconstant +screen-width+ 160)
(defconstant +screen-height+ 144)
(defconstant +scale+ 3)
(defconstant +framebuffer-size+ (* +screen-width+ +screen-height+))
 

(defconstant +color-darkest+ #x0F380F)
(defconstant +color-dark+ #x306230)
(defconstant +color-light+ #x8BAC0F)
(defconstant +color-lightest+ #x9BBC0F)

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

;; Helper function to set pixel in foreign framebuffer
(defun set-pixel (ppu x y color)
  (when (and (>= x 0) (< x +screen-width+)
             (>= y 0) (< y +screen-height+))
    (setf (cffi:mem-aref (ppu-framebuffer ppu) :uint32 
                         (+ (* y +screen-width+) x))
          color)))

(defun render-scanline (ppu mmu ly)
  "Draw a complete scanline"
  (when (not (zerop (logand (ppu-lcdc ppu) +lcdc-display-enable+)))
    ;; Draw background
    (when (not (zerop (logand (ppu-lcdc ppu) +lcdc-bg-enable+)))
      (draw-background-scanline ppu mmu ly))
    
    ;; Draw window
    (when (and (not (zerop (logand (ppu-lcdc ppu) +lcdc-window-enable+)))
               (>= ly (ppu-wy ppu)))
      (draw-window-scanline ppu mmu ly))
    
    ;; Draw sprites
    (when (not (zerop (logand (ppu-lcdc ppu) +lcdc-obj-enable+)))
      (draw-sprites-scanline ppu mmu ly))))

(defun draw-background-scanline (ppu mmu ly)
  "Draw a single background scanline"
  (let* ((lcdc (ppu-lcdc ppu))
         (scx (ppu-scx ppu))
         (scy (ppu-scy ppu))
         ;; Select tile data area based on LCDC bit 4
         (tile-data-base (if (zerop (logand lcdc +lcdc-bg-tile-data+))
                            #x8800  ; $8800-97FF, signed addressing
                            #x8000)) ; $8000-8FFF
         ;; Select tile map area based on LCDC bit 3
         (tile-map-base (if (zerop (logand lcdc +lcdc-bg-tile-map+))
                           #x9800   ; $9800-9BFF
                           #x9C00)) ; $9C00-9FFF
         ;; Adjust y position for scrolling
         (y (mod (+ ly scy) 256))
         ;; Which row of tiles to use in the map
         (tile-row (floor y 8))
         ;; Which line within the tiles to use
         (tile-line (mod y 8))
         ;; Pre-calculate y offset into tile data
         (tile-line-offset (* tile-line 2)))
    
    ;; Draw all pixels in the scanline
    (loop for screen-x from 0 below +screen-width+
          do (let* ((x (mod (+ screen-x scx) 256))
                    (tile-col (floor x 8))
                    (pixel-x (mod x 8))
                    ;; Get the tile number from the map
                    (map-addr (+ tile-map-base 
                                (+ (* tile-row 32) 
                                   tile-col)))
                    (tile-num (read-memory mmu map-addr))
                    ;; Calculate tile data address
                    (tile-addr (if (zerop (logand lcdc +lcdc-bg-tile-data+))
                                 ;; Signed addressing mode
                                 (+ tile-data-base 
                                    (* (if (> tile-num 127)
                                         (- tile-num 256)
                                         tile-num)
                                       16))
                                 ;; Unsigned addressing mode
                                 (+ tile-data-base (* tile-num 16))))
                    ;; Get the two bytes that define the line of pixels
                    (byte1 (read-memory mmu (+ tile-addr tile-line-offset)))
                    (byte2 (read-memory mmu (+ tile-addr tile-line-offset 1)))
                    ;; Extract the color value for this pixel
                    (color-bit (- 7 pixel-x))
                    (color-num (logior
                               (ash (logand (ash byte2 (- color-bit)) 1) 1)
                               (logand (ash byte1 (- color-bit)) 1)))
                    ;; Convert the 2-bit color to actual RGB using BGP register
                    (palette-color (logand (ash (ppu-bgp ppu) 
                                              (* -2 color-num))
                                         #x3))
                    ;; Map to actual color
                    (color (case palette-color
                            (0 +color-lightest+)
                            (1 +color-light+)
                            (2 +color-dark+)
                            (3 +color-darkest+))))
               ;; Set the pixel using the new set-pixel function
               (set-pixel ppu screen-x ly color)))))

;; Placeholder functions for window and sprite drawing
(defun draw-window-scanline (ppu mmu ly)
  "Draw window tiles for current scanline"
  ;; Window drawing implementation here
  nil)

(defun draw-sprites-scanline (ppu mmu ly)
  "Draw sprites for current scanline"
  ;; Sprite drawing implementation here
  nil)

(defun init-nintendo-logo (ppu mmu)
  "Initialize PPU with Nintendo logo data"
  ;; The logo data should already be in ROM from address $0104-$0133
  ;; We need to copy it to VRAM tile data starting at tile 1 ($8010)
  (loop for i from 0 below #x30
        do (write-memory mmu (+ #x8010 i)
                        (read-memory mmu (+ #x0104 i))))
  
  ;; Set up background map to display logo at $9904
  (loop for i from 0 below 19
        do (write-memory mmu (+ #x9904 i) (1+ i)))
  
  ;; Initialize PPU registers
  (setf (ppu-lcdc ppu) (logior +lcdc-display-enable+
                               +lcdc-bg-enable+))
  (setf (ppu-bgp ppu) #xFC)  ; 11111100 - Normal palette
  (setf (ppu-scx ppu) 44)    ; Center horizontally
  (setf (ppu-scy ppu) 48))

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

(defun update-ppu-state (ppu mmu current-dots)
  "Update PPU state based on current dot count"
  (let* ((scanline (ppu-ly ppu))
         (line-dots (mod current-dots +scanline-cycles+))
         (old-mode (ppu-mode ppu)))
    
    ;; Update mode based on dot position in scanline
    (setf (ppu-mode ppu)
          (cond 
            ((>= scanline +vblank-start+) 1)  ; VBlank
            ((<= line-dots +mode-2-cycles+) 2) ; OAM Search
            ((<= line-dots (+ +mode-2-cycles+ +mode-3-cycles+)) 3) ; Pixel Transfer
            (t 0))) ; H-Blank

    ;; Handle mode transitions
    (when (/= old-mode (ppu-mode ppu))
      (case (ppu-mode ppu)
        (0  ; Entering H-Blank
         (when (lcdc-display-enabled-p ppu)
           (render-scanline ppu mmu scanline)))
        (1  ; Entering V-Blank
         (when (= scanline +vblank-start+)
           (request-vblank-interrupt mmu)))
        (2  ; Starting new scanline
         (when (= line-dots 0)
           (incf (ppu-ly ppu))
           (write-memory mmu #xFF44 (ppu-ly ppu))
           (format t "PPU LY updated to ~A~%" (ppu-ly ppu))
           ;; Reset window line counter at start of frame
           (when (zerop scanline)
             (setf (ppu-window-line ppu) 0))))))
    (update-stat-register ppu mmu)))
