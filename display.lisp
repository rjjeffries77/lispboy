
(in-package :lispboy)
(declaim (optimize (speed 3) (safety 0)))

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
(defconstant +oam-search-cycles+ 80)    ; OAM search - 80 cycles
(defconstant +hblank-cycles+ 43)   ; Pixel transfer - 172-289 cycles
(defconstant +vblank-cycles+ 1140)   ; H-Blank - 87-204 cycles
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

(defstruct pixel
  x
  colour)

(defstruct fetcher
  "Pixel fetcher state"
  (state :get-tile)        ; Current state of fetch
  tile-number              ; Current tile number
  tile-data-low           ; Low byte of tile data
  tile-data-high          ; High byte of tile data
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
  (phase :h-blank)
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
  (loop for i from 0 below (length +nintendo-logo-data+)
        do (write-memory mmu (+ #x8000 i) (aref +nintendo-logo-data+ i)))
  
  ;; Set up the tile map
  (loop for y from 0 below 2
        do (loop for x from 0 below +nintendo-logo-width+
                 do (write-memory mmu 
                                  (+ #x9904 x (* y 32))
                                  (+ (* y +nintendo-logo-width+) x))))
  
  ;; Set PPU registers
  (write-memory mmu #xFF40 #x91)  ; LCD on, BG/window tilemap 0, BG on
  (write-memory mmu #xFF42 0)     ; SCY = 0
  (write-memory mmu #xFF43 0)     ; SCX = 0
  (write-memory mmu #xFF47 #xE4)) ; 

(Defun fifo-push (fifo value)
  (when (< (fifo-size fifo) 8)
    (let ((pos (mod (+ (fifo-head fifo) (fifo-size fifo)) 8)))
      (setf (aref (fifo-data fifo) pos) value)
      (incf (fifo-size fifo)))))

(defun fifo-pop (fifo)
  (when (> (fifo-size fifo) 0)
    (let ((value (aref (fifo-data fifo) (fifo-head fifo))))
      (setf (fifo-head fifo) (mod (1+ (fifo-head fifo)) 8))
      (decf (fifo-size fifo)) value)))

(defun fifo-clear (fifo)
  (setf (fifo-head fifo) 0
        (fifo-size fifo) 0))

(defun get-color (palette color-num)
  "Convert 2-bit color through palette to final color"
  (aref +color-map+ 
        (logand (ash palette (- (* color-num 2))) #x3)))

;; Helper function to set pixel in foreign framebuffer
(defun set-pixel (ppu x y color)
  "Draw a pixel to the framebuffer"
  (when (and (>= x 0) (< x +screen-width+)
             (>= y 0) (< y +screen-height+))
    (setf (cffi:mem-aref (ppu-framebuffer ppu) :uint32 
                         (+ (* y +screen-width+) x))
          color)))

(defun log-fetcher-state (fetcher)
  (format t "Fetcher state: ~A at x=~A pushed=~A~%" 
          (fetcher-state fetcher)
          (fetcher-x fetcher)
          (fetcher-pushed fetcher)))

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


(defun get-tile-number (ppu mmu scanline fetcher)
  (let* ((map-base (if (lcdc-flag-set-p ppu +lcdc-bg-tile-map+)
                       #x9C00 #x9800))
         (tile-y (floor (/ (+ scanline (ppu-scy ppu)) 8)))
         (tile-x (floor (/ (+ (fetcher-x fetcher) (ppu-scx ppu)) 8)))
         (map-addr (+ map-base 
                      (mod (* 32 tile-y) #x400) 
                      (mod tile-x 32))))
    (read-memory mmu map-addr)))

(Defun get-tile-data-address (ppu tile-num)
  (let* ((tile-base (if (lcdc-flag-set-p ppu +lcdc-bg-tile-data+)
                        #x8000 #x8800))
         (signed-tile-num (if (= tile-base #x8800)
                              (if (> tile-num 127)
                                  (- tile-num 256)
                                  tile-num)
                              tile-num)))
    (+ tile-base (* signed-tile-num 16))))

(defun fetch-tile-data (ppu mmu scanline tile-num)
  "Fetch the two bytes for current scanline row of tile"
  (let* ((tile-addr (get-tile-data-address ppu tile-num))
         (row (mod (+ scanline (ppu-scy ppu)) 8))
         (row-addr (+ tile-addr (* row 2))))
    (values (read-memory mmu row-addr)
            (read-memory mmu (1+ row-addr)))))


(defun push-pixels-to-fifo (ppu fetcher)
  "Push next pixel from current tile data to FIFO"
  (when (>= (- 16 (fifo-size (ppu-bg-fifo ppu))) 8) ; Check for enough room *before* pushing a tile
    (loop for i from 0 below 8 do  ; Push all 8 pixels of the tile
          (let* ((bit (- 7 i)) ; Simplified bit calculation
                 (color-low (logand (ash (fetcher-tile-data-low fetcher) (- bit)) 1))
                 (color-high (ash (logand (ash (fetcher-tile-data-high fetcher) (- bit)) 1) 1))
                 (color-num (logior color-high color-low)))
            (fifo-push (ppu-bg-fifo ppu)
                       (make-pixel :x (fetcher-x fetcher)
                                   :colour (get-color (ppu-bgp ppu) color-num))))
          (incf (fetcher-x fetcher)))

    (setf (fetcher-state fetcher) :get-tile))) ; Move to the next tile *after* pushing all 8 pixels

(Defun update-ppu-state (ppu mmu scanline ppu-cycle)
  "Update PPU state based on current scanline and dot position"
  (when (< scanline +vblank-start+)
            (case (ppu-phase ppu)
      (:oam-search
       (when (zerop ppu-cycle)
         (setf (mmu-oam-lock mmu) t)
         (write-memory mmu #xFF44 scanline)
         ;; Reset fetcher state at start of each scanline
         (let ((fetcher (ppu-fetcher ppu)))
           (setf (fetcher-state fetcher) :get-tile
                 (fetcher-x fetcher) 0
                 (fetcher-pushed fetcher) 0))
         (fifo-clear (ppu-bg-fifo ppu))
         (fifo-clear (ppu-sprite-fifo ppu)))
         ;; Reset window line counter at start of frame
       (when (zerop scanline)
         (setf (ppu-window-line ppu) 0))
       (when (>= ppu-cycle +oam-search-cycles+); Use ppu-cycle for timing
         (setf (ppu-phase ppu) :pixel-transfer)))

      (:pixel-transfer
       (let ((fetcher (ppu-fetcher ppu)))
         (case (fetcher-state fetcher)
           (:get-tile
            (setf (fetcher-tile-number fetcher) 
                  (get-tile-number ppu mmu scanline fetcher)) 
            (setf (fetcher-state fetcher) :get-tile-data-low))

           (:get-tile-data-low
            (multiple-value-bind (low high)
                (fetch-tile-data ppu mmu scanline (fetcher-tile-number fetcher))
              (setf (fetcher-tile-data-low fetcher) low
                    (fetcher-tile-data-high fetcher) high
                    (fetcher-state fetcher) :push-and-draw)))

           (:push-and-draw; Combined push and draw
            (loop for i from 0 below 8 do; Process and draw all 8 pixels of the tile row
                  (let* ((bit (- 7 i))
                         (color-low (logand (ash (fetcher-tile-data-low fetcher) (- bit)) 1))
                         (color-high (ash (logand (ash (fetcher-tile-data-high fetcher) (- bit)) 1) 1))
                         (color-num (logior color-high color-low)))
                    (let ((pixel (make-pixel :x (fetcher-x fetcher)
                                             :colour (get-color (ppu-bgp ppu) color-num))))
                      (when (and (>= (pixel-x pixel) 0) (< (pixel-x pixel) +screen-width+))
                        (set-pixel ppu (pixel-x pixel) scanline (pixel-colour pixel)))
                      (incf (fetcher-x fetcher)))))
            (setf (fetcher-state fetcher) :get-tile)
            (when (<= 160 (fetcher-x fetcher)); Check screen width
              (setf (fetcher-x fetcher) 0)
              (setf (ppu-phase ppu) :h-blank))
            (when (= scanline 144)
              (setf (ppu-phase ppu) :v-blank))))))
         (:h-blank
          (when (>= ppu-cycle +hblank-cycles+)
            (setf (mmu-oam-lock mmu) nil)
            (setf (mmu-vram-lock mmu) nil)
            (setf (ppu-phase ppu) :oam-search)))   
         (:v-blank
          (when (>= ppu-cycle +vblank-cycles+) ; Use ppu-cycle for timing
            (request-vblank-interrupt mmu)
            (setf (ppu-phase ppu) :oam-search)))))
    (update-stat-register ppu mmu)) 
