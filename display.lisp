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
(defconstant +vblank-end+ 153)      ; Last VBlank line
(defconstant +mode-2-cycles+ 80)    ; OAM search - 80 cycles
(defconstant +mode-3-cycles+ 172)   ; Pixel transfer - 172 cycles (minimum)
(defconstant +mode-0-cycles+ 204)   ; H-Blank - 204 cycles (maximum)
(defconstant +scanline-cycles+ 456) ; Total cycles per scanline
(defconstant +frame-cycles+ 70224)  ; Total cycles per frame

;; PPU modes
(defconstant +mode-hblank+ 0)      ; H-Blank period
(defconstant +mode-vblank+ 1)      ; V-Blank period
(defconstant +mode-oam+ 2)         ; OAM search period
(defconstant +mode-transfer+ 3)    ; Pixel transfer period

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
  (sprites nil :type list)           ; List of visible sprites for current scanline
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
       (when (>= ppu-cycle +mode-2-cycles+); Use ppu-cycle for timing
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
          (when (>= ppu-cycle +mode-0-cycles+)
            (setf (mmu-oam-lock mmu) nil)
            (setf (mmu-vram-lock mmu) nil)
            (setf (ppu-phase ppu) :oam-search)))   
         (:v-blank
          (when (>= ppu-cycle +mode-3-cycles+) ; Use ppu-cycle for timing
            (request-vblank-interrupt mmu)
            (setf (ppu-phase ppu) :oam-search)))))
    (update-stat-register ppu mmu)) 

(defun update-ppu-mode (ppu mmu scanline dot)
  "Update PPU mode based on current scanline and dot"
  (let ((old-mode (ppu-mode ppu))
        (new-mode nil))
    
    ;; Determine the new mode based on scanline and dot position
    (cond
      ;; V-Blank period (lines 144-153)
      ((>= scanline +vblank-start+)
       (setf new-mode +mode-vblank+)
       (when (and (= scanline +vblank-start+) (= dot 0))
         (request-vblank-interrupt mmu)))
      
      ;; Within visible scanline (0-143)
      (t (cond
           ;; First 80 dots: OAM Search (Mode 2)
           ((< dot +mode-2-cycles+)
            (setf new-mode +mode-oam+))
           
           ;; Next 172-289 dots: Pixel Transfer (Mode 3)
           ((< dot (+ +mode-2-cycles+ +mode-3-cycles+))
            (setf new-mode +mode-transfer+))
           
           ;; Remaining dots: H-Blank (Mode 0)
           (t (setf new-mode +mode-hblank+)))))
    
    ;; Update mode if changed
    (when (not (eql old-mode new-mode))
      (setf (ppu-mode ppu) new-mode)
      
      ;; Handle mode-specific actions
      (case new-mode
        (#.+mode-hblank+
         (when (lcdc-display-enabled-p ppu)
           (render-scanline ppu mmu scanline)))
        
        (#.+mode-vblank+
         (when (and (= scanline +vblank-start+) (= dot 0))
           (render-frame ppu)))
        
        (#.+mode-oam+
         (when (= dot 0)
           (prepare-sprites ppu mmu scanline)))
        
        (#.+mode-transfer+
         (block-vram-access mmu t)))  ; Block VRAM access during pixel transfer
      
      ;; Update STAT register and check for interrupts
      (update-stat-register ppu mmu))))

(defun prepare-sprites (ppu mmu scanline)
  "Prepare sprites for the current scanline during OAM search"
  (let ((sprite-height (if (lcdc-flag-set-p ppu +lcdc-obj-size+) 16 8))
        (visible-sprites '()))
    
    ;; Search OAM for visible sprites on this scanline
    (loop for sprite-index from 0 below 40
          for oam-base = (+ #xFE00 (* sprite-index 4))
          for sprite-y = (- (read-memory mmu oam-base) 16)
          for sprite-x = (- (read-memory mmu (+ oam-base 1)) 8)
          for tile-num = (read-memory mmu (+ oam-base 2))
          for attributes = (read-memory mmu (+ oam-base 3))
          
          ;; Check if sprite is visible on this scanline
          when (and (>= sprite-x -7)  ; At least partially visible horizontally
                   (< sprite-x +screen-width+)
                   (>= scanline sprite-y)
                   (< scanline (+ sprite-y sprite-height)))
          
          ;; Collect visible sprite data
          collect (make-sprite :x sprite-x
                             :y sprite-y
                             :tile-num tile-num
                             :attributes attributes)
          into found-sprites
          
          ;; Game Boy can only display 10 sprites per scanline
          when (>= (length found-sprites) 10)
          do (return found-sprites)
          
          finally (setf visible-sprites found-sprites))
    
    ;; Sort sprites by x-coordinate (higher x-coord gets priority)
    (setf visible-sprites 
          (sort visible-sprites #'> :key #'sprite-x))
    
    ;; Store sprites for use during pixel transfer
    (setf (ppu-sprites ppu) visible-sprites)))

(defun block-vram-access (mmu blocked)
  "Block or unblock VRAM access during pixel transfer"
  (setf (mmu-vram-blocked mmu) blocked))

(defun render-scanline (ppu mmu scanline)
  "Render a single scanline of the screen"
  (when (lcdc-display-enabled-p ppu)
    (let ((bg-enabled (lcdc-flag-set-p ppu +lcdc-bg-enable+))
          (window-enabled (and (lcdc-flag-set-p ppu +lcdc-window-enable+)
                             (>= scanline (ppu-wy ppu))))
      
      ;; Clear FIFOs
      (fifo-clear (ppu-bg-fifo ppu))
      (fifo-clear (ppu-sprite-fifo ppu))
      
      ;; Reset fetcher state
      (setf (fetcher-state (ppu-fetcher ppu)) :get-tile
            (fetcher-x (ppu-fetcher ppu)) 0
            (fetcher-pushed (ppu-fetcher ppu)) 0)
      
      ;; Render background and window
      (loop with x = 0
            while (< x +screen-width+)
            do
            ;; Fill background FIFO
            (when (< (fifo-size (ppu-bg-fifo ppu)) 8)
              (fetch-background-tile ppu mmu scanline))
            
            ;; Check if we should switch to window
            (when (and window-enabled 
                      (>= x (- (ppu-wx ppu) 7))
                      (not (eq (fetcher-state (ppu-fetcher ppu)) :window)))
              (fifo-clear (ppu-bg-fifo ppu))
              (setf (fetcher-state (ppu-fetcher ppu)) :window
                    (fetcher-x (ppu-fetcher ppu)) 0))
            
            ;; Mix sprites with background
            (when (lcdc-flag-set-p ppu +lcdc-obj-enable+)
              (mix-sprites ppu mmu scanline x))
            
            ;; Output pixel
            (when (> (fifo-size (ppu-bg-fifo ppu)) 0)
              (let* ((bg-pixel (fifo-pop (ppu-bg-fifo ppu)))
                     (sprite-pixel (fifo-pop (ppu-sprite-fifo ppu)))
                     (final-color (mix-pixels bg-pixel sprite-pixel bg-enabled)))
                (set-pixel ppu x scanline final-color)
                (incf x))))
      
      ;; Increment window line counter if window was rendered
      (when window-enabled
        (incf (ppu-window-line ppu))))))

(defun fetch-background-tile (ppu mmu scanline)
  "Fetch and decode background or window tile data"
  (let* ((fetcher (ppu-fetcher ppu))
         (window-mode (eq (fetcher-state fetcher) :window))
         (tile-y (if window-mode
                    (floor (ppu-window-line ppu) 8)
                    (floor (+ scanline (ppu-scy ppu)) 8)))
         (tile-x (if window-mode
                    (floor (fetcher-x fetcher) 8)
                    (floor (+ (fetcher-x fetcher) (ppu-scx ppu)) 8)))
         (map-base (if window-mode
                      (if (lcdc-flag-set-p ppu +lcdc-window-tile-map+)
                          #x9C00 #x9800)
                      (if (lcdc-flag-set-p ppu +lcdc-bg-tile-map+)
                          #x9C00 #x9800)))
         (map-addr (+ map-base 
                     (mod (* 32 tile-y) #x400) 
                     (mod tile-x 32)))
         (tile-num (read-memory mmu map-addr))
         (tile-addr (get-tile-data-address ppu tile-num))
         (line-in-tile (mod (if window-mode
                               (ppu-window-line ppu)
                               (+ scanline (ppu-scy ppu)))
                           8))
         (data-addr (+ tile-addr (* line-in-tile 2))))
    
    ;; Read tile data
    (let ((data-low (read-memory mmu data-addr))
          (data-high (read-memory mmu (1+ data-addr))))
      
      ;; Push 8 pixels to FIFO
      (loop for bit from 7 downto 0
            for color-num = (logior (ash (logand (ash data-high (- bit)) 1) 1)
                                  (logand (ash data-low (- bit)) 1))
            do (fifo-push (ppu-bg-fifo ppu)
                         (get-color (ppu-bgp ppu) color-num)))
      
      ;; Update fetcher state
      (incf (fetcher-x fetcher) 8))))

(defun mix-sprites (ppu mmu scanline x)
  "Mix sprite pixels with background for current x position"
  (loop for sprite in (ppu-sprites ppu)
        when (and (>= x (sprite-x sprite))
                  (< x (+ (sprite-x sprite) 8)))
        do
        (let* ((sprite-x-flip (not (zerop (logand (sprite-attributes sprite)
                                                 +sprite-x-flip+))))
               (sprite-y-flip (not (zerop (logand (sprite-attributes sprite)
                                                 +sprite-y-flip+))))
               (behind-bg (not (zerop (logand (sprite-attributes sprite)
                                            +sprite-priority+))))
               (use-obp1 (not (zerop (logand (sprite-attributes sprite)
                                           +sprite-palette+))))
               (palette (if use-obp1 (ppu-obp1 ppu) (ppu-obp0 ppu)))
               (sprite-line (- scanline (sprite-y sprite)))
               (flipped-y (if sprite-y-flip
                             (- 7 sprite-line)
                             sprite-line))
               (tile-addr (+ #x8000 (* (sprite-tile-num sprite) 16) 
                            (* flipped-y 2)))
               (data-low (read-memory mmu tile-addr))
               (data-high (read-memory mmu (1+ tile-addr)))
               (pixel-x (- x (sprite-x sprite)))
               (flipped-x (if sprite-x-flip
                             (- 7 pixel-x)
                             pixel-x))
               (color-num (logior (ash (logand (ash data-high (- flipped-x)) 1) 1)
                                (logand (ash data-low (- flipped-x)) 1))))
          
          ;; Only push non-transparent pixels
          (unless (zerop color-num)
            (fifo-push (ppu-sprite-fifo ppu)
                      (cons (get-color palette color-num) behind-bg))))))

(defun mix-pixels (bg-pixel sprite-pixel bg-enabled)
  "Mix background and sprite pixels according to priority rules"
  (cond
    ;; No sprite pixel
    ((null sprite-pixel) bg-pixel)
    
    ;; Background disabled
    ((not bg-enabled) (car sprite-pixel))
    
    ;; Sprite behind background and background pixel is not transparent
    ((and (cdr sprite-pixel) (not (= bg-pixel (aref +color-map+ 0))))
     bg-pixel)
    
    ;; Use sprite pixel
    (t (car sprite-pixel))))

(defun render-frame (ppu)
  "Render the entire screen"
  ;; Implementation of render-frame function
  )
