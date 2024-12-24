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


(defvar *window* nil)
(defvar *renderer* nil)
(defvar *texture* nil)

(defun init-display ()
  "Initialize the SDL2 window and renderer"
  (sdl2:init :video)
  (handler-case
      (multiple-value-bind (win renderer)
          (sdl2:create-window-and-renderer 
           (* +screen-width+ +scale+)
           (* +screen-height+ +scale+)
           '(:shown))
        (setf *window* win
              *renderer* renderer
              *texture* (sdl2:create-texture renderer :rgb888 :streaming 
                                           +screen-width+ +screen-height+))
        (sdl2:set-window-title *window* "LispBoy"))
    (error (c)
      (format t "Error during display initialization: ~A~%" c)
      (finish-output)
      (error c))))

(defun update-display (ppu)
  "Update the display with current framebuffer contents"
  (sdl2:with-event-loop (:method :poll)
    (:quit () t)
    (:idle ()
     ;; Update texture directly from foreign memory
     (sdl2:update-texture *texture* nil 
                         (ppu-framebuffer ppu)
                         (* 4 +screen-width+))
     
     ;; Clear renderer and copy texture
     (sdl2:set-render-draw-color *renderer* 0 0 0 255)
     (sdl2:render-clear *renderer*)
     (sdl2:render-copy *renderer* *texture*)
     (sdl2:render-present *renderer*))))

(defun cleanup-display (ppu)
  (when (not (cffi:null-pointer-p (ppu-framebuffer ppu)))
    (cffi:foreign-free (ppu-framebuffer ppu))
    (setf (ppu-framebuffer ppu) (cffi:null-pointer)))
  (when *texture* 
    (sdl2:destroy-texture *texture*)
    (setf *texture* nil))
  (when *renderer*
    (sdl2:destroy-renderer *renderer*)
    (setf *renderer* nil))
  (when *window*
    (sdl2:destroy-window *window*)
    (setf *window* nil))
  (sdl2:quit))

;; Helper function to set pixel in foreign framebuffer
(defun set-pixel (ppu x y color)
  (when (and (>= x 0) (< x +screen-width+)
             (>= y 0) (< y +screen-height+))
    (setf (cffi:mem-aref (ppu-framebuffer ppu) :uint32 
                         (+ (* y +screen-width+) x))
          color)))

(defun draw-scanline (ppu mmu ly)
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