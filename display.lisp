(in-package :lispboy)

(defconstant +screen-width+ 160)
(defconstant +screen-height+ 144)
(defconstant +scale+ 3)

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
  (framebuffer (make-array (* +screen-width+ +screen-height+) 
                          :element-type '(unsigned-byte 32) 
                          :initial-element +color-lightest+)))

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
     ;; Create pixel array for texture update
     (let ((pixels (make-array (* +screen-width+ +screen-height+ 4)
                              :element-type '(unsigned-byte 8))))
       ;; Copy framebuffer to pixel array with proper format
       (loop for i from 0 below (* +screen-width+ +screen-height+)
             for pixel = (aref (ppu-framebuffer ppu) i)
             for base = (* i 4)
             do (setf (aref pixels base) (ldb (byte 8 16) pixel)     ; R
                      (aref pixels (+ base 1)) (ldb (byte 8 8) pixel) ; G
                      (aref pixels (+ base 2)) (ldb (byte 8 0) pixel) ; B
                      (aref pixels (+ base 3)) 255))                  ; A
       
       ;; Update texture with new pixels
       (sdl2:update-texture *texture* nil pixels (* 4 +screen-width+))
       
       ;; Clear renderer and copy texture
       (sdl2:set-render-draw-color *renderer* 0 0 0 255)
       (sdl2:render-clear *renderer*)
       (sdl2:render-copy *renderer* *texture*)
       (sdl2:render-present *renderer*)))))

(defun cleanup-display ()
  "Clean up SDL2 resources"
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
          for x = (mod (+ screen-x scx) 256)
          for tile-col = (floor x 8)
          for pixel-x = (mod x 8)
          do
             (let* (;; Get the tile number from the map
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
               ;; Set the pixel in the framebuffer
               (setf (aref (ppu-framebuffer ppu)
                          (+ (* ly +screen-width+) screen-x))
                     color)))))

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
  
(defun draw-window-scanline (ppu ly)
  "Draw window tiles for current scanline"
  ;; Similar to background but using window registers
  )

(defun draw-sprites-scanline (ppu ly)
  "Draw sprites for current scanline"
  ;; Implementation for sprite rendering
  )
