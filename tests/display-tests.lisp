(in-package :lispboy)

(defun create-test-tiles ()
  "Create an array of test tiles"
  (let ((tiles (make-array 384 :initial-element nil))) ; Game Boy has 384 tiles
    ;; Create some test patterns
    ;; Each tile is 8x8 pixels, 2 bits per pixel = 16 bytes per tile
    
    ;; Tile 0: Solid block
    (setf (aref tiles 0)
          #(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
            #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
    
    ;; Tile 1: Checkerboard
    (setf (aref tiles 1)
          #(#xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55
            #xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55))
    
    ;; Tile 2: Border
    (setf (aref tiles 2)
          #(#xFF #xFF #x81 #x81 #x81 #x81 #xFF #xFF
            #xFF #xFF #x81 #x81 #x81 #x81 #xFF #xFF))
    
    tiles))

(defun draw-test-tile (ppu tile-data x y)
  "Draw a single tile at specified position"
  (loop for row below 8
        for byte1 = (aref tile-data (* row 2))
        for byte2 = (aref tile-data (1+ (* row 2)))
        do (loop for bit below 8
                 ;; Game Boy uses 2 bits per pixel
                 for color-id = (logior
                               (ash (logand (ash byte1 (- bit 7)) 1) 1)
                               (logand (ash byte2 (- bit 7)) 1))
                 for color = (case color-id
                             (0 +color-lightest+)
                             (1 +color-light+)
                             (2 +color-dark+)
                             (3 +color-darkest+))
                 do (when (and (< (+ x bit) +screen-width+)
                             (< (+ y row) +screen-height+))
                      (setf (aref (ppu-framebuffer ppu)
                                (+ (* (+ y row) +screen-width+)
                                   (+ x bit)))
                            color)))))

(defun test-tile-patterns ()
  "Display a set of test tiles"
  (let ((ppu (make-ppu))
        (tiles (create-test-tiles)))
    (init-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-press-event (:key key)
        (when (eq key :sdl-key-escape)
          (sdl:push-quit-event)))
      (:idle ()
       ;; Clear screen
       (sdl:clear-display sdl:*black*)
       ;; Draw test tiles
       (loop for i below 3 ; Number of test tiles
             for x = (* i 10)
             for y = 10
             do (draw-test-tile ppu (aref tiles i) (* x 8) (* y 8)))
       ;; Update display
       (update-display ppu)))))

;; Export tile test function
(export '(test-tile-patterns))