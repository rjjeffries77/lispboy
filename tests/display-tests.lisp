(in-package :lispboy)

;; Test utilities
(defun create-test-ppu ()
  "Create a PPU instance with known initial state"
  (let ((ppu (make-ppu)))
    (setf (ppu-lcdc ppu) #xFF)  ; All features enabled
    (setf (ppu-bgp ppu) #xE4)   ; Standard Game Boy palette
    (setf (ppu-scx ppu) 0)
    (setf (ppu-scy ppu) 0)
    ppu))

(defun create-test-mmu ()
  "Create an MMU instance with test pattern"
  (let ((mmu (make-mmu)))
    ;; Write a simple tile pattern to VRAM
    ;; This creates a simple 8x8 checkerboard pattern
    (loop for i from 0 below 16
          do (write-memory mmu (+ #x8000 i) 
                          (if (evenp i) #xAA #x55)))
    
    ;; Set up tile map
    (write-memory mmu #x9800 0)  ; First tile is our test pattern
    mmu))

;; Basic framebuffer tests
(defun test-set-pixel ()
  "Test basic pixel setting functionality"
  (let ((ppu (create-test-ppu)))
    (format t "Testing set-pixel...~%")
    
    ;; Test setting pixels at various positions
    (set-pixel ppu 0 0 +color-darkest+)
    (set-pixel ppu 159 143 +color-lightest+)
    
    ;; Verify pixels were set correctly
    (let ((color0 (cffi:mem-aref (ppu-framebuffer ppu) :uint32 0))
          (color-last (cffi:mem-aref (ppu-framebuffer ppu) :uint32 
                                    (+ (* 143 +screen-width+) 159))))
      (format t "First pixel color: ~X (expected ~X)~%" 
              color0 +color-darkest+)
      (format t "Last pixel color: ~X (expected ~X)~%" 
              color-last +color-lightest+)
      
      ;; Cleanup
      (cleanup-display ppu)
      
      ;; Return test results
      (and (= color0 +color-darkest+)
           (= color-last +color-lightest+)))))

;; Test background rendering
(defun test-background-scanline ()
  "Test background scanline rendering"
  (let ((ppu (create-test-ppu))
        (mmu (create-test-mmu)))
    (format t "Testing background scanline rendering...~%")
    
    ;; Draw first scanline
    (draw-background-scanline ppu mmu 0)
    
    ;; Check first 8 pixels of the scanline
    (let ((expected-pattern #(#x9BBC0F #x8BAC0F #x9BBC0F #x8BAC0F 
                            #x9BBC0F #x8BAC0F #x9BBC0F #x8BAC0F)))
      (loop for x from 0 below 8
            for expected across expected-pattern
            for actual = (cffi:mem-aref (ppu-framebuffer ppu) :uint32 x)
            do (format t "Pixel ~D: ~X (expected ~X)~%" 
                      x actual expected)
            unless (= actual expected)
            do (format t "Mismatch at pixel ~D~%" x)
               (return-from test-background-scanline nil)))
    
    ;; Cleanup
    (cleanup-display ppu)
    t))

;; Test full display update cycle
(defun test-display-update ()
  "Test complete display update cycle"
  (format t "Testing display update...~%")
  (let ((ppu (create-test-ppu)))
    (handler-case
        (progn
          ;; Initialize display
          (init-display)
          (format t "Display initialized~%")
          
          ;; Draw a test pattern
          (loop for y from 0 below +screen-height+
                do (loop for x from 0 below +screen-width+
                        do (set-pixel ppu x y 
                                    (if (evenp (+ x y))
                                        +color-light+
                                        +color-dark+))))
          
          ;; Try to update display
          (update-display ppu)
          (format t "Display updated successfully~%")
          
          ;; Sleep briefly to see the result
          (sleep 0.1)
          
          ;; Cleanup
          (cleanup-display ppu)
          t)
      (error (c)
        (format t "Error during display test: ~A~%" c)
        nil))))

;; Run all tests
(defun run-display-tests ()
  "Run all display system tests"
  (format t "Running display system tests...~%~%")
  (let ((results (list
                 (cons "set-pixel" (test-set-pixel))
                 (cons "background-scanline" (test-background-scanline))
                 (cons "display-update" (test-display-update)))))
    
    (format t "~%Test Results:~%")
    (loop for (test . result) in results
          do (format t "~A: ~A~%" 
                    test 
                    (if result "PASS" "FAIL")))
    
    ;; Return true only if all tests passed
    (every #'cdr results)))

;; Export test functions
(export '(run-display-tests))