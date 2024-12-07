(in-package :lispboy)

(defun get-implementation-status (opcode)
  "Check if an opcode is implemented in execute-instruction"
  (let ((mnemonic (intern (cdr (assoc :mnemonic opcode)) :keyword)))
    (case mnemonic
      (:NOP t)
      (:LD t)
      (:LDH t)
      (:INC t)
      (:DEC t)
      (:PUSH t)
      (:POP t)
      (:DI t)
      (:JP t)
      (:CALL t)
      (:RET t)
      (:JR t)
      (otherwise nil))))

(defun check-opcode-coverage ()
  "Check and report coverage of CPU opcodes"
  (format t "~%Checking CPU opcode coverage...~%~%")
  (let ((total-opcodes 0)
        (implemented-count 0)
        (unimplemented-opcodes '()))

    ;; Check regular opcodes
    (format t "Regular Opcodes:~%")
    (format t "---------------~%")
    (maphash 
     (lambda (code opcode)
       (incf total-opcodes)
       (let ((mnemonic (cdr (assoc :mnemonic opcode))))
         (if (get-implementation-status opcode)
             (incf implemented-count)
             (push (cons code mnemonic) unimplemented-opcodes))))
     unprefixed-opcodes)

    ;; Check CB-prefixed opcodes
    (format t "~%CB-Prefixed Opcodes:~%")
    (format t "-------------------~%")
    (maphash 
     (lambda (code opcode)
       (incf total-opcodes)
       (let ((mnemonic (cdr (assoc :mnemonic opcode))))
         (if (get-implementation-status opcode)
             (incf implemented-count)
             (push (cons (logior #xCB00 code) mnemonic) unimplemented-opcodes))))
     cbprefixed-opcodes)

    ;; Report results
    (format t "~%Coverage Summary:~%")
    (format t "----------------~%")
    (format t "Total opcodes: ~D~%" total-opcodes)
    (format t "Implemented: ~D~%" implemented-count)
    (format t "Missing: ~D~%" (- total-opcodes implemented-count))
    (format t "Coverage: ~,1F%~%" (* 100 (/ implemented-count total-opcodes)))

    ;; List unimplemented opcodes
    (format t "~%Unimplemented Opcodes:~%")
    (format t "---------------------~%")
    (dolist (opcode (sort unimplemented-opcodes #'< :key #'car))
      (format t "0x~4,'0X: ~A~%" (car opcode) (cdr opcode)))))

(defun analyze-opcode-groups ()
  "Analyze and report on groups of unimplemented opcodes"
  (format t "~%Analyzing opcode groups...~%~%")
  (let ((mnemonic-counts (make-hash-table :test #'equal)))
    
    ;; Count regular opcodes
    (maphash 
     (lambda (code opcode)
       (let ((mnemonic (cdr (assoc :mnemonic opcode))))
         (unless (get-implementation-status opcode)
           (incf (gethash mnemonic mnemonic-counts 0)))))
     unprefixed-opcodes)
    
    ;; Count CB-prefixed opcodes
    (maphash 
     (lambda (code opcode)
       (let ((mnemonic (cdr (assoc :mnemonic opcode))))
         (unless (get-implementation-status opcode)
           (incf (gethash mnemonic mnemonic-counts 0)))))
     cbprefixed-opcodes)
    
    ;; Sort and display results
    (let ((sorted-counts nil))
      (maphash 
       (lambda (mnemonic count)
         (push (cons mnemonic count) sorted-counts))
       mnemonic-counts)
      
      (setf sorted-counts (sort sorted-counts #'> :key #'cdr))
      
      (format t "Missing instruction groups:~%")
      (format t "-------------------------~%")
      (dolist (count-pair sorted-counts)
        (format t "~A: ~D unimplemented opcodes~%" 
                (car count-pair) (cdr count-pair))))))

(export '(check-opcode-coverage analyze-opcode-groups))