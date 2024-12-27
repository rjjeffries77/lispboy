(in-package :lispboy)
(defun custom-name-to-lisp (string)
  (if
    ;; Convert hex strings to integers for opcodes
    (cl-ppcre:scan "^0x[0-9A-Fa-f]+$" string) 
    string
    (json:camel-case-to-lisp string)))

(setf json:*json-identifier-name-to-lisp* #'custom-name-to-lisp)

(defvar opcodes
  (let ((source-dir (asdf:system-source-directory (asdf:find-system :lispboy))))
    (with-open-file (stream (merge-pathnames #P"Opcodes.json" source-dir))
      (json:decode-json stream))))

(defun opcode-hashmap (opcode-a)
  (let ((table (make-hash-table)))
    ;; Add debug print to see what we're processing
    ;(format t "Creating hash table with ~A entries~%" (length opcode-a))
    (dolist (pair opcode-a table)
     ; (format t "Processing: ~A -> ~A~%" (car pair) (cdr pair))
      (setf (gethash (parse-integer (subseq (string (car pair)) 2) :radix 16) table) 
            (cdr pair)))
    ;; Print a sample lookup to verify
    ;(format t "Sample lookup 0x00: ~A~%" (gethash 0 table))
    table))

(defparameter unprefixed-opcodes (opcode-hashmap (cdr (assoc :unprefixed opcodes))))
(defparameter cbprefixed-opcodes (opcode-hashmap (cdr (assoc :cbprefixed opcodes))))

(defvar cbprefixed-opcodes (cdr (assoc :cbprefixed opcodes)))

(defun get-mnemonic (opcode)
  (cdr (assoc :mnemonic opcode)))

(defun get-operands (opcode)
  (cdr (assoc :operands opcode)))

; for testing
(export '(unprefixed-opcodes cbprefixed-opcodes))
