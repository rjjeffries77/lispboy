(in-package :lispboy)

(defun custom-name-to-lisp (string)
  (if (cl-ppcre:scan "^0x[0-9A-Fa-f]+$" string)
      (parse-integer (subseq string 2) :radix 16) 
      (json:camel-case-to-lisp string))); Convert others to keywords

(setf json:*json-identifier-name-to-lisp* #'custom-name-to-lisp)

(defvar opcodes 
  (with-open-file (stream "c:/users/cms/projects/lispboy/Opcodes.json")
    (json:decode-json stream)))

(defvar unprefixed-opcodes (cdr (assoc :unprefixed opcodes)))
(defvar prefixed-opcodes (cdr (assoc :prefixed opcodes)))

(defun get-mnemonic (opcode)
  (cdr (assoc :mnemonic opcode)))

(defun get-operands (opcode)
  (cdr (assoc :operands opcode)))

