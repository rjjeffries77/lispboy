(require :asdf)
(push (directory-namestring *load-truename*) asdf:*central-registry*)
(asdf:load-system :lispboy)

(defun reload ()
  (asdf:load-system :lispboy :force t))