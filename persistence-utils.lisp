;;;; persistence-utils

(in-package :clojure-cl.persistence-utils)

(defstruct atomic-reference
  (val nil))

(defun atomic-reference-get (ref) (atomic-reference-val ref))
