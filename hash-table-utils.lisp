;;;; hash-table-utils.lisp

(in-package :clojure-cl.hash-table)

(defun equiv (v1 v2)
  ;; TODO: Some custom equality function?
  (equal v1 v2))

(defun hash (x)
  (sxhash x))

(defun integer-count-&-bits (n1 n2)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum n1 n2))
  (logcount (logand n1 n2)))

(defun remove-pair (array i)
  (let ((new-array (make-array (- (length array) 2))))
    (array-copy array 0 new-array 0 (* 2 i))
    (array-copy array (* 2 (1+ i)) new-array (* 2 i) (- (length new-array) (* 2 i)))
    new-array))
