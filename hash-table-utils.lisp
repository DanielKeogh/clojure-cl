;;;; hash-table-utils.lisp

(in-package :clojure-cl.hash-table)

(defun equiv (v1 v2)
  ;; TODO: Some custom equality function?
  (equal v1 v2))

(defun array-copy (src src-pos dest dest-start length)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t) src dest)
	   (type fixnum src-pos dest-start length))
  (loop repeat length
	for src-index from src-pos
	for dest-index from dest-start
	do (setf (aref dest dest-index) (aref src src-index))))

(defun hash (x)
  (sxhash x))

(defun copy-simple-array (arr)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t) arr))
  (let* ((len (length arr))
	 (r (make-array len)))
    (dotimes (i len)
      (setf (aref r i) (aref arr i)))
    r))

(defun integer-count-&-bits (n1 n2)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum n1 n2))
  (logcount (logand n1 n2)))

(defun remove-pair (array i)
  (let ((new-array (make-array (- (length array) 2))))
    (array-copy array 0 new-array 0 (* 2 i))
    (array-copy array (* 2 (1+ i)) new-array (* 2 i) (- (length new-array) (* 2 i)))
    new-array))
