;;;; persistence-utils

(in-package :clojure-cl.persistence-utils)

(defstruct atomic-reference
  (val nil))

(defun atomic-reference-get (ref) (atomic-reference-val ref))

(defun copy-simple-array (arr)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t) arr))
  (let* ((len (length arr))
	 (r (make-array len)))
    (dotimes (i len)
      (setf (aref r i) (aref arr i)))
    r))

(defun array-copy (src src-pos dest dest-start length)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t) src dest)
	   (type fixnum src-pos dest-start length))
  (loop repeat length
	for src-index fixnum from src-pos
	for dest-index fixnum from dest-start
	do (setf (aref dest dest-index) (aref src src-index))))
