;;;; reader-macros

(in-package #:clojure-cl.reader-macros)

;; vars

(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defvar *previous-readtables* nil)

;; hash-table-reader

(defun hash-table-reader (stream char)
  (declare (ignore char)
	   (optimize (speed 3) (safety 0)))
  (let ((lst (read-delimited-list +right-brace+ stream nil)))
    (when (oddp (length lst)) (error "Odd number of pairs in hash-table"))
    `(clojure-cl.hash-table:create-persistent-map ,@lst)))

(defun no-matching-brace (stream char)
  (declare (ignore stream char))
  (error "No matching { for }"))

;; vector reader

(defun vector-reader (stream char)
  (declare (ignore char)
	   (optimize (speed 3) (safety 0)))
  (let ((lst (read-delimited-list +right-bracket+ stream nil)))
    `(clojure-cl.vector:create-persistent-vector ,@lst)))

(defun no-matching-bracket (stream char)
  (declare (ignore stream char))
  (error "No matching [ for ]"))

;; enable/disable reader macros

(defun enable-clojure-reader-macros ()
  (push *readtable* *previous-readtables*)
  (setf *readtable* (copy-readtable))
  (set-macro-character +left-brace+ 'hash-table-reader)
  (set-macro-character +right-brace+ 'no-matching-brace)
  (set-macro-character +left-bracket+ 'vector-reader)
  (set-macro-character +right-bracket+ 'no-matching-bracket))

(defun disable-clojure-reader-macros ()
  (setf *readtable* (pop *previous-readtables*)))
