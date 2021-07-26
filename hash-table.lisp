;;;; hash-table.lisp

(in-package :clojure-cl.hash-table)

(defstruct map-entry
  (key nil :read-only t)
  (val nil :read-only t))

(defstruct chash-map)

(defstruct (persistent-hash-map (:conc-name phm-)
				(:include chash-map))
  (meta nil :type (or null chash-map))
  (count nil :type integer :read-only t)
  (root nil :type (or null hash-map-node) :read-only t)
  (has-null nil :type boolean :read-only t)
  (null-value nil :read-only t))

(defstruct (transient-hash-map (:conc-name thm-)
			       (:include chash-map))
  (count nil :type integer)
  (root nil :type (or null hash-map-node))
  (has-null nil :type boolean)
  (null-value nil))

(defstruct hash-map-node)

(defstruct (hash-map-bitmap-node (:conc-name hmn-)
				 (:include hash-map-node))
  (bitmap nil :type integer)
  (array nil :type vector))

(defstruct (hash-map-array-node (:conc-name hman-)
				(:include hash-map-node))
  (count nil :type integer :read-only t)
  (array nil :type vector :read-only t))

(defstruct (hash-map-collision-node (:conc-name hmcn-)
				    (:include hash-map-node))
  (hash nil :type integer :read-only t)
  (count nil :type integer)
  (array nil :type vector))

(defgeneric assoc-node (node shift key1hash key1 val1 addedLeaf))
(defgeneric assoc-map (hash-map key val))
(defgeneric val-at (hash-map key &optional not-found))
(defgeneric node-find (node shift hash key not-found))

(defvar +empty-hash-map-node+ (make-hash-map-bitmap-node :bitmap 0 :array (vector)))
(defvar +empty-hash-map+ (make-persistent-hash-map :count 0 :root nil :has-null nil :null-value nil))
(defvar +not-found+ (gensym))

(defstruct box
  (val nil))

(defun integer-count-bits (n)
  (loop for i from 0 to 63
	count (logbitp i n)))

(defun integer-count-&-bits (n1 n2)
  (loop for i from 0 to 63
	count (and (logbitp i n1) (logbitp i n2))))

(defun hmn-index (node bit)
  (integer-count-&-bits (hmn-bitmap node) (1- bit)))

(defun mask (hash shift)
  (logand (ash hash (* -1 shift)) 31))

(defun bitpos (hash shift)
  (ash 1 (mask hash shift)))

(defun clone-and-set (array i new-val &optional j new-val2)
  (let ((new-array (alexandria:copy-array array)))
    (setf (aref new-array i) new-val)
    (when j
      (setf (aref new-array j) new-val2))
    new-array))

(defun equiv (v1 v2)
  ;; TODO: Some custom equality function?
  (equal v1 v2))

(defun array-copy (src src-pos dest dest-start length)
  (loop repeat length
	for src-index from src-pos
	for dest-index from dest-start
	do (setf (aref dest dest-index) (aref src src-index))))

(defun hash (x)
  (sxhash x))

(defun create-node (shift key1 val1 key2hash key2 val2)
  (let ((key1hash (hash key1)))
    (if (= key1hash key2hash)
	(make-hash-map-collision-node :hash key1hash :count 2 :array (vector key1 val1 key2 val2))
	(let* ((added-leaf (make-box :val nil))
	       (n1 (assoc-node +empty-hash-map-node+ shift key1hash key1 val1 added-leaf)))
	  (assoc-node n1 shift key2hash key2 val2 added-leaf)))))

(defmethod assoc-map ((m persistent-hash-map) key val)
  (with-accessors ((has-null phm-has-null)
		   (null-value phm-null-value)
		   (count phm-count)
		   (root phm-root)
		   (meta phm-meta))
      m
    (if (not key)
	(if (and has-null (eq val null-value))
	    m
	    (make-persistent-hash-map :meta meta
				      :count (if has-null count (1+ count))
				      :root root
				      :has-null t
				      :null-value val))
	(let* ((added-leaf (make-box :val nil))
	       (new-root (assoc-node (if (not root) +empty-hash-map-node+ root)
				     0 (hash key) key val added-leaf)))
	  (if (equal new-root root)
	      m
	      (make-persistent-hash-map :meta meta 
					:count (if (box-val added-leaf)
						   (1+ count)
						   count)
					:root new-root
					:has-null has-null
					:null-value null-value))))))

(defmethod val-at ((map persistent-hash-map) key &optional not-found)
  (with-accessors ((root phm-root)
		   (has-null phm-has-null)
		   (null-value phm-null-value))
      map
    (if (not key)
	(if has-null null-value not-found)
	(if root (node-find root 0 (hash key) key not-found) not-found))))

;;; hash-map-bitmap-node

(defmethod assoc-node ((node hash-map-bitmap-node) shift hash key val added-leaf)
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (let* ((bit (bitpos hash shift))
	   (idx (hmn-index node bit)))
      (if (/= 0 (logand bitmap bit))
	(let ((key-or-null (aref array (* 2 idx)))
	      (value-or-node (aref array (1+ (* 2 idx)))))
	  (cond
	    ((null key-or-null)
	     (let ((n (assoc-node value-or-node (+ 5 shift) hash key val added-leaf)))
	       (if (equal n value-or-node)
		   node
		   (make-hash-map-bitmap-node :bitmap bitmap
					      :array (clone-and-set array (1+ (* 2 idx)) n)))))
	    ((equiv key key-or-null)
	     (if (equal val value-or-node)
		 node
		 (make-hash-map-bitmap-node :bitmap bitmap
					    :array (clone-and-set array (1+ (* 2 idx)) val))))
	    (t
	     (setf (box-val added-leaf) added-leaf)
	     (make-hash-map-bitmap-node :bitmap bitmap
				 :array (clone-and-set array
						       (* 2 idx) nil
						       (1+ (* 2 idx))
						       (create-node (+ 5 shift) key-or-null value-or-node hash key val))))))
	;; else
	(let ((n (integer-count-bits bitmap)))
	  (if (>= n 16)
	    (let* ((nodes (make-array 32))
		   (jdx (mask hash shift))
		   (j 0))
	      (setf (aref nodes jdx) (assoc-node +empty-hash-map-node+ (+ 5 shift) hash key val added-leaf))
	      (dotimes (i 32)
		(when (/= 0 (logand (ash bitmap i) 1)) ;; TODO logbitp
		  (if (null (aref array j))
		      (setf (aref nodes i) (aref array (1+ j)))
		      (setf (aref nodes i) (assoc-node +empty-hash-map-node+ (+ 5 shift) (hash (aref array j)) (aref array j) (aref array (1+ j)) added-leaf)))
		  (incf j 2)))
	      (make-hash-map-array-node :count (1+ n) :array nodes))
	    ; else
	    (let ((new-array (make-array (* 2 (1+ n)))))
	      (array-copy array 0 new-array 0 (* 2 idx))
	      (setf (aref new-array (* 2 idx)) key)
	      (setf (box-val added-leaf) added-leaf)
	      (setf (aref new-array (1+ (* 2 idx))) val)
	      (array-copy array (* 2 idx) new-array (* 2 (1+ idx)) (* 2 (- n idx)))
	      (make-hash-map-bitmap-node :bitmap (logior bitmap bit) :array new-array))))))))

(defmethod node-find ((node hash-map-bitmap-node) shift hash key not-found)
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (let ((bit (bitpos hash shift)))
      (if (= (logand bitmap bit) 0)
	  not-found
	  (let* ((idx (hmn-index node bit))
		 (key-or-null (aref array (* 2 idx)))
		 (val-or-node (aref array (1+ (* 2 idx)))))
	    (cond ((not key-or-null) (node-find val-or-node (+ 5 shift) hash key not-found))
		  ((equiv key key-or-null) val-or-node)
		  (t not-found)))))))


;; Print

(defmethod print-object ((map persistent-hash-map) stream)
  (format stream "<Map Count:~a>" (phm-count map)))
