;;;; hash-table.lisp

(in-package :clojure-cl.hash-table)

(defstruct atomic-reference
  (val nil))

(defun atomic-reference-get (ref) (atomic-reference-val ref))

(defstruct map-entry
  (key nil :read-only t)
  (val nil :read-only t))

(defstruct box
  (val nil))

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
  (edit nil :read-only t)
  (count nil :type integer)
  (root nil :type (or null hash-map-node))
  (has-null nil :type boolean)
  (null-value nil)
  (leaf-flag (make-box) :read-only t))

(defstruct hash-map-node)

(defstruct (hash-map-bitmap-node (:conc-name hmn-)
				 (:include hash-map-node))
  (edit nil)
  (bitmap nil :type integer)
  (array nil :type vector))

(defstruct (hash-map-array-node (:conc-name hman-)
				(:include hash-map-node))
  (edit nil)
  (count nil :type integer :read-only t)
  (array nil :type vector :read-only t))

(defstruct (hash-map-collision-node (:conc-name hmcn-)
				    (:include hash-map-node))
  (edit nil)
  (hash nil :type integer :read-only t)
  (count nil :type integer)
  (array nil :type vector))

(defgeneric map-assoc (hash-map key val))
(defgeneric val-at (hash-map key &optional not-found))
(defgeneric node-assoc (node shift hash key val addedLeaf))
(defgeneric node-assoc-edit (node edit shift hash key val addedLeaf))
(defgeneric node-find (node shift hash key not-found))
(defgeneric node-without (node shift hash key))
(defgeneric node-without-edit (node edit shift hash key removed-leaf))

(defvar +empty-hash-map-node+ (make-hash-map-bitmap-node :bitmap 0 :array (vector)))
(defvar +empty-hash-map+ (make-persistent-hash-map :count 0 :root nil :has-null nil :null-value nil))
(defvar +not-found+ (gensym))

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

(defun create-node (shift key1 val1 key2hash key2 val2)
  (let ((key1hash (hash key1)))
    (if (= key1hash key2hash)
	(make-hash-map-collision-node :edit nil :hash key1hash :count 2 :array (vector key1 val1 key2 val2))
	(let* ((added-leaf (make-box :val nil))
	       (edit (make-atomic-reference))
	       (n1 (node-assoc-edit +empty-hash-map-node+ edit shift key1hash key1 val1 added-leaf)))
	  (node-assoc-edit n1 edit shift key2hash key2 val2 added-leaf)))))

(defun create-edit-node (edit shift key1 val1 key2hash key2 val2)
  ;; TODO: De-duplicate?
  (let ((key1hash (hash key1)))
    (if (= key1hash key2hash)
	(make-hash-map-collision-node :edit nil :hash key1hash :count 2 :array (vector key1 val1 key2 val2))
	(let* ((added-leaf (make-box :val nil))
	       (n1 (node-assoc-edit +empty-hash-map-node+ edit shift key1hash key1 val1 added-leaf)))
	  (node-assoc-edit n1 edit shift key2hash key2 val2 added-leaf)))))

;;; Persisent hash map impl

(defun phm-as-transient (map)
  (make-transient-hash-map
   :edit (make-atomic-reference :val t) ;; TODO: thread-safety
   :count (phm-count map)
   :root (phm-root map)
   :has-null (phm-has-null map)
   :null-value (phm-null-value map)))

(defun create-persistent-hash-map (&rest args)
  (let ((r (phm-as-transient +empty-hash-map+)))
    (loop for (key val) on args by #'cddr
	  do (setf r (map-assoc r key val)))
    (thm-persistent r)))

(defmethod map-assoc ((m persistent-hash-map) key val)
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
	       (new-root (node-assoc (if (not root) +empty-hash-map-node+ root)
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

;;; transient-hash-map impl

(defun thm-do-assoc (map key val)
  (with-accessors ((has-null thm-has-null)
		   (null-value thm-null-value)
		   (count thm-count)
		   (leaf-flag thm-leaf-flag)
		   (root thm-root))
      map
    (if (not key)
	(progn (setf null-value val)
	       (unless has-null
		 (incf count)
		 (setf has-null t)))
	;; else
	(progn
	  (setf (box-val leaf-flag) nil)
	  (let ((n (node-assoc (or root +empty-hash-map-node+)
			       0 (hash key) key val leaf-flag)))
	    (unless (equal n root)
	      (setf root n))
	    (when (box-val leaf-flag) (incf count))))))
  map)

(defun thm-do-without (map key)
  (with-accessors ((has-null thm-has-null)
		   (null-value thm-null-value)
		   (count thm-count)
		   (leaf-flag thm-leaf-flag)
		   (root thm-root)
		   (edit thm-edit))
      map
    (if (not key)
	(when has-null
	  (setf has-null nil)
	  (setf null-value nil)
	  (decf count))
	(unless root
	  (setf (box-val leaf-flag) nil)
	  (let ((n (node-without-edit root edit 0 (hash key) key leaf-flag)))
	    (unless (equal n root) (setf root n))
	    (when (box-val leaf-flag) (decf count))))))
  map)

(defun thm-do-val-at (map key not-found)
  (with-accessors ((root thm-root)
		   (null-value thm-null-value)
		   (has-null thm-has-null))
      map
    (cond ((null key)
	   (if has-null
	       null-value
	       not-found))
	  ((null root) not-found)
	  (t (node-find root 0 (hash key) key not-found)))))

(defun thm-ensure-editable (map)
  (unless (atomic-reference-get (thm-edit map))
    (error "Transient used after persistent call")))

(defun thm-do-persistent (map)
  (with-accessors ((count thm-count)
		   (root thm-root)
		   (has-null thm-has-null)
		   (null-value thm-null-value)
		   (edit thm-edit))
      map
    (setf (atomic-reference-val edit) nil) ;; TODO: Thread safety
    (make-persistent-hash-map :count count :root root
			      :has-null has-null :null-value null-value)))

(defun thm-persistent (map)
  (thm-ensure-editable map)
  (thm-do-persistent map))

(defmethod map-assoc ((map transient-hash-map) key value)
  (thm-ensure-editable map)
  (thm-do-assoc map key value))

(defmethod val-at ((map transient-hash-map) key &optional not-found)
  (thm-ensure-editable map)
  (thm-do-val-at map key not-found))

;;; hash-map-array-node

(defmethod node-assoc ((this hash-map-array-node) shift hash key val added-leaf)
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      
      (if (null node)
	  (let ((new-node (node-assoc +empty-hash-map-node+
				      (+ shift 5) hash key val added-leaf)))
	    (make-hash-map-array-node
	     :edit nil
	     :count (1+ count)
	     :array (clone-and-set array idx new-node)))
	  
	  (let ((n (node-assoc node (+ shift 5) hash key val added-leaf)))
	    (if (equal n node)
		this
		(make-hash-map-array-node
		 :edit nil
		 :count count
		 :array (clone-and-set array idx n))))))))

(defun hman-ensure-editable (node edit)
  (if (equal (hman-edit node) edit)
      node
      (make-hash-map-array-node :edit edit
				:count (hman-count node)
				:array (alexandria:copy-array (hman-array node)))))

(defun hman-edit-and-set (node edit i n)
  (let ((editable (hman-ensure-editable node edit)))
    (setf (aref (hman-array editable) i) n)
    editable))

(defmethod node-assoc-edit ((this hash-map-array-node) edit shift hash key val added-leaf)
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      (if (null node)
	  (let* ((n (node-assoc-edit +empty-hash-map-node+ edit (+ 5 shift) hash key val added-leaf))
		 (editable (hman-edit-and-set this edit idx n)))
	    (incf (hman-count editable))
	    editable)
	  (let ((n (node-assoc-edit node edit (+ shift 5) hash key val added-leaf)))
	    (if (equal n node)
		this
		(hman-edit-and-set this edit idx n)))))))

(defun hman-pack (node edit idx)
  (with-accessors ((count hman-count)
		   (array hman-array))
      node
    (let ((new-array (make-array (* 2 (1- count))))
	  (j 1)
	  (bitmap 0))
      (dotimes (i idx)
	(when (aref array i)
	  (setf (aref new-array j) (aref array j))
	  (setf bitmap (logior bitmap (ash 1 i)))
	  (incf j 2)))
      (loop for i from (1+ idx) below (length array)
	    for v = (aref array i)
	    when v do
	      (setf (aref new-array j) v)
	      (setf bitmap (logior bitmap (ash 1 i)))
	      (incf j 2))
      (make-hash-map-bitmap-node :edit edit :bitmap bitmap :array new-array))))

(defmethod node-without-edit ((this hash-map-array-node) edit shift hash key removed-leaf)
  (with-accessors ((array hman-array)
		   (count hman-count))
      this
    (let* ((idx (mask hash shift))
	   (node (aref array idx)))
      (if (null node)
	  this
	  (let ((n (node-without node (+ 5 shift) hash key)))
	    (if (equal n node)
		this
		(if (null n)
		    (if (<= count 8)
			(hman-pack this nil idx)
			(make-hash-map-array-node :edit nil
						  :count (1- count)
						  :array (clone-and-set array idx n)))
		    (make-hash-map-array-node :edit nil
					      :count count
					      :array (clone-and-set array idx n)))))))))

(defmethod node-find ((this hash-map-array-node) shift hash key not-found)
  (let* ((idx (mask hash shift))
	 (node (aref (hman-array this) idx)))
    (if (null node)
	not-found
	(node-find node (+ 5 shift) hash key not-found))))

;;; hash-map-bitmap-node

(defun hmn-ensure-editable (node edit)
  (with-accessors ((this-edit hmn-edit)
		   (bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (if (equal edit this-edit)
	node
	(let* ((n (integer-count-bits bitmap))
	       (new-array (make-array (if (>= n 0) (* 2 (1+ n)) 4))))
	  (array-copy array 0 new-array 0 (* 2 n))
	  (make-hash-map-bitmap-node :edit edit :bitmap bitmap :array new-array)))))

(defun hmn-edit-and-set (node edit i a &optional j b)
  (let ((editable (hmn-ensure-editable node edit)))
    (setf (aref (hmn-array editable) i) a)
    (when j (setf (aref (hmn-array editable) j) b))
    editable))

(defun hmn-edit-and-remove-pair (node edit bit i)
  (with-accessors ((bitmap hmn-bitmap))
      node
    (when (/= bitmap bit)
      (let* ((editable (hmn-ensure-editable node edit))
	     (array (hmn-array editable))
	     (array-len (length array)))
	(setf (hmn-bitmap editable) (logxor (hmn-bitmap editable) bit))
	(array-copy array (* 2 (1+ i)) array (* 2 i) (- array-len (* 2 (1+ i))))
	(setf (aref array (- array-len 2)) nil)
	(setf (aref array (- array-len 1)) nil)
	editable))))

(defmethod node-assoc-edit ((node hash-map-bitmap-node) edit shift hash key val added-leaf)

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
	     (let ((n (node-assoc-edit value-or-node edit (+ 5 shift) hash key val added-leaf)))
	       (if (equal n value-or-node)
		   node
		   (hmn-edit-and-set node edit (1+ (* 2 idx)) n))))
	    ((equiv key key-or-null)
	     (if (equal val value-or-node)
		 node
		 (hmn-edit-and-set node edit (1+ (* 2 idx)) val)))
	    (t
	     (setf (box-val added-leaf) added-leaf)
	      (hmn-edit-and-set node edit
				(* 2 idx) nil
				(1+ (* 2 idx))
				(create-edit-node edit (+ 5 shift) key-or-null value-or-node hash key val)))))
	;; else
	(let ((n (integer-count-bits bitmap)))
	  (cond
	    ((< (* 2 n) (length array))
	     (setf (box-val added-leaf) added-leaf)
	     (let* ((editable (hmn-ensure-editable node edit))
		    (array (hmn-array editable)))
	       (array-copy array (* 2 idx) array (* 2 (1+ idx)) (* 2 (- n idx)))
	       (setf (aref array (* 2 idx)) key)
	       (setf (aref array (1+ (* 2 idx))) val)
	       (setf (hmn-bitmap editable) (logior bit (hmn-bitmap editable)))
	       editable))

	    ((>= n 16)
	     (let* ((nodes (make-array 32))
		    (jdx (mask hash shift))
		    (j 0))
	       (setf (aref nodes jdx) (node-assoc-edit +empty-hash-map-node+ edit (+ 5 shift) hash key val added-leaf))
	       (dotimes (i 32)
		 (when (/= 0 (logand (ash bitmap i) 1)) ;; TODO logbitp
		   (if (null (aref array j))
		       (setf (aref nodes i) (aref array (1+ j)))
		       (setf (aref nodes i) (node-assoc-edit +empty-hash-map-node+ edit (+ 5 shift) (hash (aref array j)) (aref array j) (aref array (1+ j)) added-leaf)))
		   (incf j 2)))
	       (make-hash-map-array-node :edit edit :count (1+ n) :array nodes)))

	    (t
	     (let ((new-array (make-array (* 2 (1+ n)))))
	       (array-copy array 0 new-array 0 (* 2 idx))
	       (setf (aref new-array (* 2 idx)) key)
	       (setf (box-val added-leaf) added-leaf)
	       (setf (aref new-array (1+ (* 2 idx))) val)
	       (array-copy array (* 2 idx) new-array (* 2 (1+ idx)) (* 2 (- n idx)))
	       (let* ((editable (hmn-ensure-editable node edit)))
		 (setf (hmn-array editable) new-array)
		 (setf (hmn-bitmap editable) (logior (hmn-bitmap editable) bit))
		 editable)))))))))

(defmethod node-assoc ((node hash-map-bitmap-node) shift hash key val added-leaf)
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
	     (let ((n (node-assoc value-or-node (+ 5 shift) hash key val added-leaf)))
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
	      (setf (aref nodes jdx) (node-assoc +empty-hash-map-node+ (+ 5 shift) hash key val added-leaf))
	      (dotimes (i 32)
		(when (/= 0 (logand (ash bitmap i) 1)) ;; TODO logbitp
		  (if (null (aref array j))
		      (setf (aref nodes i) (aref array (1+ j)))
		      (setf (aref nodes i) (node-assoc +empty-hash-map-node+ (+ 5 shift) (hash (aref array j)) (aref array j) (aref array (1+ j)) added-leaf)))
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

(defmethod node-without-edit ((node hash-map-bitmap-node) edit shift hash key removed-leaf)
  (with-accessors ((bitmap hmn-bitmap)
		   (array hmn-array))
      node
    (let ((bit (bitpos hash shift)))
      (if (= 0 (logand bitmap bit))
	  node
	  (let* ((idx (hmn-index node bit))
		 (key-or-null (aref array (* 2 idx)))
		 (val-or-node (aref array (1+ (* 2 idx)))))
	    (cond
	      ((null key-or-null)
	       (let ((n (node-without node edit (+ 5 shift) hash key removed-leaf)))
		 (cond
		   ((eql n val-or-node) node)
		   (n (hmn-edit-and-set node edit (1+ (* 2 idx)) n))
		   ((= bitmap bit) nil)
		   (t (hmn-edit-and-remove-pair node edit bit idx)))))
	      ((equiv key key-or-null))
	      (t node)))))))

;; hash-collision-node

(defun hmcn-find-index (node key)
  (loop for i from 0 by 2
	repeat (hmcn-count node)
	when (equiv key (aref (hmcn-array node) i))
	  do (return-from hmcn-find-index i))
  -1)

(defmethod node-assoc ((node hash-map-collision-node) shift hash key val added-leaf)
  (with-accessors ((this-hash hmcn-hash)
		   (array hmcn-array)
		   (count hmcn-count)
		   (edit hmcn-edit))
      node
    (if (= hash this-hash)
	(let ((idx (hmcn-find-index node key)))
	  (if (/= idx -1)
	      (if (equal val (aref array (1+ idx)))
		  node
		  (make-hash-map-collision-node :edit nil :hash hash :count count
						:array (clone-and-set array (1+ idx) val)))
	      (let ((new-array (make-array (* 2 (1+ count)))))
		(array-copy array 0 new-array 0 (* 2 count))
		(setf (aref new-array (* 2 count)) key)
		(setf (aref new-array (1+ (* 2 count))) val)
		(setf (box-val added-leaf) added-leaf)
		(make-hash-map-collision-node :edit edit :hash hash :count (1+ count) :array new-array))))

	(node-assoc (make-hash-map-bitmap-node :bitmap (bitpos this-hash shift) :array (vector nil node))
		    shift hash key val added-leaf))))

(defmethod node-without ((node hash-map-collision-node) shift hash key)
  (let ((idx (hmcn-find-index node key)))
    (cond ((= idx -1) node)
	  ((= (hmcn-count node) 1) nil)
	  (t (make-hash-map-collision-node :edit nil
					   :hash hash
					   :count (1- (hmcn-count node))
					   :array (remove-pair (hmcn-array node) (floor idx 2)))))))

(defun hmcn-ensure-editable (node edit)
  (with-accessors ((this-edit hmcn-edit)
		   (count hmcn-count)
		   (array hmcn-array)
		   (hash hmcn-hash))
      node
    (if (equal edit this-edit)
	node
	(let ((new-array (make-array (* 2 (1+ count)))))
	  (array-copy array 0 new-array 0 (* 2 count))
	  (make-hash-map-collision-node :edit edit
					:hash hash
					:count count
					:array new-array)))))

(defun hmcn-edit-and-set (node edit i a &optional j b)
  (let ((editable (hmcn-ensure-editable node edit)))
    (setf (aref (hmcn-array editable) i) a)
    (when j (setf (aref (hmcn-array editable) j) b))
    editable))

(defmethod node-assoc-edit ((node hash-map-collision-node) edit shift hash key val added-leaf)
  (with-accessors ((this-hash hmcn-hash)
		   (array hmcn-array)
		   (count hmcn-count))
      node
    (if (= hash this-hash)
	(let ((idx (hmcn-find-index node key)))
	  (cond ((/= idx -1)
		 (if (equal val (aref array (1+ idx)))
		     node
		     (hmcn-edit-and-set node edit (1+ idx) val)))

		((> (length array) (* 2 count))
		 (setf (box-val added-leaf) added-leaf)
		 (let ((editable (hmcn-edit-and-set node edit
						    (* 2 count) key
						    (1+ (* 2 count)) val)))
		   (incf (hmcn-count editable))
		   editable))))

	(node-assoc-edit
	 (make-hash-map-bitmap-node :edit edit :bitmap (bitpos this-hash shift)
				    :array (vector nil node nil nil))
	 edit shift hash key val added-leaf))))

(defmethod node-without-edit ((node hash-map-collision-node) edit shift hash key removed-leaf)
  (let ((idx (hmcn-find-index node key))
	(count (hmcn-count node)))
    (if (= idx -1)
	node
	(progn
	  (setf (box-val removed-leaf) removed-leaf)
	  (unless (= 1 count)
	    (let* ((editable (hmcn-ensure-editable node edit))
		   (array (hmcn-array editable)))
	      (setf (aref array idx) (aref array (- (* 2 count) 2)))
	      (setf (aref array (1+ idx)) (aref array (- (* 2 count) 1)))
	      (setf (aref array (- (* 2 idx) 2)) nil)
	      (setf (aref array (- (* 2 idx) 1)) nil)
	      (decf (hmcn-count editable))
	      editable))))))

;; Print

(defmethod print-object ((map persistent-hash-map) stream)
  (format stream "<Map Count:~a>" (phm-count map)))
