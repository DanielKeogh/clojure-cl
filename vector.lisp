;;;; vector.lisp

(in-package :clojure-cl.vector)

;;; definition utils

(defconstant +chunk-size+ 32)
(defconstant +chunk-mask+ (1- +chunk-size+))
(defconstant +chunk-bit+ (round (log 32 2)))
;; +chunk-size+ must be a power of 2 or else #'ash tricks dont work
(assert (= +chunk-size+ (expt 2 +chunk-bit+))) 

(defun make-array-chunk ()
  (declare (optimize (speed 3) (safety 0)))
  (make-array +chunk-size+ :initial-element nil))

;;; struct definitions

(defstruct (vector-node (:conc-name vn-))
  (edit nil :type (or null atomic-reference) :read-only t)
  (array (make-array-chunk) :type (simple-array t) :read-only t))

(defstruct (persistent-vector (:include seq:clj-seq)
			      (:conc-name pv-))
  (count nil :type fixnum :read-only t)
  (shift nil :type fixnum :read-only t)
  (root nil :type vector-node :read-only t)
  (tail nil :type (simple-array t) :read-only t)
  (meta nil))

(defstruct (transient-vector (:include seq:clj-seq)
			     (:conc-name tv-))
  (count nil :type fixnum)
  (shift nil :type fixnum)
  (root nil :type vector-node)
  (tail nil :type (simple-array t)))

;;; variables and constants

(defvar *no-edit* (make-atomic-reference :val nil))
(defvar *empty-node* (make-vector-node :edit *no-edit*))
(defvar *empty-vector* (make-persistent-vector :count 0 :shift 5 :root *empty-node* :tail (make-array 0)))

;;; generics

(defgeneric vec-conj (vector item))

;;; macros

(defmacro with-pv ((count shift root tail &optional meta) vec &body body)
  `(with-accessors ((,count pv-count)
		    (,shift pv-shift)
		    (,root pv-root)
		    (,tail pv-tail)
		    (,(or meta (gensym)) pv-meta))
      ,vec
     ,@body))

(defmacro with-tv ((count shift root tail) vec &body body)
  `(with-accessors ((,count tv-count)
		    (,shift tv-shift)
		    (,root tv-root)
		    (,tail tv-tail))
      ,vec
     ,@body))

;;; transient vector impl

(defun tv-ensure-editable (vec)
  (with-tv (count shift root tail) vec
    (unless (atomic-reference-get (vn-edit root))
      ;; TODO: better errors
      (error "Editting this vector is forbidden"))))

(defun tv-ensure-editable-node (vec node)
  (with-tv (count shift root tail) vec
    (if (eq (vn-edit root) (vn-edit node))
	node
	(make-vector-node :edit (vn-edit root)
			  :array (copy-simple-array (vn-array node))))))

(defun new-path (edit level node)
  (if (= level 0)
      node
      (let ((r (make-vector-node :edit edit)))
	(setf (aref (vn-array r) 0) (new-path edit (- level +chunk-bit+) node))
	r)))

(defun tv-push-tail (vec level parent tail-node)
  (with-tv (count shift root tail) vec
    (let* ((parent (tv-ensure-editable-node vec parent))
	   (subidx (logand +chunk-mask+ (ash (1- count) (- level)))))

      (setf (aref (vn-array parent) subidx)
	    (if (= level +chunk-bit+)
		tail-node
		(let ((child (aref (vn-array parent) subidx)))
		  (if child
		      (tv-push-tail vec (- level +chunk-bit+) child tail-node)
		      (new-path (vn-edit root) (- level +chunk-bit+) tail-node)))))
      parent)))

(defun tv-conj (vec val)
  (with-tv (count shift root tail) vec
    (tv-ensure-editable vec)
    (let ((i count))
      (if (< (- i (tv-tail-off vec)) +chunk-size+)
	  (progn
	    (setf (aref tail (logand i +chunk-mask+)) val)
	    (incf count))
	  ;;else 
	  (let ((new-tail (make-vector-node :edit (vn-edit root)
					    :array tail)))
	    (setf tail (make-array-chunk))
	    (setf (aref tail 0) val)

	    (if (> (ash count (- +chunk-bit+)) (ash 1 shift))
		(let ((new-root (make-vector-node :edit (vn-edit root))))
		  (setf (aref (vn-array new-root) 0) root)
		  (setf (aref (vn-array new-root) 1) (new-path (vn-edit root) shift new-tail))
		  (incf shift 5)
		  (setf root new-root))
		;;else
		(setf root (tv-push-tail vec shift root new-tail)))
	    (incf count)))))
  vec)

;;; persistent vector impl

(defun editable-root (node)
  (make-vector-node :edit (make-atomic-reference :val t)
		    :array (copy-simple-array (vn-array node))))

(defun editable-tail (array)
  (let ((ret (make-array-chunk)))
    (array-copy array 0 ret 0 (length array))
    ret))

(defun pv-as-transient (vec)
  (with-pv (count shift root tail) vec
    (make-transient-vector :count count :shift shift
			   :root (editable-root root)
			   :tail (editable-tail tail))))

(defun tail-off (count)
  (if (< count +chunk-size+)
      0
      (ash (ash (1- count) (- +chunk-bit+)) +chunk-bit+)))

(defun tv-tail-off (vec)
  (tail-off (tv-count vec)))

(defun tv-as-persistent (vec)
  (with-tv (count shift root tail) vec
    (tv-ensure-editable vec)
    (setf (atomic-reference-val (vn-edit root)) nil)
    (let* ((len (- count (tv-tail-off vec)))
	   (shorter-tail (make-array len)))
      (array-copy tail 0 shorter-tail 0 len)
      (make-persistent-vector :count count
			      :shift shift
			      :root root
			      :tail shorter-tail))))

(defun create-persistent-vector (&rest items)
  ;; TODO: Create vector from clj-seq
  (let ((len (length items)))
    (if (<= len +chunk-size+)
	(make-persistent-vector :count len :shift 5 :root *empty-node*
				:tail (make-array len :initial-contents items))
	(loop for ret = (pv-as-transient *empty-vector*)
		then (tv-conj ret item)
	      for item in items
	      finally (return (tv-as-persistent ret))))))

(defun pv-tail-off (vec)
  (tail-off (pv-count vec)))

(defun pv-array-for (vec i)
  (with-pv (count shift root tail) vec
    (if (and (>= i 0) (< i count))
	(if (>= i (pv-tail-off vec))
	    tail
	    (loop for node = root then (aref (vn-array node) (logand (ash i (- level)) +chunk-mask+))
		  for level = shift then (- level 5)
		  while (> level 0)
		  finally (return (vn-array node))))
	(error "index out of bounds"))))

(defun pv-nth (vec i)
  (aref (pv-array-for vec i) (logand i +chunk-mask+)))

(defun pv-nth-safe (vec i not-found)
  (if (and (>= i 0) (< i (pv-count vec)))
      (pv-nth vec i)
      not-found))

(defun pv-make-iterator (vec &optional (start 0) (end nil))
  (with-pv (count shift root tail) vec
    (let ((i start)
	  (base (- start (mod start +chunk-size+)))
	  (array (if (< start count) (pv-array-for vec start) nil))
	  (r-end (or end count)))
      (lambda ()
	(when (< i r-end)
	  (when (= (- i base) +chunk-size+)
	    (setf array (pv-array-for vec i))
	    (incf base +chunk-size+))
	  (let ((element (aref array (logand i +chunk-mask+))))
	    (incf i)
	    (values t element)))))))

(defmethod seq:make-iterator ((vec persistent-vector))
  (pv-make-iterator vec))

(defmethod print-object ((vec persistent-vector) stream)
  (write-char #\[ stream)
  (loop with itr = (pv-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr))
	  then (list next-remaining next-val)
	while remaining
	for (next-remaining next-val) = (multiple-value-list (funcall itr))
	do (prin1 val stream)
	   (when next-remaining (write-char #\  stream)))
  (write-char #\] stream))
