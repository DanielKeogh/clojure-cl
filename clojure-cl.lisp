;;;; clojure-cl.lisp

(in-package #:clojure-cl)

(defun clj-destructure (vector)
  (let* ((args (cdr (substitute '& '&body vector))))
    (values args (lambda (x) x))))

(defmacro defn (name vector &body body)
  (unless (and (listp vector)
	       (eql (car vector) 'vector))
    (error "defn should have the form (defn name [args] @body)"))
  (multiple-value-bind (args let-wrapper)
      (clj-destructure vector)
    `(defun ,name ,args
       ,@(funcall let-wrapper body))))

;; String
(defun str (&rest y)
  (format nil "~{~a~}"
	  (mapcar (lambda (x) (if x x "")) y)))

(defstruct clj-seq)

(defgeneric make-iterator (clj-seq))

(defmethod make-iterator (object)
  nil)

(defstruct (vector-clj-seq (:include clj-seq))
  (vector nil :type vector :read-only t))

(defmethod make-iterator ((vec vector))
  (let* ((index -1)
	 (len (length vec)))
    (lambda ()
      (incf index)
      (if (>= index len)
	  (values nil nil)
	  (values t (aref vec index))))))

(defmethod make-iterator ((s vector-clj-seq))
  (make-iterator (vector-clj-seq-vector s)))

(defstruct (list-clj-seq (:include clj-seq))
  (list nil :type list :read-only t))

(defmethod make-iterator ((lst list))
  (let ((curr lst))
    (lambda ()
      (if curr
	  (let ((r (car curr)))
	    (setf curr (cdr curr))
	    (values t r))
	  (values nil nil)))))

(defmethod make-iterator ((n null))
  n)

(defmethod make-iterator ((lst list-clj-seq))
  (make-iterator (list-clj-seq-list lst)))

(defstruct (multi-clj-seq (:include clj-seq))
  (sequences nil :type (vector clj-seq) :read-only t))

(defmethod make-iterator ((lst multi-clj-seq))
  (let ((seqs (make-iterator (multi-clj-seq-sequences lst)))
	curr)
    (labels ((set-curr ()
	       (multiple-value-bind (remaining seq)
		   (funcall seqs)
		 (when remaining (setf curr (make-iterator seq)) t))))
      (lambda ()
	(loop named l while t
	      when (not curr)
		do (unless (set-curr)
		     (return-from l (values nil nil)))
	      end
	      when curr
		do (multiple-value-bind (remaining itr)
		       (funcall curr)
		     (if remaining
			 (return-from l (values remaining itr))
			 (setf curr nil))))))))

(defstruct (string-clj-seq (:include clj-seq))
  (string nil :type string :read-only t))

(defmethod make-iterator ((str string-clj-seq))
  (make-iterator (string-clj-seq-string str)))

(defstruct (closure-clj-seq (:include clj-seq))
  (generate-iterator nil :type function :read-only t))

(defmethod make-iterator ((fn closure-clj-seq))
  (funcall (closure-clj-seq-generate-iterator fn)))

(defstruct (lazy-cat-seq (:include clj-seq))
  (expressions nil :type list :read-only t))

(defmethod make-iterator ((lazy lazy-cat-seq))
  (let ((exprs (lazy-cat-seq-expressions lazy)))
    (lambda ()
      (if exprs
	  (progn
	    (let ((current (car exprs)))
	      (setf exprs (cdr exprs))
	      (values t (eval current))))
	  (values nil nil)))))

(defstruct (range-clj-seq (:include clj-seq))
  (start 0 :type integer :read-only t)
  (end nil :read-only t)
  (step 0 :type integer :read-only t))

(defmethod make-iterator ((range range-clj-seq))
  (let ((ctr (range-clj-seq-start range))
	(end (range-clj-seq-end range))
	(incr (range-clj-seq-step range)))
    (labels ((next () (let ((r ctr))
			(incf ctr incr)
			(values t r))))
      (cond ((or (= incr 0) (null end)) (next))
	    ((> incr 0) (lambda () (if (< ctr end)
				       (next)
				       (values nil nil))))
	    (t (lambda () (if (> ctr end)
			      (next)
			      (values nil nil))))))))

(defstruct (repeat-clj-seq (:include clj-seq))
  (element nil)
  (count nil :type (or null integer)))

(defmethod make-iterator ((repeat repeat-clj-seq))
  (let ((ctr (repeat-clj-seq-count repeat))
	(element (repeat-clj-seq-element repeat)))
    (if ctr
	(lambda ()
	  (if (> ctr 0)
	      (progn (decf ctr)
		     (values t element))
	      (values nil nil)))
	(lambda () (values t element)))))

(defun seq (collection)
  (etypecase collection
    (string (when (> (length collection) 0) (make-string-clj-seq :string collection)))
    (list (when collection (make-list-clj-seq :list collection)))
    (vector (when (> (length collection) 0)) (make-vector-clj-seq :vector collection))
    (clj-seq collection)
    (null nil)))

(defun vals (map)
  (etypecase map
    (hash-table (when (> (hash-table-count map) 0)
		  (seq (alexandria:hash-table-values map)))) ;; Not lazy. Yukky.
    (null nil)))

(defun keys (map)
  (etypecase map
    (hash-table (when (> (hash-table-count map) 0)
		  (seq (alexandria:hash-table-keys map))))
    (null nil)))

;; Tests

(defun seq? (x)
  (clj-seq-p x))

;; Iterators

(defmacro do-seq (seq-exprs &body body)
  ;; TODO: Complex destructuring
  (let* ((lst (cdr seq-exprs))
	 (vars (loop for (var seq) on lst by #'cddr
		     collect (list var seq (gensym) (gensym)))))
    `(loop ,@(loop for (var itr seq remaining) in vars
		   append `(with ,seq = (make-iterator ,itr)))
	   ,@(loop for (var itr seq remaining) in vars
		   append `(for (,remaining ,var) = (multiple-value-list (funcall ,seq))))
	   while (and ,@(loop for (var itr sym remaining) in vars collect remaining))
	   do ,@body)))

(defmacro clj-dotimes (bindings &body body)
  (let ((lst (cdr bindings)))
    (when (/= (length lst) 2) (error "Bindings should be in the form [x <fixnum>]"))
    `(dotimes ,lst
       ,@body)))

(defmacro while (test &body body)
  `(loop while ,test
	 do (progn ,@body)))

(defmacro for ()
  (error "TODO: Not implemented"))

(defun clj-map (f &rest cols)
  (make-closure-clj-seq
   :generate-iterator
   (lambda ()
     (let ((itrs (mapcar #'make-iterator cols)))
       (lambda ()
	 (loop for itr in itrs
	       for (remaining val) = (multiple-value-list (funcall itr))
	       unless remaining do (return (values remaining nil))
		 end
	       collect val into vals
	       finally (return (values t (apply f vals)))))))))

(defun range (&optional arg1 arg2 arg3)
  ;; TODO: Better error checking
  (cond ((and arg1 arg2 arg3) (make-range-clj-seq :start arg1 :end arg2 :step arg3))
	((and arg1 arg2) (make-range-clj-seq :start arg1 :end arg2 :step 1))
	(arg1 (make-range-clj-seq :start 0 :end arg1 :step 1))
	(t (make-range-clj-seq :start 0 :end nil :step 1))))

(defun repeat (arg1 &optional (arg2 'clj-repeat-overload-hack))
  (if (eq 'clj-repeat-overload-hack arg2)
      (make-repeat-clj-seq :element arg1)
      (make-repeat-clj-seq :element arg2 :count arg1)))

;; List extenders

(defun clj-count (s)
  (etypecase s
    (null 0)
    (list (length s))
    (vector (length s))
    (clj-seq
     (loop with itr = (make-iterator s)
	   while (funcall itr) count 1))))

(defun vec (&rest args)
  (let ((len (length args)))
    (make-array len :initial-contents args)))

(defun concat (&rest seqs)
  (let ((len (length seqs))) ;; TODO: not O(n)
    (cond ((= 0 len) nil)
	  ((= 1 len) (car seqs))
	  (t (make-multi-clj-seq :sequences (apply #'vector seqs))))))

(defun conj (col1 &rest rest)
  ;; TODO map support.
  (let ((s (seq col1)))
    (if s
	(concat s (seq rest))
	(seq rest))))

(defun clj-cons (x sequence)
  (concat (seq (list x)) (seq sequence)))

(defmacro lazy-cat (&rest cols)
  ;; TODO: Evaluate only once?
  `(make-lazy-cat-seq
    :expressions ',cols))

(defun mapcat (f &rest cols)
  (let ((r (apply #'clj-map (cons f cols))))
    (apply #'concat (clj-list r))))

;; Predicates

(defun empty? (col)
  ;; TODO: Is this really empty?
  (not col))

;; Transform a sequence

(defun clj-list (coll)
  (let (r)
    (do-seq [x coll]
      (push x r))
    (nreverse r)))

(defun clj-reverse (coll)
  ;; Not lazy as per specification
  ;; TODO: For sets where we know the length, this could use a vector
  (let ((r nil))
    (do-seq [x coll]
      (push x r))
    r))

(defun clj-sort (arg1 &optional arg2)
  (labels ((sort% (comparer coll)
	     (let ((r (clj-list coll)))
	       (sort r comparer))))
    (cond ((and (seq arg1) (null arg2))
	   (sort% #'< arg1))
	  ((functionp arg1) (sort arg1 arg2)) ;; TODO: Better errors
	  (t (error "Bad arguments for clj-sort")))))

;; Remove items from sequences

(defun filter% (gen-predicate coll)
  (make-closure-clj-seq
   :generate-iterator
   (lambda ()
     (let ((itr (make-iterator coll))
	   (pred (funcall gen-predicate)))
       (lambda ()
	 (loop named l
	       for (remaining val) = (multiple-value-list (funcall itr))
	       do (if remaining
		      (when (funcall pred val) (return-from l (values t val)))
		      (return-from l (values nil nil)))))))))

(defun filter (pred coll)
  (filter% (lambda () pred) coll))

(defun clj-remove (pred coll)
  (filter% (lambda () (lambda (x) (not (funcall pred x)))) coll))

(defun distinct (coll)
  (filter% (lambda ()
	     (let ((set (make-hash-table :test 'equal)))
	       (lambda (x)
		 (unless (gethash x set)
		   (setf (gethash x set) t)
		   t)
		 )))
	   coll))

(defun flatten (x)
  (labels ((flatten% (root-itr)
	     (let (current-itr)
	       (lambda ()
		 (loop named outer while t do 
		   (if current-itr
		       (multiple-value-bind (rem val)
			   (funcall current-itr)
			 (if rem
			     (return-from outer (values rem val))
			     (setf current-itr nil)))
		       (multiple-value-bind (rem val)
			   (funcall root-itr)
			 (if (null rem)
			     (return-from outer (values rem val))
			     (let ((new-itr (make-iterator val)))
			       (if new-itr 
				   (setf current-itr (flatten% new-itr))
				   (return-from outer (values rem val))))))))))))
    (make-closure-clj-seq
     :generate-iterator
     (lambda () (flatten% (make-iterator x))))))

(defun dedupe (coll)
  (filter% (lambda ()
	     (let ((last (gensym)))
	       (lambda (x)
		 (unless (equal x last)
		   (setf last x)
		   t))))
	   coll))

(defun random-sample (prob coll)
  (assert (>= prob 0))
  (assert (<= prob 1))
  (filter% (lambda () (lambda (x) (declare (ignore x))
			(> (random 1.0) prob)))
	   coll))

(defun repeat-first% (coll)
  (make-closure-clj-seq
   :generate-iterator
   (lambda ()
     (let* (initialized remaining val)
       (lambda ()
	 (unless initialized
	   (multiple-value-bind (r v)
	       (funcall (make-iterator coll))
	     (setf remaining r
		   val v)))
	 (values remaining val))))))

(defun take-nth (n coll)
  (if (> n 0)
      (filter% (lambda ()
		 (let ((cnt 0))
		   (lambda (x)
		     (declare (ignore x))
		     (if (= cnt 0)
			 (progn (setf cnt n)
				t)
			 (progn (decf cnt))))))
	       coll)
      (repeat-first% coll)))

(defun take (n coll)
  (make-closure-clj-seq
   :generate-iterator
   (lambda ()
     (let ((cnt 0)
	   (itr (make-iterator coll)))
       (lambda ()
	 (if (< cnt n)
	     (progn (incf cnt)
		    (funcall itr))
	     (values nil nil)))))))

(defun clj-subseq (sc start-test start-key &optional end-test end-key)
  ;; TODO: Introduce concept of sorted set
  ;; TODO: Test that sc is a sorted collection
  ;; TODO: Make end-test/end-key more clojure compliant
  (make-closure-clj-seq
   :generate-iterator
   (lambda ()
     (let ((itr (make-iterator sc))
	   found-start found-end)
       (labels ((return-unless-end (remaining item)
		  (if (and remaining
			   (or (not end-test)
			       (funcall end-test item end-key)))
		      (values remaining item)
		      (progn
			(setf found-end t)
			(values nil nil)))))
	 (lambda ()
	   (cond (found-end (values nil nil))
		 ((null found-start)
		  (loop for (remaining item) = (multiple-value-list (funcall itr))
			when (or (not remaining) (funcall start-test item start-key))
			  do (return (progn (setf found-start t)
				       (return-unless-end remaining item)))))
		 (t (multiple-value-bind (remaining item)
			(funcall itr)
		     (return-unless-end remaining item))))))))))

(defun clj-rsubseq (sc start-test start-key &optional end-test end-key)
  (clj-subseq (clj-reverse sc) start-test start-key end-test end-key))

  ;; Pretty print
(defmethod print-object ((seq clj-seq) stream)
  (write-char #\( stream)
  (let (after-first)
    (do-seq [x seq]
      (when after-first
	(write-char #\  stream))
      (typecase x
	(character (write-char #\\ stream)
	 (write-char x stream))
	(t (princ x stream)))
      
      (setf after-first t)))
  (write-char #\) stream))

