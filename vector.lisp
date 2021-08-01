;;;; vector.lisp

(in-package :clojure-cl.vector)

(defstruct (vector-node (:conc-name nv-))
  (edit nil :type (or null atomic-reference) :read-only t)
  (array (make-array 32 :initial-element nil) :type (simple-array t) :read-only t))

(defstruct (persistent-vector (:conc-name pv-))
  (count nil :type fixnum :read-only t)
  (shift nil :type fixnum :read-only t)
  (root nil :type vector-node :read-only t)
  (tail nil :type (simple-array t) :read-only t)
  (meta nil))

()
