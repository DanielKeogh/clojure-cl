;;;; sequences.lisp

(in-package :clojure-cl.sequences)

(defstruct clj-seq)

(defgeneric make-iterator (seq))
