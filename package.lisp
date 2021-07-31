;;;; package.lisp

(defpackage #:clojure-cl
  (:use #:cl)
  (:export
   ;; String manipulation
   :str
   ;; Sequences
   :clj-seq
   :seq
   :keys
   :vals
   ))

(defpackage #:clojure-cl.hash-table
  (:use #:cl)
  (:export
   :map-assoc
   :map-without
   :map-make-iterator
   :map-val-at
   :create-persistent-map
   ))

(defpackage #:clojure-cl.tests
  (:use #:cl #:clojure-cl #:fiveam #:clojure-cl.hash-table))
