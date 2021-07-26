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
  (:use #:cl))

(defpackage #:clojure-cl.tests
  (:use #:cl #:clojure-cl #:fiveam #:clojure-cl.hash-table))
