;;;; package.lisp

(defpackage #:clojure-cl
  (:use #:cl))

(defpackage #:clojure-cl.tests
  (:use #:cl #:clojure-cl #:fiveam)
  (:export
   ;; String manipulation
   :str
   ;; Sequences
   :clj-seq
   :seq
   :keys
   :vals
   ))
