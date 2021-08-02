;;;; package.lisp

(defpackage #:clojure-cl.persistence-utils
  (:use #:cl)
  (:export
   :atomic-reference
   :atomic-reference-val
   :make-atomic-reference
   :atomic-reference-get
   :copy-simple-array
   :array-copy))

(defpackage #:clojure-cl.hash-table
  (:use #:cl #:clojure-cl.persistence-utils)
  (:export
   :map-assoc
   :map-without
   :map-make-iterator
   :map-val-at
   :create-persistent-map))

(defpackage #:clojure-cl.vector
  (:use #:cl #:clojure-cl.persistence-utils))

(defpackage #:clojure-cl.reader-macros
  (:use #:cl)
  (:export
   :enable-clojure-reader-macros
   :disable-clojure-reader-macros))

(defpackage #:clojure-cl
  (:use #:cl)
  (:export
   ;; String manipulation
   :str
   ;; Sequences
   :clj-seq
   :seq
   :keys
   :vals))

(defpackage #:clojure-cl.tests
  (:use #:cl #:clojure-cl #:fiveam #:clojure-cl.hash-table))
