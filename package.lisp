;;;; package.lisp

(defpackage #:clojure-cl.sequences
  (:use #:cl)
  (:export
   :clj-seq
   :make-iterator))

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
   ;; types
   :chash-map
   :persistent-hash-map
   :transient-hash-map
   ;; methods
   :map-count
   :map-assoc
   :map-without
   :map-make-iterator
   :map-val-at
   :create-persistent-map)
  (:local-nicknames (#:seq #:clojure-cl.sequences)))

(defpackage #:clojure-cl.vector
  (:use #:cl #:clojure-cl.persistence-utils)
  (:export
   :create-persistent-vector
   :persistent-vector
   :pv-make-iterator
   :pv-count)
  (:local-nicknames (#:seq #:clojure-cl.sequences)))

(defpackage #:clojure-cl.reader-macros
  (:use #:cl)
  (:export
   :enable-clojure-reader-macros
   :disable-clojure-reader-macros))

(defpackage #:clojure-cl
  (:use #:cl #:clojure-cl.sequences)
  (:local-nicknames (#:vec #:clojure-cl.vector)
		    (#:hash #:clojure-cl.hash-table)
		    (#:reader-macros #:clojure-cl.reader-macros))
  (:export
   ;; String manipulation
   :str
   ;; Sequences
   :clj-seq
   :seq
   :keys
   :vals
   :range
   ;; Vectors
   :vec))

(defpackage #:clojure-cl.tests
  (:use #:cl #:clojure-cl #:fiveam #:clojure-cl.hash-table))
