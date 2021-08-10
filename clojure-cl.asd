;;;; clojure-cl.asd

(asdf:defsystem #:clojure-cl
  :description "Clojure annoys Common Lisp programmers"
  :license "Eclipse"
  :version "0.0.1"
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
	       (:file "sequences")
	       (:file "persistence-utils")
	       (:file "hash-table-utils")
	       (:file "hash-table")
	       (:file "vector")
	       (:file "namespace")
	       (:file "reader-macros")
               (:file "clojure-cl")))
