;;;; clojure-cl.asd

(asdf:defsystem #:clojure-cl
  :description "Clojure annoys Common Lisp programmers"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
	       (:file "hash-table-utils")
	       (:file "hash-table")
               (:file "clojure-cl")))
