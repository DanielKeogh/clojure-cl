;;;; test.lisp

(in-package :clojure-cl.tests)
(in-suite* :clojure-cl.tests)

(clojure-cl.reader-macros:enable-clojure-reader-macros)

(def-suite test-suite :description "Tests for clojure rip-off in Common Lisp")

(def-test is-str (:suite test-suite)
  (is (equal "happy" (str "happy")))
  (is (equal "" (str)))
  (is (equal "" (str nil)))
  (is (equal "" (str nil nil nil)))
  (is (equal "123" (str nil 123 nil))))

(def-test is-seq (:suite test-suite)
  (is (typep (seq "123") 'clj-seq))
  (is (typep (seq (vector 1 2 3)) 'clj-seq))
  (is (typep (seq (list 1 2 3)) 'clj-seq))
  (is (typep (seq [1 2 3]) 'clj-seq))
  (is (typep (seq {1 2 3 4}) 'clj-seq)))

(def-test is-seq-nil (:suite test-suite)
  (is (null (seq nil)))
  (is (null (seq (vector))))
  (is (null (seq (vec))))
  (is (null (seq ())))
  (is (null (seq [])))
  (is (null (seq "")))
  (is (null (seq {}))))

(def-test keys (:suite test-suite)
  (is (null (keys (make-hash-table)))))

(def-test vals (:suite test-suite)
  (is (null (vals {}))))

(clojure-cl.reader-macros:disable-clojure-reader-macros)
