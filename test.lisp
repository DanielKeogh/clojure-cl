;;;; test.lisp

(in-package :clojure-cl.tests)
(in-suite* :clojure-cl.tests)

(def-suite test-suite :description "Tests for clojure rip-off in Common Lisp")

(def-test is-str (:suite test-suite)
  (is (equal "happy" (str "happy")))
  (is (equal "" (str)))
  (is (equal "" (str nil)))
  (is (equal "" (str nil nil nil)))
  (is (equal "123" (str nil 123 nil))))

(def-test is-seq (:suite test-suite)
  (is (typep (seq "123") clj-seq))
  (is (typep (seq (vector 1 2 3)) clj-seq))
  (is (typep (seq (list 1 2 3)) clj-seq))
  (is (typep (seq (seq (list 1 2 3))) clj-seq))
  (is (null (seq nil)))
  (is (null (seq (vector))))
  (is (null (seq ())))
  (is (null (seq ())))
  (is (null (seq ""))))

(def-test keys (:suite test-suite)
  (is (null (keys (make-hash-table)))))

(deftest vals (:suite test-suite)
  (is (null (vals {}))))

