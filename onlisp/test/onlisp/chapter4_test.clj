
(ns onlisp.chapter4-test
  (:use clojure.test)
  (:use onlisp.chapter4))

(deftest test-longer
  (is (longer '(a b c) '(x y)))
  (is (not (longer '(a b c) '(x y z))))
  (is (not (longer '(a b) '(x y z))))
  (is (not (longer '(x) '(y))))
  (is (not (longer '() '()))))

(deftest test-flatten2
  (is (flatten2 '((1 2 3) (4 5))) '(1 2 3 4 5))
  (is (flatten2 '()) '())
  (is (flatten2 '(1 2)) '(1 2))
  (is (flatten2 '(9 (2 2) (3 4 (5 6)) 8)) '(9 2 2 3 4 5 6 8)))

(run-tests)
