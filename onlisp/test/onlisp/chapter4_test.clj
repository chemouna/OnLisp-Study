
(ns onlisp.chapter4-test
  (:require [clojure.test :refer :all]
            [onlisp.chapter4 :refer :all]))

(deftest test-longer?
  (is (longer '(a b c) '(x y)))
  (is (not (longer '(a b c) '(x y z))))
  (is (not (longer '(a b) '(x y z))))
  (is (not (longer '(x) '(y))))
  (is (not (longer '() '()))))

