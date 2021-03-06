
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

(comment "
(deftest test-prune
  (is (prune even? '()) '())
  (is (prune even? '(1 2 (3 (4 5) 6) 7 8 (9))) (1 (3 (5)) 7 (9))))
")

(deftest test-before?
  (is (before? 3 4 '(1 2 3 4) =) '(3 4))
  (is (before? 'b 'c '(1 2 a b c) =) '(b c))
  (is (before? 1 'c '(1 2 a b c) =) '(1 2 a b c)) 
  (is (before? 'a 1 '(1 2 a b c) =) nil))

(deftest test-after?
  (is (after? 3 2 '(1 2 3 4 5) =) '(3 4 5)))

(deftest test-split-if
  (is (split-if #(> % 4) '(1 2 3 4 5 6 7 8 9 10)) '((1 2 3 4) (5 6 7 8 9 10))))

(deftest test-most
  (is (most count '((a b) (a b c) (a) (e f g))) '((a b c) 3))
  (is (most count '((a b) (c d e) (g h l m))) '((g h l m) 4)))

(deftest test-best
  (is (best > '(1 2 3 4 5)) 5)
  (is (best < '(8 2 3 9 6 12)) 2))

(deftest test-mostn
  (is (mostn count '((a b) (a b c) (a) (e f g))) '(((a b c) (e f g)) 3)))

(deftest test-mapa-b
  (is (mapa-b #(+ 1 %) -2 0 0.5) '(-1 -0.5 0.0 0.5 1.0)))

(deftest test-map->
  (is (map-> identity 5 #(> % 10) inc) '(5 6 7 8 9 10)))

(deftest test-mapcars
  (is (mapcars #(Math/sqrt %) '(4 9 25) '(49 64)) '(2.0 3.0 5.0 7.0 8.0)))

(deftest test-rmapcar
  (is (rmapcar + '(1 (2 (3) 4)) '(10 (20 (30) 40))) '(11 (22 (33) 44))))

(deftest test-explode
  (is (explode "BOMB") '(B O M B)))

(run-tests)
