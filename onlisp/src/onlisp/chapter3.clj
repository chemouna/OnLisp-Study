(ns onlisp.chapter3)

(defn good-reverse [lst]
  (loop [lst lst acc ()]
    (if (= () lst)
      acc
      (recur (rest lst) (cons (first lst) acc )))))

;(good-reverse [1 2 3 4])

; imperative
(defn imp [x]
  (let [y (first x) sqr (expt y 2)]
    (list 'a sqr)))

;; mutable vs immutable

;; x is immutable
(let [x 0]
  (defn total [y]
    (+ x y)))

;; x is mutable
(let [x (atom 0)]
  (defn total [y]
    (swap! x #(+ % y))))
