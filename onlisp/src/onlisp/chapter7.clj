
(ns onlisp.chapter7)

((use '[clojure.contrib.fcase :only (case)])

(defn nil!
  [at]
  (swap! at (fn [_] nil)))

(def a (atom 10))

(nil! a)

(print @a)

 (defmacro nif
   [expr pos zero neg]
   `(case (Integer/signum ~expr)
      -1 ~neg
      0 ~zero
      1 ~pos))
