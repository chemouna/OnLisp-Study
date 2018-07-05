(ns onlisp.chapter5)

(defn joiner
  [obj]
  (number? obj) (partial + obj)
  (sequential? obj) (partial conj obj))

(defn make-adder
  [n]
  (partial + n))

(defn lrec
  [trans base]
  (fn
    [lst]
    (loop [lst lst result base]
      (if (seq lst)
        (recur (rest lst) (trans (first lst) result))
        result))))

((lrec (fn [new acc] (inc acc)) 0) [2 4 6])
