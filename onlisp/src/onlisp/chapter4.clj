(ns onlisp.chapter4
  (:use [clojure.tools.trace :as trace]))

(trace/trace-ns 'onlisp.chapter4)

(defn find2
  [f lst]
  (when lst
    (let [val (f (first lst))]
      (if val
        [(first lst) val]
        (find2 f (rest lst))))))

;(find2 even? '(1 2 3))

(defn last1
  [s]
  (last s))

(defn single
  [s]
  (and (seq? s) (not (next seq))))

(defn append1
  [lst obj]
  (concat lst (list obj)))

(defn mklist [obj] (if (sequential? obj) obj (list obj)))

(defn conc1 [sq elem]
  (dosync
   (if (seq? elem)
     (ref-set sq (cons @sq elem))
     (ref-set sq (concat @sq (vector elem))))))

(defn longer
  [x y]
  (if (and (sequential? x) (sequential? y))
    (loop [x (next x) y (next y)]
      (and x (or (not y) (recur (next x) (next y)))))
    (> (count x) (count y))))

(defn filtr [f lst]
  (for [x lst :when (f x)]
    x))

;(filtr even? '(2 3 4 5 6 7 8 9))

(defn group [source n]
  (if (zero? n) (pr "error: zero length"))

  (let [t (int (/ (count source) n))
        remainder (drop (* n t) source)]
    (lazy-cat
     (for [i (range t)]
       (take n (nthrest source (* i n))))
     (if (nil? remainder)
       '()
       (list remainder)))))

(group '(1 2 3 4 5 6 7) 4)
