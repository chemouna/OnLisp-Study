(ns onlisp.chapter4
  (:use [clojure.tools.trace :as trace]
        [clojure.zip :as zip]))

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

(defn flatten2
  [v]
  (letfn
      [(rec [x acc]
         (cond
           (not (list? x)) (cons x acc)
           :else (concat acc (apply concat (map #(rec % acc) x)))))]
    (rec v '())))

(Defn zip-util [root]
  (if (seq? root)
    (zip/seq-zip root)
    (zip/vector-zip root)))

(defn prune [f tree]
  (loop [loc (zip-util tree)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
       (zip/next
        (if (f (zip/node loc))
          (zip/remove loc)
          loc))))))

(defn before?
  "Checks in a value x is found before another value y in the list"
  [x y lst test] ;; todo: find a way to make test defaults to equal
  (and lst
       (let [fst (first lst)]
         (cond
           (test y fst) nil
           (test x fst) lst
           :else (recur x y (rest lst) test)))))
