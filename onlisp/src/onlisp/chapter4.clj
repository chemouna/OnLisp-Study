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

(defn zip-util [root]
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

(defn member
  [x sq test]
  (if (sequential? sq)
    (if (test x (first sq))
      sq
      (recur x (rest sq) test))))

(defn before?
  "Checks if a value that returns true when test is called on it with x is found before another value y in the list, if it is found returns the rest of the list starting from x"
  [x y sq test]
  (and sq
       (let [elem (first sq)]
         (if (= y elem)
           nil
           (if (test x elem)
             sq
             (recur x y (rest sq) test))))))

(defn after?
  "Checks if a value that returns true when test is called on it with x is found after another value y in the list, if it is found returns the rest of the list starting from x"
  [x y lst test]
  (let [rst (before? y x lst test)]
    (and rst (member x rst test))))

(defn duplicate?
  [obj lst test]
  (member obj (rest (member obj lst test)) test))

(defn split-if
  "Returns both halves the first half contains elements that fail the predicate f and the second half succeeds for the predicate"
  [pred lst]
  (cons (for [x lst :when (not (pred x))] x)
        (cons (for [x lst :when (pred x)] x) '())))

(defn most
  "Takes a list and a scoring function and returns the element with the highest score, If there's a tie it returns the element occuring first."
  [f lst]
  (if (empty? lst) (list nil nil)
      (let [max (f (first lst))]
        (letfn [(helper [score coll winner m]
                  (cond
                    (empty? coll) (list winner m)
                    (< m (score (first coll))) (helper score (rest coll) (first coll) (score (first coll))) ;; todo put score (first coll) in a var
                    :else (helper score (rest coll) winner m)))]
          (helper f (rest lst) (first lst) max)))))
