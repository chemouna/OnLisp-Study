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
                    (< m (score (first coll))) (helper score (rest coll) (first coll) (score (first coll)))
                    :else (helper score (rest coll) winner m)))]
          (helper f (rest lst) (first lst) max)))))

(defn most2
  [f lst]
  "Another way to implement most function above using list comprehensions"
  (let [wins (ref (first lst))
        mx (ref (f @wins))]
    (doall
     (for [x (rest lst) :when (> (f x) @mx)]
       [(dosync (ref-set wins x) (ref-set mx (f x)))]))
    (list @wins @mx)))

(defn best
  "Takes a function and a list where the function must be a predicate of two
  arguments and returns the element that beats all others according to the predicate"
  [pred lst]
  (if (empty? lst) nil
      (let [best (first lst)]
        (letfn [(helper [f coll best]
                  (cond
                    (empty? coll) best
                    (f (first coll) best) (helper f (rest coll) (first coll))
                    :else (helper f (rest coll) best)))]
          (helper pred (rest lst) best)))))

(defn best2
  "Takes a function and a list where the function must be a predicate of two
  arguments and returns the element that beats all others according to the predicate
  (this implementation uses lisp comprehensions)"
  [f lst]
  (let [wins (ref (first lst))]
    (last
     (for [x (rest lst) :when (f x @wins)]
       (dosync (ref-set wins x))))))

(defn best3
  "Does the same as the two best implementations above but a lot more succint"
  [f xs]
  (reduce #(if (f %1 %2) %1 %2) xs))

(defn most3
  "Does the same as the two most implementations above but a lot more succint"
  [f xs]
  (reduce #(if (< (f %1) (f %2)) (list %2 (f %2)) (list %1 (f %1))) xs))

(defn mostn
  [f lst]
  (let [winners (ref (list (first lst)))
        mx (ref (f (first lst)))]
    (doall
     (for [x (rest lst) :when (>= (f x) @mx)]
       [(dosync
         (cond
           (= (f x) @mx) (ref-set winners (cons x @winners))
           :else (and (ref-set winners (list x)) (ref-set mx (f x)))))]))
    (list @winners @mx)))

(defn mapa-b
  ([f a b] (mapa-b f a b 1))
  ([f a b step]
   (letfn [(rec
            [s results]
            (cond
              (> s b) (cons (+ s step) results)
              :else (cons (+ s step) (rec (+ s step) results))))]
    (conj (rec (f a) '()) (f a)))))

(defn map0-n
  [f n]
  (mapa-b f 0 n))

(defn map1-n
  [f n]
  (mapa-b f 1 n))

(defn map->
  [f start test-fn succ-fn] ;; test-fn is kind of a stop function
  (letfn [(rec
            [s results]
            (cond
              (test-fn s) results
              :else (cons s (rec (succ-fn s) results))))]
     (rec (f start) '()) ))

(defn map2-> [f start test-fn succ-fn]
  (letfn [(go [i]
              (lazy-seq (when (not (test-fn i))
                          (cons (f i) (go (succ-fn i))))))]
    (go start)))

(def mappend mapcat)

(defn mapcars
  [f & lsts]
  (letfn [(rec
            [colls results]
            (cond
              (empty? colls) results
              :else (recur (rest colls)
                         (concat results (map f (first colls))))))]
    (rec lsts '())))

(def atom?
  (complement coll?))

(defn rmapcar
  [f & args]
  (if (some atom? args)
    (apply f args)
    (apply map #(apply rmapcar f %&) args)))

(defn readlist
  [& args]
  (read-string
   (str "(" (apply read-line args) ")")))

(def mkstr str)

(defn mkstr2
  [& args]
  (with-out-str
    (doseq [v args]
      (print v))))

(defn symb
  [& args]
  (symbol (apply mkstr args)))

(defn reread
  [& args]
  (read-string (mkstr args)))

(defn explode
  [sym]
  (map #(symbol (str %)) (seq (str sym))))
