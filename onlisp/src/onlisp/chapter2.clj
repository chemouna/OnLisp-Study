(ns onlisp.chapter2)

(defn double [x] (* x 2))

(= double (first (list double)))

(fn [x] (* x 2))

(apply + [1 2])                   ; (apply #'+ '(1 2))
(apply @(resolve '+) [1 2])       ; (apply (symbol-function '+) '(1 2))
(apply (fn [x y] (+ x y)) [1 2])  ; (apply #'(lambda (x y) (+ x y)) '(1 2))


(map #(+ % 10) [1 2 3])         ; (mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
(map + [1 2 3] [10 100 1000])   ; (mapcar #'+ '(1 2 3) '(10 100 1000))


(sort [1 4 2 5 6 7 3])           ; (sort '(1 4 2 5 6 7 3) #'&lt;)

(filter odd? [1 2 3 4 5 6 7])    ; (remove-if #'evenp '(1 2 3 4 5 6 7))

(defn our-remove-if [f coll]                               ; (defun our-remove-if (fn lst)
  (if (empty? coll)                                        ;   (if (null lst)
    nil                                                    ;     nil
    (if (f (first coll))                                   ;     (if (funcall fn (car lst))
      (our-remove-if f (rest coll))                        ;         (our-remove-if fn (cdr lst))
      (cons (first coll) (our-remove-if f (rest coll)))))) ;         (cons (car lst) (our-remove-if fn (cdr lst))))))

(our-remove-if even? [1 2 3 4 5 6 7])


;; Section 2.4

;; conditional version
(defn behave [animal]
  (cond
    (= animal 'dog) (do '(wag-tail) '(bark))
    (= animal 'rat) (do '(scurry) '(squeek))
    (= animal 'cat) (do '(rub-legs) '(scratch-carpet))))

;; Protocols. Define the protocol
(defprotocol animal
  (behave [this] ))

;; define a dog
(defrecord dog [breed]  )

;; add the animal protocol to dog type
(extend dog
  animal
  {:behave (fn [src] (do '(wag-tail) '(bark)))})

;; create a dog
(def my-dog (dog. "collie"))

(behave my-dog)

;; define a rat
(deftype rat [color])

;; add the animal protocol to the rat type
(extend rat
  animal
  {:behave (fn [src] (do '(scurry) '(squeek)))})

;; create a rat
(def brown-rat (rat. "brown") )

(behave brown-rat)

(extend String
  animal
  {:behave (fn [src] (do '(what)))})

(behave "huh")


;; Section 2.5

(let [y 7]
  (defn scope-test [x]
    (list x y)))

(let [y 5]
  (scope-test 3))

;; Section 2.6
(defn list+ [lst n]
  (map (fn [x] (+ x n))
       lst))

(list+ '(1 2 3) 10)

;; lexically scoped counter
(let [counter (atom 0)]
  (defn new-id [] (swap! counter + 1))
  (defn reset-id [] (reset! counter 0 )))

(new-id)
(reset-id)

;; group of closures sharing the same data
(defn make-dbms [db]
  (list
   (fn [key]
     (db key))
   (fn [key val]
     (assoc db key val))
   (fn [key]
     (dissoc db key))))

(def cities (make-dbms {'boston 'us, 'paris 'france}) )

((first cities) 'boston)
((second cities) 'london 'england)
((last cities) 'boston)

;; using a mutable function
(defn make-mutable-dbms [db]
  (let [mdb (atom db)]
    (list
     (fn [key]
       (@mdb key))
     (fn [key val]
       (swap! mdb assoc key val))
     (fn [key]
       (swap! mdb dissoc key)))))

(def citiesx (make-mutable-dbms
              {'boston 'us, 'paris 'france}))

((first citiesx) 'boston)
((second citiesx) 'london 'england)
((last citiesx) 'boston)


;; using map to store closures instead of a list
(defn make-dbms-map [db]
  (let [mdb (atom db)]
    { :select (fn [key] (@mdb key))
      :insert (fn [key val]
        (swap! mdb assoc key val))
      :delete (fn [key]
        (swap! mdb dissoc key))}))

(def citiesm (make-dbms-map {'boston 'us 'paris 'france}))
((:select citiesm) 'boston)
((:insert citiesm) 'london 'england)
((:delete citiesm) 'boston)

;; local functions
(let [my-inc (fn [x] (+ x 1))] (my-inc 3))
(let [x 10 y x] y)

(defn count-instances [obj lists]
  (map (fn instances-in [lst]
         (if (seq lst)
           (+ (if (= (first lst) obj) 1 0)
              (instances-in (rest lst))) 0)) lists))

(count-instances 2 [[1 2 3] [1 2] [1]])

;; tail recursion
(defn our-find-if [pred lst]
  (if (pred (first lst))
    (first lst) (recur pred (rest lst))))

(our-find-if even? [1 2 3 4])

;; tail call version of our-length

(defn our-length [lst]
  (loop [lst lst acc 0]
    (if (empty? lst)
      acc
      (recur (rest lst) (+ acc 1) ))))

(our-length '(1 2 3 4))

;; using type hints for performace
(defn triangle [^long n]
  (loop [c 0 n n]
    (if (zero? n)
      c
      (recur (+ n c) (- n 1)))))

(triangle 1000000)
