
;given a function and a list, returns both the first matched value and the value returned from
;the matching function
(defn findr [f sq]
      (when (seq sq)
            (let [value (f (first sq))]
                 (if (nil? value)
                   (recur f (rest sq))
                   [(first sq) value]))))

(defn evenr [elem]
      (if (even? elem)
        "is even"))

(println (findr evenr '(1 13 3 4)))
(println (findr evenr '(1 13 3 5)))

; clojure
; rest :

(println (rest [1 2 3]))
(println (rest '()))
(println (rest nil))

;seq
(println (seq '(1)))
(println (seq ""))

;(defn f2
;  (let [x ()] (println (seq x)))
;  )

;(println (seq [1 2 3]))