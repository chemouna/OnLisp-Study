
(ns onlisp.chapter7)

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

(defmacro our-when
  [test & body]
  '(if ~test
     (do
       ~@body)))

(defmacro our-while
  [tst & body]
  `(loop []
    (when ~tst
      ~@body
      (recur))))

(defmacro when-bind [bindings & body]
  (let [[form tst] bindings]
    `(let [~form ~tst]
       (when ~form
         ~@body
         ))))

(when-bind [a (+ 1 2)] (println "a is " a))

(macroexpand-1 `(our-while able laugh))

(defmacro mac [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))

;(d)(mac when-bind)

;(macroexpand-1 when-bind)
;(macroexpand our-while)

;(defmacro memq (obj lst)
;  '(member ))

