
(ns onlisp.chapter11)

(defmacro our-let
  [binds & body]
  `((fn [~@(take-nth 2 binds)]
      (do ~@body)) ~@(take-nth 2 (rest binds))))

(macroexpand-1 '(our-let [x 1 y 2] (+ x y)))
