(ns cljscratch.core)
(defn dilataco
  [{:keys [x y dados]} elemento-estruturante]
  (let [a (vec dados)
        b (vec (:dados elemento-estruturante))
        coll (map (fn [el]
                    (set (map
                          #(vector (+ (%1 0) (el 0))
                                   (+ (%1 1) (el 1)))
                          a)))
                  b)
        validos (set
                 (filter #(and (< (% 0) x) (< (% 1) y))
                         (apply union conj)))]
    {:x x :y y :dados validos}))


(loop [x 5
       result []]
  (if (> x 0)
    (recur (dec x) (conj result (+ 2 x)))
    result))


#(if (and (some #{%} (keys %2)) (nil? (% %2)))
   true
   false)

(:c {:a nil :b 2}) #{:c} {:a nil :b 2})

(if (some #{:a} (keys {:a nil :b 2}))
  true
  false)

(for [x (range 40)
            :when (= 1 (rem x 4))]
  x)

#(if (= (first %) (last %))
   true
   false)

(fn [n]
  (for [i (range n)]
    (reduce + (take 2 i))))

(for [i (range 3)]
  i)

(#(take % ((fn rfib [a b]
             (cons a (lazy-seq (rfib b (+ a b)))))
           1 1)) 3)

(cons 1 '(1))

(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

(frequencies [1 1 2 3 2 1 1])

(apply hash-map
       (flatten
        (for [i (group-by (fn [x] x) [1 1 2 3 2 1 1])]
          [(first i) (count (second i))])))

;; #55
(apply hash-map (apply concat (for [i (group-by (fn [x] x) [1 1 2 3 2 1 1])]
  [(first i) (count (second i))])))

(= (sort (#(for [i (group-by (fn [x] x) %)] (first i)) (range 50))) (range 50))


#(sort (for [i (group-by (fn [x] x) %)] (first i)) (range 50))
