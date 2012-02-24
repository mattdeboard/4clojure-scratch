(ns cljscratch.core)
(def myseq [1 2 3 4 5 6])
(mapcat list (take-nth 2 myseq) #{(take-nth 2 myseq)} myseq)
(partition 3 3 [0] (apply interleave (partition 3 3 [0] myseq)))

(let [[a b] (partition 3 3 [0] (apply interleave (partition 3 3 [0] myseq)))]
  (concat a b))
(partition 3 3 [0] (apply interleave (partition 3 3 [0] myseq)))
(#(partition %2 %2 [0] (apply interleave (partition %2 %2 [0] %))) myseq 2)

(partition-all 2 (apply interleave (partition-all 5 (range 10))))

(defn ri [coll n]
  (let [k (/ (count coll) n)]
    (partition-all k (apply interleave (partition-all n coll)))))

(ri (range 10) 5)
(partition-all 3 myseq)
(defn modulo [& args] (apply mod args))

(modulo 10 4)
(defn rotate [n coll]
  (mapcat merge
          (let [splt (partial split-at (mod n (count coll)))]
            (reverse (splt coll)))))

(rotate -1 [:a :b :c])
(rotate 6 [1 2 3 4 5])
(rotate 2 [1 2 3 4 5])
(((fn [x] #(x %2 %)) nth) 2 [1 2 3 4 5])

(def test-monkey [1 :a 2 :b 3 :c])
(sort-by (fn [x] (type x)) [1 :a 2 :b 3 :c])
(map (fn [x] (type x)) test-monkey)
(keys (apply hash-map test-monkey))
(vals (apply hash-map test-monkey))

(defn sort-by-type [coll]
  (keys (reduce merge (map #(hash-map % (type %)) coll))))

(sort-by-type test-monkey)

((fn [coll] (split-with (partial isa? (type (first coll)))
                        coll)) test-monkey)

(sorted-map-by #(map type %) test-monkey)
(for [n test-monkey] (type n))
(defn type-map [coll]
  (->> (map (fn [x] (hash-map (type x) x)) coll)
       (reduce (partial merge-with vector))
       vals))

(defn type-map [coll]
  (->> (map (fn [x] (hash-map (type x) x)) coll)
       (apply merge-with #(vector % %2))))

(defn type-map [coll]
  (vals (group-by #(type %) coll)))

(= set (#{[1 2 3]}))
(= (set (type-map [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
(= (set (type-map [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(type-map [1 :a 2 :b 3 :c])
(def tm (type-map [[1 2] :a [3 4] 5 6 :b]))
tm
(group-by #(type %) [[1 2] :a [3 4] 5 6 :b])

(->> tm (reduce (partial merge-with vector)) vals set)
((comp vals (partial reduce (partial merge-with vector))) tm)
(for [t tm]
  (vals t))

(flatten [[2 5] 6])
(apply merge-with #(flatten (vector % %2)) [{:a 1} {:b 2} {:c 3} {:b 5} {:b 6}])
(into [1 2] [3 4])
(into (vector 5 6))
(sort-by #(find type-map %) test-monkey)

(find type-map 1)

(map type test-monkey)

(instance? (type (first test-monkey)) (nth test-monkey 1))

(take-while #(partial < %) [1 0 1 2 3 0 4 5])

(def myseq [1 0 1 2 3 0 4 5])


(let [s (drop-while #(< (apply min myseq) %) myseq)]
  (take-while
   (fn [x]
     (or
      (= (first s) x)
      (= 1 (- x (nth s (dec (.indexOf s x)))))))
   s))

(nth myseq (dec (.indexOf myseq 0)))
    

  
(drop-while #(partial < (apply min myseq)) myseq)

(drop-while #(< (apply min myseq) %) myseq)
(first (drop-while (partial < (apply min myseq)) myseq))
    
  
(flatten (vector
 (for [a s1 b s2]
   (if (not= a b)
     1
     0))
 (- (count s2) (count s1))))

(partition-by #(type %) ["a" ["b"] "c"])
(partition-by #(type %) '((1 2) 3 [4 [5 6]]))

#(filter (complement coll?)
         (rest (tree-seq coll? seq %)))

(tree-seq sequential? seq '((1 2) 3 [4 [5 6]]))

(apply str ((fn [x] (mapcat set (partition-by #(identity %) x))) "Leeeerrrrroyyy"))

(partition-by #(identity %) [1 1 2 1 1 1 3 3])

(mapcat (partial repeat 2) [1 2 3])
(mapcat (partial repeat 2) [[1 2] [3 4]])

(#(mapcat (partial repeat %2) %) [1 2 3] 2)

(#(vector (take % %2) (drop % %2)) 3 [1 2 3 4 5 6 7])

(defn long-subseq [coll]
  (let [L (apply min coll)
        front (take (/ (count coll) 2) coll)
        back (drop (/ (count coll) 2) coll)
        tally []]
    (for [[x & xs] front]
      (if (not= L x)
        (conj tally x)))))



(apply min myseq)
(defn subseq- [coll]
  (partition-by (fn [x] (= 1 (- (second x) (first x))))
                (filter #(< (first %) (second %))
                        (partition 2 1 coll))))
((fn [coll]
  (->> (partition 2 1 coll)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (< (count %) (count %2)) %2 %) [])
       flatten
       distinct)) myseq)

(filter #(= 1 (- (second (first %)) (ffirst %))) (partition-by #(- (second %) (first %)) (partition 2 1 myseq)))


                      

(drop-while #(< (count %) 2)
            (partition-by #(= (first %) (second %))
                          (partition 2 1 myseq)))

(def test-vecs [[1 0 1 2 3 0 4 5] [5 6 1 3 2 7] [2 3 3 4 5] [7 6 5 4]])
test-vecs
(def myseq [1 0 1 2 3 0 4 5])

(vector
 (vec (set (flatten (first (subseq- myseq))))))

(vector
 (for [i test-vecs]
   (vec (set (flatten (first (subseq- i)))))))

(for [i test-vecs]
  (letf [j (drop-while #(< (count %) 2)
                      (partition-by #(= (first %) (second %))
                                    (partition 2 1 i)))]
    (for [k j]
      (filter #(= 1 (- (second %) (first %))) k))))

(def myseq (partition 2 1 myseq))

(defn filter-decs [coll]
  (filter #(< (first %) (last %)) coll))

(defn into-overlap [coll1 coll2]
  "Concatenate overlapping collections, as long as the elements of each
coll is unique local to the coll."
  ;; If the last element of coll1 is the same as the first element of
  ;; coll2, and neither arg is a repetition, e.g. [3 3], then concatenate
  ;; the three unique values.
  (if (and
       (= (last coll1) (first coll2))
       (not-any? #(= (first %) (second %)) [coll1 coll2]))
    (concat coll1 [(second coll2)])))

(let [[a b] (take 2 (filter-decs (partition 2 1 myseq)))]
  (into-overlap a b))

((fn [coll]
   (->> (partition 2 1 coll)
        (partition-by #(
 myseq)


(let [[a b] (take 2 (filter-decs (partition 2 1 [1 0 1 2 3 0 4 5])))]
  (prn a)
  (into-overlap a b))

(filter-decs (partition 2 1 [1 0 1 2 3 0 4 5]))


(take 2 (partition 2 1 myseq))
(into-overlap [2 3] [3 3])
(concat [2 3] [(second [3 4])])

(last (-> [2 5 4 1 3 6] reverse rest sort))


(defn analyze-ttt [colls]
  (let [a (first colls)
        b (second colls)
        c (last colls)
        vals {:e 0
              :x 1
              :o -1}]
      (for [x [a b c]]
        (case (reduce + (map #(% vals) x))
          0 nil
          3 "X"
          -3 "O")

(analyze-ttt [[:x :x :x]
              [:x :x :x]
              [:x :x :x]])
            
            

(case (reduce + (map #(% vals) [:x :x :x]))
  0 nil
  3 "X"
  -3 "O")

(case (reduce + (map #(% vals)
                     (mapcat (partial take 1)
                             [[:x :e :o] [:x :o :e] [:x :o :e]])))
  0 nil
  3 "X"
  -3 "O")
  

(def vals {:e 0 :x 1 :o -1})
(defn abs [n]
  (if (pos? n)
    n
    (* -1 n)))

(abs -3)
      
(apply hash-map (interleave [:a :b :c] [1 2 3]))


(defn gcd [a b]
  (if (= 0 (mod a b))
    b
    (gcd b (mod a b))))

(gcd 5 7)

(gcd 27 18)

(fn [x y]
  (set
   (drop-while #(nil? %)
               (set (for [a y]
                    (if (contains? x a)
                      a)))))))

(fn times-two [n]
  (cons n (lazy-seq (times-two (* 2 n)))))

(take 5 (times-two 1))

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
