(ns project-euler.core)

(defn is3or5 [n]
  (let [i3 (mod n 3)
        i5 (mod n 5)]
    (or (= 0 i3) (= 0 i5))
    ))

(defn problem1
  "Problem 1"
  []
  (apply + (filter is3or5 (range 1 1000))))

(defn evens-of-fib [end]
  (loop [stack '(2)
         fst 5
         snd 8]
    (if (< end snd)
      stack
      (let [fst' (+ fst (* snd 2))]
        (recur (conj stack snd) fst' (+ fst' fst snd))))))

(defn problem2
  []
  (apply + (evens-of-fib 4000000)))

(defn problem3 []
  (loop [prime 2
         stack (filter #(not= 0 (mod % 2)) (drop 3 (range)))
         n 600851475143]
    (if (= prime n)
      prime
      (let [prime' (first stack)
            stack' (filter #(not= 0 (mod % prime')) stack)
            n' (if (= 0 (mod n prime)) (quot n prime) n)]
        (recur prime' stack' n')))))

(defn is-prod-of-3-digit [n]
  (not (empty? (for [x (range 999 99 -1)
                     y (range x 99 -1)
                     :when (= n (* x y))]
                 true))))

(defn problem4 []
  (first (for [n1 (take 10 (iterate #(- % 100001) 999999))
               n2 (take 10 (iterate #(- % 10010) n1))
               n3 (take 10 (iterate #(- % 1100) n2))
               :when (is-prod-of-3-digit n3)]
           n3)))

(defn problem5 []
  (let [init (* 3 5 7 11 13 17 19)]
    (first (for [n (iterate #(+ % init) init)
                 :when (every? #(= 0 (mod n %)) (range 6 20))]
             n))))

(defn square [n] (* n n))

(def problem6
  (let [sum (apply + (map square (range 1 101)))
        square (square (apply + (range 1 101)))]
    (- square sum)))

(defn primes [xs ps n]
  (let [p (first xs)
        xs' (rest xs)]
    (cond
      (= n 0) (first ps)
      (every? #(not= 0 (mod p %)) ps) (recur xs' (cons p ps) (dec n))
      :else  (recur xs' ps n))))

(defn problem7 []
  (primes (range 3 300000 2) '(2) 10000))
