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

(defn problem8 []
  (let [longNumber "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"]
    (apply max (map (fn [xs] (apply * (map #(Character/digit % 10) xs))) (partition 13 1 longNumber)))))

(defn problem9 []
  (for [a (range 1 999)
        b (range 1 (inc a))
        c (range 1 (inc b))
        :let [a' (* a a)
              b' (* b b)
              c' (* c c)]
        :when (and (= 1000 (+ a b c)) (= a' (+ b' c')))]
    (* a b c)))
