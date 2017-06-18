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

(defn is-prime [n]
  "for odd number which is greater than 2"
  (cond
    (< n 9) true
    (= 0 (mod n 3)) false
    :else (let [r (java.lang.Math/floor (java.lang.Math/sqrt n))]
            (loop [f 5]
              (cond
                (< r f) true
                (= 0 (mod n f)) false
                (= 0 (mod n (+ f 2))) false
                :else (recur (+ f 6)))))))

(defn problem10 []
  (+ 2 (apply + (filter is-prime (range 3 2000000 2)))))

(def numbers20x20 [ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8
                   49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0
                   81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65
                   52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91
                   22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                   24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50
                   32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                   67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21
                   24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                   21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95
                   78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92
                   16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57
                   86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58
                   19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40
                   4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66
                   88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69
                   4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36
                   20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16
                   20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54
                   1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48
                   ])

(defn problem11 []
  (let [lines (partition 20 numbers20x20)
        lines4 (partition 4 numbers20x20)
        cols4 (mapcat #(partition 4 %) (map #(map (fn [x] (nth x %)) lines) (range 20)))
        dia-left-bottom (for [x (range 17)
                              y (range 17)
                              :let [xs (take 4 (iterate inc x))
                                    ys (take 4 (iterate inc y))]]
                          (map #(nth (nth lines %2) %1) xs ys))
        dia-right-bottom (for [x (range 17)
                               y (range 3 20)
                               :let [xs (take 4 (iterate inc x))
                                     ys (take 4 (iterate dec y))]]
                           (map #(nth (nth lines %2) %1) xs ys))]
    (apply max (map #(apply * %) (concat lines4 cols4 dia-left-bottom dia-right-bottom)))))

(defn count-prime-factor [x]
  (loop [n x
         m {}]
    (cond
      (even? n) (recur (quot n 2) (countup m 2))
      (< n 9) (countup m n)
      (= 0 (mod n 3)) (recur (quot n 3) (countup m 3))
      :else (let [r (java.lang.Math/floor (java.lang.Math/sqrt n))
                  p (loop [f 5]
                      (cond
                        (< r f) n
                        (= 0 (mod n f)) f
                        (= 0 (mod n (+ f 2))) (+ f 2)
                        :else (recur (+ f 6))))]
              (if (= p n) (countup m n)
                  (recur (quot n p) (countup m p)))))))

(defn countup [m k]
  (let [v (m k)]
    (if (nil? v)
      (assoc m k 1)
      (assoc m k (inc v)))))

(defn problem12 []
  (loop [n 31]
    (let [an (* (/ 2) n (inc n))
          m (count-prime-factor an)
          factors (apply * (map inc (vals m)))]
      (if (< 500 factors)
        an
        (recur (inc n))))))
