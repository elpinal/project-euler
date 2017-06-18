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
