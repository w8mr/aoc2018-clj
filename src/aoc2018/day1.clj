(ns aoc2018.day1)

(def input (map read-string
                (clojure.string/split-lines
                  (slurp "resources/day1.txt"))))

(defn part1 [input] (reduce + input))

(assert (= (part1 [+1 +1 +1]) 3))
(assert (= (part1 [+1 +1 -2]) 0))
(assert (= (part1 [-1 -2 -3]) -6))

(part1 input) ;; 536

(defn part2
      ([input] (part2 #{0} 0 (reduce conj (clojure.lang.PersistentQueue/EMPTY) input)))
      ([visited freq input] (let
                 [check (peek input)
                  new (+ freq check)]
                 (if (contains? visited new)
                   new
                   (recur (conj visited new) new (pop (conj input check)))
                   ))))


(assert (= (part2 [+1 -1]) 0))
(assert (= (part2 [+3 +3 +4 -2 -4]) 10))
(assert (= (part2 [-6 +3 +8 +5 -6]) 5))
(assert (= (part2 [+7 +7 -2 -7 -4]) 14))


(part2 input) ;; 75108
