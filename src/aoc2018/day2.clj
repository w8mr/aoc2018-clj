(ns aoc2018.day2)

;;TODO can make check for two and three generic with map

(defn part1 [input] (let [value (reduce ;; count amount of 2s and 3s over all ids
                                  (fn [[count_two count_three], id]
                                      (let [[has_two has_three]
                                            (reduce ;; check if id has 2s or 3s
                                              (fn [[has_two has_three], [char count]]
                                                  [(or has_two (= count 2)), (or has_three (= count 3))])
                                              []
                                              (reduce ;; count occurences (per id)
                                                (fn [a, c]
                                                    (update
                                                      a
                                                      (keyword (str c))
                                                      (fn [x] (+ 1 (or x 0)))))
                                                {}
                                                (seq id))
                                              )]
                                           [(+ count_two (if has_two 1 0)) (+ count_three (if has_three 1 0))]))
                                  [0 0]
                                  (clojure.string/split-lines input))]
                         (* (first value) (second value))))

(assert (= (part1 "
abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab") 12))


(part1 (slurp "resources/day2.txt")) ;; 8398


(defn part2 [input]
      (first
        (reduce
          (fn[[a x] y]
             [(let [
                    value (clojure.string/join
                            (map first
                                 (filter (partial apply =)
                                         (map vector (seq x) (seq y)))))] ;; zip characters of
                   (or a
                       (when (= (+ (count value) 1) (count y)) value)))
              y])
          [nil nil] ;; first actual accumulator second previous value
          (sort (clojure.string/split-lines input))))) ;; sort, two almost equals id's should be next to each other


(assert (= (part2 "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz") "fgij"))

(part2 (slurp "resources/day2.txt")) ;; hhvsdkatysmiqjxunezgwcdpr

