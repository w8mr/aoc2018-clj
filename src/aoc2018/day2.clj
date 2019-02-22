(ns aoc2018.day2)


(defn part1 [input] (let [value (reduce
                                  (fn [acc, id]
                                      (let [value
                                            (reduce
                                              (fn [acc, kv]
                                                  [(or (first acc) (= (second kv) 2)), (or (second acc) (= (second kv) 3))])
                                              [false, false]
                                              (reduce
                                                (fn [a, c]
                                                    (update
                                                      a
                                                      (keyword (str c))
                                                      (fn [x] (+ 1 (or x 0)))))
                                                {}
                                                (seq id)))]
                                           [(+ (first acc) (if (first value) 1 0)) (+ (second acc) (if (second value) 1 0))]))
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

