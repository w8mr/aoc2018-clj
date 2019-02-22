(ns aoc2018.day2)

(defn part1
      ([input] (part1 input [2 3]))
      ([input check]
              (reduce *
                      (reduce
                        #(map + %1 %2)
                        (map
                          (fn [id]
                              (map #(if % 1 0)              ;; turn boolean in 1 and 0
                                   (reduce                  ;; check if id has check counts ; returns vector of booleans
                                     (fn [arr [char count]] (map #(or %1 (= count %2)) arr check))
                                     (map (fn[x] false) check)
                                     (reduce                ;; count occurences (per id) ; returns map of char -> count
                                       (fn [a, c]
                                           (update a (keyword (str c)) #(inc (or % 0))))
                                       {}
                                       (seq id))
                                     )))
                          (clojure.string/split-lines input))))))

(assert (= (part1 "abcdef
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

(part2 (slurp "resources/day2.txt"))                        ;; hhvsdkatysmiqjxunezgwcdpr

