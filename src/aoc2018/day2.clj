(ns aoc2018.day2)

;;TODO can make check for two and three generic with map

(defn part1 [input] (reduce * (reduce (fn [acc vec] (map + acc vec))
                                      (map (fn [id]
                                               (map #(if % 1 0)                           ;; turn boolean in 1 and 0
                                                    (reduce                               ;; check if id has 2s or 3s ; returns vector of booleans
                                                      (fn [[has_two has_three], [char count]]
                                                          [(or has_two (= count 2)), (or has_three (= count 3))])
                                                      []
                                                      (reduce                             ;; count occurences (per id) ; returns map of char -> count
                                                        (fn [a, c]
                                                            (update
                                                              a
                                                              (keyword (str c))
                                                              (fn [x] (+ 1 (or x 0)))))
                                                        {}
                                                        (seq id))
                                                      ))) (clojure.string/split-lines input)))))

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



(reduce * (reduce (fn [acc vec] (map + acc vec))
        (map (fn [id]
                 (map #(if % 1 0)                           ;; turn boolean in 1 and 0
                      (reduce                               ;; check if id has 2s or 3s ; returns vector of booleans
                        (fn [[has_two has_three], [char count]]
                            [(or has_two (= count 2)), (or has_three (= count 3))])
                        []
                        (reduce                             ;; count occurences (per id) ; returns map of char -> count
                          (fn [a, c]
                              (update
                                a
                                (keyword (str c))
                                (fn [x] (+ 1 (or x 0)))))
                          {}
                          (seq id))
                        ))) ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])))