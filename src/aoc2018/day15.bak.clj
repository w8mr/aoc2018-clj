

;;(defn playPlayer [context player])

      

;;(defn getCell [field pos] (nth (nth field (:y pos)) (:x pos)))

;;(defn posToString [{:keys [x y]}] (str "(" x "," y ")"))

;;(defn cellPos [cell] (select-keys cell [:x :y]))

;;(defn attack [field cell enemy]
;;      (print (str (posToString (cellPos cell)) " attacks " (posToString enemy)))
;;      (swap! (getCell field enemy)
;;             (fn [cell]
;;                 (let [updatedCell (update cell :lp - 3)]   ;; TODO move -3 to config
;;                  (if (>= (:lp updatedCell) 0)
;;                     updatedCell
;;                     (conj ((:whenDead updatedCell) playerTypes) (cellPos enemy))
;;                        
;;      :attack
;;
;;(defn possibleMoves [field pos] (filter (fn [newpos] (:open @(getCell field newpos))) (map (fn [mv] (mv pos)) directions)))
;;
;;(defn swapCells [field pos1 pos2]
;;      (let [cell1 (getCell field pos1)
;;            cell2 (getCell field pos2)
;;            copy @cell1
;;           (swap! cell1 (fn [{:keys [x y]}] (conj @cell2 {:x x :y y})))
;;           (swap! cell2 (fn [{:keys [x y]}] (conj copy {:x x :y y})))
;;           

;;(defn weakestEnemyInRange [field enemies pos]
;;      (first
;;        (sort-by (juxt :lp :index)
;;          (map-indexed (fn [index, enemy] (conj enemy {:index index}))
;;            (filter (fn [y] (some #{(:type y)} enemies))
;;              (map (fn [x] @(getCell field x))
;;                (map (fn [moveTo] (moveTo pos)) directions)

;;(defn simulateWalk
;;  ([field predicate pos]
;;   (loop [visited #{}
;;          check (conj (clojure.lang.PersistentQueue/EMPTY) [pos nil])
;;     (let [[next] check]
;;       (if next
;;         (let [[move direction] next
               result (predicate move)
               possible (into #{} (possibleMoves field move))
               newPossible (difference possible visited (into #{} check))
               newCheck (apply conj (pop check) (map (fn [x] [x (or direction x)]) newPossible))
           
           (if result
             direction
             (recur (conj visited move) newCheck))
         
         false
                     

(defn enemies? [field enemies]
      (first (filter (fn [type] (some #{type} enemies)) (map :type (map deref (flatten field))))))


(defn move [field cell]
      (let [newpos (simulateWalk field (partial weakestEnemyInRange field (:enemies cell)) (select-keys cell [:x :y]))]
           (if newpos
             (do
               (print (str (posToString (cellPos cell)) " moves to " (posToString newpos)))
               (swapCells field (select-keys cell [:x :y]) newpos)
               (let [movedCell @(getCell field newpos)]
                    (let [enemy (weakestEnemyInRange field (:enemies cell) (cellPos movedCell))]
                         (when enemy
                               (attack field movedCell enemy))))
                                                          ;; TODO combine move and attack
                    
               :moved)
               
             (do
               (print (str (posToString (cellPos cell)) " has no moves"))
               (if (enemies? field (:enemies cell))
                 :notreachable
                 :noenemies)))))
                 
               

(defn playCell [field cell]
      (when (:player @(getCell field (cellPos cell)))       ;; check if cell is player
            ;;    (print (str (:y cell) " " (:x cell) " " (:type cell) " " (:lp cell)))
            (let [enemy (weakestEnemyInRange field (:enemies cell) (cellPos cell))]
                 (if enemy
                   (attack field cell enemy)
                   (move field cell)))))
                   

(defn playRound [field]
      (reduce #(and %1 (not= %2 :noenemies)) true
              (map (fn [item] (playCell field item))
                   (into []
                         (filter :player (map deref (flatten field)))))))
              
      

(defn fieldToString [field]
      (str/join (map (fn [row]
                         (str (str/join (map (fn [cell] (:char @cell)) row)) "   "
                              (str/join "," (map (fn [cell] (str (:char @cell) "(" (:lp @cell) ")")) (filter (fn [x] (>= (:lp @x -1) 0)) row))) "\n"))
                     field)))



(defn readInputToField [input] (map-indexed
                                 (fn [y line]
                                     (map-indexed
                                       (fn [x char]
                                           (atom
                                             (conj (get charPlayerTypeMap (str char)) {:x x :y y})))
                                       line))
                                 (str/split-lines input)))

(defn play [field]
      (let [result (atom true)
            round (atom 0)]
           (print (str "Initially\n"))
           (print (fieldToString field))

           (while @result
                  (do
                    (swap! round inc)
                    (print (str "Round " @round "\n"))
                    (swap! result (fn [o] (playRound field)))
                    (print (fieldToString field))
                    (when (> @round 100)
                      (swap! result (fn [old] false)))))
        
        (let [totallp (reduce + (map :lp (filter :player (map deref (flatten field)))))
              totalrounds (dec @round)]
          (print (str "Finished in round " totalrounds "\n"))
          (print (str "Player points " totallp "\n"))
          (print (str "Result " (* totalrounds totallp) "\n"))
          (* totalrounds totallp))))
                

           


(assert (= (play (readInputToField "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")) 20755))


(assert (= (play (readInputToField "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######")) 36334))

(assert (= (play (readInputToField "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######
")) 39514))

(assert (= (play (readInputToField "#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######
")) 27755))

(assert (= (play (readInputToField "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
")) 28944))

(assert (= (play (readInputToField "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
")) 18740))


(def input (slurp "resources/day15.txt"))

(play (readInputToField input))

;;TODO chance x and y to pos array
;;TODO reorganize methtopods into play method.
;;TODO move from atom based field to recursive play rounds
;;
;;
