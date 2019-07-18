(ns aoc2018.day15)

(require '[clojure.string :as str])
(require '[clojure.set :refer [difference union]])

(def input "#######
#..E..#
#.###.#
#E#.#G#
#.###.#
#.....#
#######")

(def directions [
                 (fn [{:keys [x y]}] {:x x :y (dec y)})     ;; up
                 (fn [{:keys [x y]}] {:x (dec x) :y y})     ;; left
                 (fn [{:keys [x y]}] {:x (inc x) :y y})     ;; right
                 (fn [{:keys [x y]}] {:x x :y (inc y)})])     ;; down
                 

(def playerTypes {
                  :wall   {:char "#" :type :wall :fixed true}
                  :space  {:char "." :type :space :open true :node true}
                  :goblin {:char "G" :type :goblin :player true :node true :lp 200 :enemies [:elf] :whenDead :space}
                  :elf    {:char "E" :type :elf :player true :node true :lp 200 :enemies [:goblin] :whenDead :space}})
                  

(def charPlayerTypeMap (into {} (for [[k v] playerTypes] [(:char v) v])))

(defn createNodes [cells]
      (loop
        [nodesByPos {}
         nodes []
         cells cells
         index 0]
        (let
          [[cell & remaining] cells]
          (if (empty? cells)
            [nodesByPos nodes]
            (recur
              (into nodesByPos {(:pos cell) index})
              (into nodes [{:pos (:pos cell) :index index}])
              remaining
              (inc index))))))

(defn connectNodes [nodesByPos nodes]
  (map 
    (fn [node] 
      (conj node {:to 
                  (into [] 
                    (remove nil? 
                      (map (fn [move] 
                             (get nodesByPos 
                               (move (:pos node)))) directions)))})) nodes))

(defn charsToCells [input playerTypes]
      (let [charPlayerTypeMap (into {} (for [[k v] playerTypes] [(:char v) v]))]
           (flatten
             (map-indexed
               (fn [y line]
                   (map-indexed
                     (fn [x char]
                         (conj (get charPlayerTypeMap (str char)) {:pos {:x x :y y}}))
                     line))
               (str/split-lines input)))))

(defn initContext [input playerTypes]
  (let [[players fixed nodes mx my]
        (reduce
          (fn [[players fixed nodes mx my], cell]
            [
             (if (:player cell) (conj players cell) players)
             (if (:fixed cell) (conj fixed cell) fixed)
             (if (:node cell) (conj nodes cell) nodes)
             (max mx (:x (:pos cell)))
             (max my (:y (:pos cell)))])
          
          [[] [] [] 0 0]
          (charsToCells input playerTypes))
        [nodesByPos nodeList] (createNodes nodes)
        connectedNodeList (connectNodes nodesByPos nodeList)]
    {:playerTypes playerTypes
     :players     players
     :fixed       fixed
     :nodesByPos  nodesByPos
     :nodes       connectedNodeList
     :size        {:width (inc mx) :height (inc my)}}))
            
(def context (initContext input playerTypes))


(def player (get (:players context) 0))

player

(defn playersByNode [context]
  (into {} (map (fn[player] [(get (:nodesByPos context) (:pos player)) player]) (:players context))))

(playersByNode context)

(defn playerNodes [context]
  (map (fn[player] (get (:nodesByPos context) (:pos player))) (:players context)))

(playerNodes context)

(defn inRangeNodes [context enemies]
      (into #{} (remove nil? (map (fn[pos] (get (:nodesByPos context) pos)) (mapcat (fn [enemy] (map (fn [move] (move (:pos enemy))) directions)) enemies)))))

(defn enemy? [player other]
  (some #{(:type other)} (:enemies player)))

(defn findEnemies [context player]
      (filter (partial enemy? player) (:players context)))


(def enemies (findEnemies context player))

enemies

(def irn (inRangeNodes context enemies))

irn

(defn playerNode [player] (get (:nodesByPos context) (:pos player)))

(playerNode player)

(defn predicate [node] (contains? irn node))

(defn neighbours [nodes node] (map 
                                (fn[x] 
                                  (apply conj [] x node)) 
                                (:to (nth nodes (first node)))))

(neighbours (:nodes context) [1])

(defn remainingNodes [remaining]
  (into #{} (map first remaining)))

(remainingNodes [[5 0 1] [3 2 1] [7 2 1]])

(defn filteredNodes [paths filters]
  (remove 
    (fn[path] 
      (some (fn[filter] (contains? filter (first path))) filters))
    paths))

(filteredNodes [[1] [0 1] [3 0 1]] [#{21} #{0} #{22 3}])
  
(defn filteredNeighbours [nodes node visited remaining blocked]
  (filteredNodes 
    (neighbours nodes node) 
    [visited 
     (remainingNodes remaining) 
     blocked]))


(filteredNeighbours (:nodes context) [1] #{} [] #{2})
(filteredNeighbours (:nodes context) [0 1] #{1} [[2 1] [6 1]] #{2})
(filteredNeighbours (:nodes context) [2 1] #{1 0} [[6 1] [5 0 1]] #{2})
(filteredNeighbours (:nodes context) [6 1] #{1 0 2} [[5 0 1] [3 2 1] [7 2 1]] #{2})
(filteredNeighbours (:nodes context) [5 0 1] #{1 0 2 6} [[3 2 1] [7 2 1]] #{2})




(defn shortestPath [nodes start predicate blocked] 
  (loop [visited #{}
         check  (conj (clojure.lang.PersistentQueue/EMPTY) [start])]
    (let [next (peek check)
          remaining (pop check)]
      (if next
        (let 
          [result (predicate (first next))]
          (if result
            next
            (recur 
              (conj visited (first next))
              (apply conj remaining (filteredNeighbours nodes next visited remaining blocked)))))
        false))))

(into #{} (playerNodes context))

(def sp (shortestPath (:nodes context) 2 predicate (into #{} (playerNodes context))))

sp

(peek (pop sp))

(defn rowCells [context playersByNode y]
  (str/join 
    (for [x (range (:width (:size context)))]
       (let [node (get (:nodesByPos context) {:y y :x x})]
         (if node
           (let [player (get playersByNode node)]
             (if player
               (:char player)
               "."))
           "#")))))

(rowCells context (playersByNode context) 3)

(defn rowLifePoints [context playersByNode y]
  (str/join ", " 
    (remove nil? 
      (for [x (range (:width (:size context)))]
        (let [node (get (:nodesByPos context) {:y y :x x})]
          (when node
            (let [player (get playersByNode node)]
              (when player
                (str "(" (:char player) ":" (:lp player) ")")))))))))

(rowLifePoints context (playersByNode context) 3)

(defn displayString [context]
  (let [playersByNode (playersByNode context)]
    (str/join 
      (for [y (range (:height (:size context)))]
        (str 
          (rowCells context playersByNode y) "   "
          (rowLifePoints context playersByNode y) "\n")))))

(displayString context)

(defn attack [context playerNodeIndex]
  (let 
    [nodes (:nodes context)
     playersByNode (playersByNode context)
     player (get playersByNode playerNodeIndex)
     playerNode (nth nodes playerNodeIndex)
     neighbourNodes (:to playerNode)
     neighbourPlayers (remove nil? (map (partial get playersByNode) neighbourNodes))
     enemyPlayers (filter (partial enemy? player) neighbourPlayers)
     sortedPlayers (sort-by :lp enemyPlayers)
     attackPlayer (first sortedPlayers)
     attackPlayerIndex (.indexOf (:players context) attackPlayer)]
    (if (>= attackPlayerIndex 0)
      (let 
        [context (update-in context [:players attackPlayerIndex :lp] #(- % 3))
         players (:players context)]
        (if (<= (:lp (get players attackPlayerIndex)) 0)
          (do 
            (assoc-in context [:players] 
              (vec (concat 
                     (take attackPlayerIndex players)
                     (drop (inc attackPlayerIndex) players)))))
          context))
      context)))

(defn playerMoveTo [context moveToNode player]
  (let 
    [movePlayerIndex (.indexOf (:players context) player)
     moveToPos (:pos (nth (:nodes context) moveToNode))]
    (attack
      (assoc-in context [:players movePlayerIndex :pos] moveToPos)
      moveToNode)))

(defn move [context player inRangeNodes] 
  (let 
    [playerNode (playerNode player)
     predicate (fn [node] (contains? inRangeNodes node))
     nodes (:nodes context)
     playerNodesSet (into #{} (playerNodes context))
     shortestPath (shortestPath nodes playerNode predicate playerNodesSet)
     moveToNode (peek (pop shortestPath))]
    (if shortestPath
      (playerMoveTo context moveToNode player)
      context)))
    
(defn play [context player]
  (let 
    [playerNode (playerNode player)
     enemies (findEnemies context player)
     inRangeNodes (inRangeNodes context enemies)]
    (if (contains? inRangeNodes playerNode)
      (attack context playerNode)
      (move context player inRangeNodes))))

(defn orderPlayers [context]
  (update-in 
    context 
    [:players] 
    (fn [players] 
      (into [] 
        (sort-by 
          (juxt 
            #(:y (:pos %)) 
            #(:x (:pos %))) 
          players)))))

(defn playRound [context]
  (loop 
    [context (orderPlayers context)
     index 0]
    (if (< index (.size (:players context)))
      (recur 
        (play context 
          (get (:players context) index)) 
        (inc index))
      context)))

(def context (playRound context))
(displayString context)
    
(def context (playRound context))
(displayString context)
  
(def context (playRound context))
(displayString context)

(def context (playRound context))
(displayString context)

(def context (playRound context))
(displayString context)

(def context (playRound context))
(displayString context)

(def context (playRound context))
(displayString context)

(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)
(def context (playRound context))
(displayString context)



 

;TODO use partials as much as possible
;TODO use apply as much as possible
;TODO use thread last macro as much as possible
;TODO remove dependency on position












