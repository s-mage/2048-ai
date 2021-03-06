(ns twentyfortyeight-ai.game
  (:require [om.core :as om :include-macros true]))

(declare winner? loser?)
(def not-nil? (complement nil?))

(defn randomize-start []
  "Returns two different integers from 0 to 15.  These are the two
  initial positions that start with 2."
  (let [x (rand-int 16)
        y (rand-int 16)]
    (if (= x y)
      (recur)
      [x y])))

(defn init-board []
  "Creates a 2048 game board.
  A board is a vector containing 16 elements representative
  of three rows.  14 of the elements will begin as 0.  2 will
  have the value of 2."
  (let [[x y] (randomize-start)
        coll (vec (repeat 16 nil))]
    (assoc coll x 2 y 2)))

(def board-state (atom (init-board)))

(defn get-open [coll]
  "Returns the indices of all zero items in a collection."
  (map first (filter #(nil? (second %)) (map-indexed vector coll))))

(def num->score
  (reduce
    #(let [x (Math/pow 2 (+ %2 1))] (conj %1 {x (* x %2)}))
    {}
    (range 11)))

(defn score [atom-board]
  (reduce + (map num->score (filter not-nil? @atom-board))))

(defn msg [atom-board]
  (cond
    (winner? @atom-board) "Winner!"
    (loser?  @atom-board) "Loser!"
    :else nil))

(defn choose-random [coll]
  "Given a vector, chooses a random element."
  (get coll (rand-int (count coll))))

(defn add-new-block [coll]
  "Adds a new block to a random location on the board."
  (if-let [open ((comp choose-random vec get-open) coll)]
    (assoc coll open 2)
    coll))

(def board-edges {:left [0 4 8 12]
                  :right [3 7 11 15]
                  :top [0 1 2 3]
                  :bottom [12 13 14 15]})

(defn movable? [direction coll]
  "Given a direction and collection representing the board's current state,
  determines if a move in that direction is possible."
  (let [edge (direction board-edges)
        xs (map #(nth coll %) edge)]
    (boolean (some nil? xs))))

(defn combine [xs ys]
  "Given two collections of non-nil elements,
  recursively combines them according to the game rules.
  Example:
  [] [2 2 4 8] ;=> [4 4 8]
  "
  (let [x (first ys)
        y (second ys)]
    (if (seq ys)
      (if (= x y)
        (combine (conj xs (+ x y)) (drop 2 ys))
        (combine (conj xs x) (drop 1 ys)))
      xs)))

(defn pad-with-nils [coll]
  "A helper function to ensure coll has 4 elements.  Pads with
  nils if less than 4."
  (take 4 (concat coll (repeat nil))))

(defn combine-row [coll]
  "A helper function to take a collection representing a
  game row, filter out nil elements, combine the tiles where
  appropriate, and pad it back up with nils."
  (let [filtered (filter not-nil? coll)
        combined (combine [] filtered)]
    (pad-with-nils combined)))

(defn combine-reversed-row [coll]
  (reverse (combine-row (reverse coll))))

(defn combine-rows [coll-of-colls reverse?]
  (let [combine-fn (if reverse? combine-reversed-row combine-row)]
    (map combine-fn coll-of-colls)))

(defn rearrange-rows [coll-of-colls]
  "Creates a new collection with all nil elements at the end of each coll"
  (let [not-nils (map #(filter not-nil? %) coll-of-colls)]
    (map pad-with-nils not-nils)))

(defn position-rows [coll vertical]
  "Partitions the rows in the correct orientation depending upon the direction."
  (if vertical
    (->> coll
         (partition 4)
         (apply interleave)
         (partition 4))
    (partition 4 coll)))

(defn vertical-rows? [direction]
  "Pred for determining if we need to partition into columns."
  (or (= direction :up) (= direction :down)))

(defn slide? [direction]
  "Pred for determining if we need to put the nils before the non-nils."
  (or (= direction :right) (= direction :down)))

(defn slide [coll]
  (let [not-nils (filter not-nil? coll)
        nils (repeat (- 4 (count not-nils)) nil)]
    (concat nils not-nils)))

(defn arrange-and-combine [coll direction]
  (let [should-slide (slide? direction)
        colls (combine-rows
                (-> coll
                  (position-rows (vertical-rows? direction))
                  rearrange-rows)
                should-slide)]
    (if should-slide
      (map slide colls)
      colls)))

(defn process-move [coll direction]
  "Given a direction, reorients and combines the tiles according
  to game rules.  If applicable, we add a new block."
  (let [colls (arrange-and-combine coll direction)]
    (if (vertical-rows? direction)
      (vec (flatten (apply interleave colls)))
      (vec (flatten colls)))))

(def directions [:up :right :down :left])

(defn moves [board]
  (reduce #(conj %1 {%2 (process-move board %2)}) {} directions))

(defn possible-moves [board]
  (filter #(not= board (val %)) (moves board)))

(defn winner? [board]
  (boolean (some #{2048} [board])))

(defn loser? [board]
  (empty? (possible-moves board)))

(defn move [direction cursor]
  "Updates the application state when a move has been issued."
  (let [data @cursor
        moved (process-move data direction)]
    (when-not (= moved data)
      (om/update! cursor (add-new-block moved)))))
