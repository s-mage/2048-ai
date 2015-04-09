(ns twentyfortyeight-ai.ai
  (:require [twentyfortyeight-ai.core :as c]
            [om.core :as om :include-macros true]))

(declare score)

(defn nature-moves [board]
  (filter #(not (nil? %)) (map-indexed #(if (nil? %2) (assoc board %1 2)) board)))

(def directions [:up :right :down :left])

(defn moves [board]
  (reduce #(conj %1 {%2 (c/process-move board %2)}) {} directions))

(defn possible-moves [board]
  (filter #(not= board (val %)) (moves board)))

(def player-moves possible-moves)

(defn one-step-forward [board]
  "Nature moves and player moves, returns list of boards."
  (distinct (apply concat (map #(vals (player-moves %)) (nature-moves board)))))

(defn moves-results [board]
  (reduce #(conj %1 {(key %2) (score (val %2))}) {} (possible-moves board)))

(defn win? [board]
  (boolean (some #{2048} [board])))

(defn lose? [board]
  (empty? (possible-moves board)))

(defn gameover? [board]
  (or (win? board) (lose? board)))

(defn transpose [board] (apply interleave (partition 4 board)))

(defn one-line-score [v]
  (reduce + (map #(let [[x y] %] (Math/abs (- x y))) (partition 2 1 v))))

(defn subscore [v]
  (reduce + (map one-line-score (partition 4 v))))

(defn empty-score [board]
  (+ 1 (count (filter nil? board))))

(defn max-in-corner-score [board]
  (let [corners [0 3 12 15]
        el (apply max board)
        els-in-corners (map board corners)]
    (if (some #{el} els-in-corners)
      1.5
      1)))

(defn lose-score [board]
  (if (lose? board) 0.01 1))

(defn score [board]
  (/ (+ (subscore board) (subscore (transpose board)))
     (empty-score board)
     (max-in-corner-score board)
     (lose-score board)))

(defn board->score [board]
  "Get min score for direction in next 2 moves"
  (let [moves (one-step-forward board)
        scores (map score moves)]
    (apply min scores)))

; Simply choose best next move.
; (defn next-move [board]
;   (key (apply min-key val (moves-results board))))

(defn next-move [board]
  "User move, then nature move, then user move again.
  Choose direction on which final score will be min"
  (let [score-fun (if (< (apply max board) 128) score #(+ (board->score %) (score %)))
        nmr (reduce #(conj %1 {(key %2) (score-fun (val %2))}) {} (player-moves board))]
    (key (apply min-key val nmr))))

(defn move! []
  (c/move (minimax @c/board-state) (om/root-cursor c/board-state)))

(defn play! []
  (while (not (gameover? @c/board-state))
    (move!)))
