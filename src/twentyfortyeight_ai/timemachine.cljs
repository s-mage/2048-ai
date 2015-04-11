(ns twentyfortyeight-ai.timemachine
  (:require [twentyfortyeight-ai.game :as g]
            [om.core :as om :include-macros true]))

(def app-history (atom [@g/board-state]))
(def app-future (atom []))

(defn forget-everything! []
  (reset! app-future [])
  (reset! app-history []))

(defn can-undo? []
  (> (count @app-history) 1))

(defn can-redo? []
  (> (count @app-future) 0))

(defn memorize! [new-state]
  (let [old-state (last @app-history)]
    (when-not (= old-state new-state)
      (swap! app-history conj new-state))))

(defn undo! []
  (when (can-undo?)
    (swap! app-future conj (last @app-history))
    (swap! app-history pop)
    (reset! g/board-state (last @app-history))))

(defn redo! []
  (when (can-redo?)
    (reset! g/board-state (last @app-future))
    (memorize! (last @app-future))
    (swap! app-future pop)))

(defn move! [direction]
  (g/move direction (om/root-cursor g/board-state))
  (memorize! @g/board-state))
