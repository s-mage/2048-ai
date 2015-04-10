(ns ^:figwheel-always twentyfortyeight-ai.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [dommy.core :as dommy]
            [twentyfortyeight-ai.ai :as ai]
            [twentyfortyeight-ai.game :as g]))

(enable-console-print!)

(def board-state (atom (g/init-board)))
(def message-state (atom {:msg nil}))

(def keycodes {37 :left ;; keycodes with vi mode!
               38 :up
               39 :right
               40 :down
               72 :left
               75 :up
               76 :right
               74 :down})

(def key-ch (chan))

(defn evt->dir [e]
  "Translates a key code to a potential direction keyword."
  (keycodes (.-which e)))

(defn key-listener [ch]
  "When a directional key has been pressed, we drop it into
  a channel so it can be processed by something else."
  (dommy/listen! js/window
                 :keyup
                 #(when-let [k (evt->dir %)]
                    (put! ch k)
                    (.preventDefault %))))

(dommy/listen! (. js/document (getElementById "move"))
                 :click
                 #(ai/move! board-state))

(dommy/listen! (. js/document (getElementById "moves"))
                 :click
                 #(dotimes [n 20] (ai/move! board-state)))

(key-listener key-ch)

(defn render-row [coll]
  "Creates an Om element representing a row.  A row contains 4 blocks."
  (apply dom/div #js {:className "row"} coll))

(defn render-block [x]
  "Creates an Om element representing a block within a row.  There are 16
  total blocks in the game."
  (dom/div #js {:className "block"}
           (dom/h2 #js {:className "number"} x)))

(defn message [app owner]
  "An Om component representing a message, either Winner or Game Over."
  (reify
    om/IRender
    (render [_]
      (dom/h1 #js {:display (if (:msg app)
                              "block"
                              "none")} (:msg app)))))

(defn score-board [app owner]
  "An Om component representing the score board."
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (str "Score: " (g/score app))))))

(defn game-board [app owner]
  "An Om component representing the game board."
  (reify
    om/IWillMount
    (will-mount [_]
      (let [keypresses key-ch]
        (go (loop []
              (let [direction (<! keypresses)]
                (g/move direction app)
                (recur))))))
    om/IWillUpdate
    (will-update [this next-state next-props]
      (when (g/winner? next-state) (swap! message-state assoc :msg "Winner!"))
      (when (g/loser? next-state) (swap! message-state assoc :msg "Loser!")))
    om/IRender
    (render [this]
      (let [rows (partition 4 (map render-block app))]
        (apply dom/div
               nil
               (map #(render-row %) rows))))))

; Attach to DOM
;

(om/root score-board
         board-state
         {:target (. js/document (getElementById "score"))})

(om/root message
         message-state
         {:target (. js/document (getElementById "message"))})

(om/root game-board
         board-state
         {:target (. js/document (getElementById "game"))})
