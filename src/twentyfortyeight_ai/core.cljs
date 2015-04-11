(ns ^:figwheel-always twentyfortyeight-ai.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [dommy.core :as dommy :refer-macros [sel1]]
            [twentyfortyeight-ai.ai :as ai]
            [twentyfortyeight-ai.timemachine :as tm]
            [twentyfortyeight-ai.game :as g]))

(enable-console-print!)

(def auto-state (atom nil))

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

(dommy/listen! (sel1 :#move)
                 :click
                 #(ai/move! g/board-state))

(dommy/listen! (sel1 :#moves)
                 :click
                 #(dotimes [n 20] (ai/move! g/board-state)))

(defn kill-auto-game []
  (js/clearInterval @auto-state)
  (reset! auto-state nil))

(defn start-auto-game []
  (reset! auto-state (js/setInterval #(ai/move! g/board-state) 1500))) 

(defn auto-game-callback []
  (if (.-checked (sel1 :#auto)) (kill-auto-game))
  (if @auto-state (kill-auto-game) (start-auto-game)))

(dommy/listen! (sel1 "#auto")
                 :change
                 auto-game-callback)

(dommy/listen! (sel1 "#undo")
                 :click
                 tm/undo!)

(dommy/listen! (sel1 "#redo")
                 :click
                 tm/redo!)

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
      (dom/h1 #js {:display (if (g/msg app)
                              "block"
                              "none")} (g/msg app)))))

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
                (tm/move! direction)
                (recur))))))
    om/IRender
    (render [this]
      (let [rows (partition 4 (map render-block app))]
        (apply dom/div
               nil
               (map #(render-row %) rows))))))

; Attach to DOM
;
(om/root score-board g/board-state {:target (sel1 :#score)})
(om/root message     g/board-state {:target (sel1 :#message)})
(om/root game-board  g/board-state {:target (sel1 :#game)})
