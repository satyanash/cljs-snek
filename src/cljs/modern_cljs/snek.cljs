(ns modern-cljs.snek
  (:require [domina.core :refer [by-id value set-value! set-style!]]
            [domina.events :refer [get-listeners listen! unlisten!]]
            ))

(def movementDelta 1)
(def lengthDelta 1)
(def scoreDelta 1)
(def tickDelta 2)

(defn reverser-dir
  "returns the reverse of the given direction"
  [direction]
  (case direction
     :UP :DOWN
     :DOWN :UP
     :LEFT :RIGHT
     :RIGHT :LEFT
  )
)

(defn log-state
  "logs the current gamestate to the HUD"
  [state]
  (do
    (-> "score" by-id .-innerHTML (set! (:score state)))
    (-> "length" by-id .-innerHTML (set! (-> state :snake :cells count)))
    (-> "snake" by-id .-innerHTML (set! (-> state :snake :cells first :pos)))
    (-> "direction" by-id .-innerHTML (set! (-> state :snake :cells first :dir)))
    (-> "nextDir" by-id .-innerHTML (set! (-> state :nextDir)))
    (-> "fruit" by-id .-innerHTML (set! (-> state :fruit :pos)))
    (-> "tick" by-id .-innerHTML (set! (:tickDelay state)))
    (-> "state" by-id .-innerHTML (set! (:gameState state)))
    (-> "animationId" by-id .-innerHTML (set! (:animationId state)))
    (-> "tickId" by-id .-innerHTML (set! (:tickId state)))
  )
)

(defn snake-head "returns the head cell of the snake" [state] (first (-> state :snake :cells)))

(defn snake-tail "returns the tail cell of the snake" [state] (last (-> state :snake :cells)))

(defn random-point
  "returns a random point within the given dimensions"
  [width height]
  {:x (.floor js/Math (* (.random js/Math) width))
    :y (.floor js/Math (* (.random js/Math) height))})

(defn wrap
  "returns a wrapped value within the limit, multiple times if needed"
  [value limit]
  (cond
    (< value 0) (+ value limit)
    (>= value limit) (- value limit)
    :else value))

(defn wrap-point
  "returns a wrapped version of the given point based on given dimensions"
  [point width height]
  {:x (wrap (get point :x) width)
   :y (wrap (get point :y) height)})

(defn clear-ctx
  "clears the canvas represented by the given context"
  [ctx]
  (do
    ;(.log js/console "CONTEXT CLEARED")
    (set! (.-fillStyle ctx) "White") ; set the background color of the field
      (.fillRect ctx 0 0 (-> ctx .-canvas .-clientWidth) (-> ctx .-canvas .-clientHeight))))

(defn get-next-pos
  "returns the next position on the basis of the given position and direction"
  [startPos direction]
  (case direction
    :UP {:x (get startPos :x)
        :y (- (get startPos :y) movementDelta)}
    :RIGHT {:x (+ (get startPos :x) movementDelta)
            :y (get startPos :y)}
    :DOWN {:x (get startPos :x)
            :y (+ (get startPos :y) movementDelta)}
    :LEFT {:x (- (get startPos :x) movementDelta)
          :y (get startPos :y)}
  )
)

(defn draw-snake
  "draws the snake onto the given canvas context"
  [ctx state]
  (let [scaleFactor (-> state :field :scaleFactor )
        cells (-> state :snake :cells)]
    (set! (.-lineWidth ctx) (* (:girth (first cells)) (/ scaleFactor 5)))
    (dotimes [i (count cells)]
        (let [cell (nth cells i)
              nextCell (if (= (inc i) (count cells)) nil (nth cells (inc i)))
              x (* (-> cell :pos :x) scaleFactor)
              y (* (-> cell :pos :y) scaleFactor)
              girth (* (:girth cell) scaleFactor)]
          (set! (.-fillStyle ctx) (if (zero? i) "Blue" "Red"))

          (cond
            (zero? i) (.fillRect ctx x y girth girth)
            (or
              (not nextCell)
              (= (:dir cell) (:dir nextCell)))
               (cond
                  (== (mod i 6) 0) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx x y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x y girth (/ girth 2)))
                  (== (mod i 6) 1) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx (+ x (* girth 0.25)) y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x (+ y (* girth 0.25)) girth (/ girth 2)))
                  (== (mod i 6) 2) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx (+ x (/ girth 2)) y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x (+ y (/ girth 2)) girth (/ girth 2)))
                  (== (mod i 6) 3) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx (+ x (* girth 0.25)) y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x (+ y (* girth 0.25)) girth (/ girth 2)))
                  (== (mod i 6) 4) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx x y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x y girth (/ girth 2)))
                  (== (mod i 6) 5) (case (:dir cell)
                                    (:UP :DOWN) (.fillRect ctx (+ x (* girth 0.25)) y (/ girth 2) girth)
                                    (:LEFT :RIGHT) (.fillRect ctx x (+ y (* girth 0.25)) girth (/ girth 2)))
                )
            :else (do
                    (.beginPath ctx)
                    (cond
                      (or (and (= (:dir nextCell) :LEFT) (= (:dir cell) :DOWN)) (and (= (:dir nextCell) :UP) (= (:dir cell) :RIGHT)))
                      (do (.moveTo ctx (+ x girth) y)
                          (.lineTo ctx x (+ y girth))
                          (.lineTo ctx (+ x girth) (+ y girth))
                          (.lineTo ctx (+ x girth) y))
                      (or (and (= (:dir nextCell) :DOWN) (= (:dir cell) :LEFT)) (and (= (:dir nextCell) :RIGHT) (= (:dir cell) :UP)))
                      (do (.moveTo ctx x y)
                          (.lineTo ctx (+ x girth) y)
                          (.lineTo ctx x (+ y girth))
                          (.lineTo ctx x y))
                      (or (and (= (:dir nextCell) :DOWN) (= (:dir cell) :RIGHT)) (and (= (:dir nextCell) :LEFT) (= (:dir cell) :UP)))
                      (do (.moveTo ctx x y)
                          (.lineTo ctx (+ x girth) y)
                          (.lineTo ctx (+ x girth) (+ y girth))
                          (.lineTo ctx x y))
                      (or (and (= (:dir nextCell) :UP) (= (:dir cell) :LEFT)) (and (= (:dir nextCell) :RIGHT) (= (:dir cell) :DOWN)))
                      (do (.moveTo ctx x y)
                          (.lineTo ctx (+ x girth) (+ y girth))
                          (.lineTo ctx x (+ y girth))
                          (.lineTo ctx x y))
                    )
                    (.fill ctx))
          )

          (set! (.-strokeStyle ctx) "White")
          (if i (.strokeRect ctx x y girth girth))
        ))))


(defn draw-fruit
  "draws the fruit onto the given canvas context"
  [ctx state]
  (let [scaleFactor (-> state :field :scaleFactor)
        x (* (-> state :fruit :pos :x) scaleFactor)
        y (* (-> state :fruit :pos :y) scaleFactor)
	s (* (-> state :snake :cells first :girth) scaleFactor)]
    (do (set! (.-fillStyle ctx) "Green")
        ;(.log js/console "x " x " y " y " s " s " scaleFactor" scaleFactor " cells first" (-> state :snake :cells first .toString))
        (.fillRect ctx x y s s))))


(defn move-snake
  "returns a vector of cells representing the snake with all cells moved one cell in the corresponding direction"
  [state]
  (let [c (-> state :snake :cells)]
    (loop [cells c
           lastDir (-> c first :dir)
           newcells []]
      (if (empty? cells)
        newcells
        (recur (rest cells)
               (-> cells first :dir)
               (conj newcells {:pos (wrap-point
                                       (get-next-pos (:pos (first cells)) (:dir (first cells)))
                                       (-> state :field :width) 
                                       (-> state :field :height))
                               :dir lastDir
                               :girth (:girth (first cells))}))))))


(defn check-collision
  "returns true if cell1 has the same position as cell2, false otherwise"
  [cell1 cell2]
  (do
    ;(.log js/console "cell1" (:x (:pos cell1)) (:y (:pos cell1)) "cell2" (:x (:pos cell2)) (:y (:pos cell2)))
 (and
     (== (-> cell1 :pos :x) (-> cell2 :pos :x))
     (== (-> cell1 :pos :y) (-> cell2 :pos :y)))))

(defn get-new-fruit-pos
  "returns a new position for the fruit"
  [state]
  (let [cells (-> state :snake :cells)]
    (loop []
      (let [newpoint (random-point (-> state :field :width) (-> state :field :height))]
        (if (not-any? #(check-collision %1 newpoint) cells)
          newpoint
          (recur)
        )
      )
    )
  )
)

(defn init-ctx
  "resizes a canvas with the given ID and the given dimensions and scaling and returns a 2d context"
  [canvasId width height scaleFactor]
  (let [canvas (by-id canvasId)]
      (set! (.-width canvas) (* width scaleFactor))
      (set! (.-height canvas) (* height scaleFactor))
      (.getContext canvas "2d"))
)



(defn create-init-snake-cells
  "returns a vector of length initialLength representing the snake with it's head at startPos and pointing at initialDirection"
  [startPos initialLength initialDirection]
  (loop [x [] lastPos startPos]
    (if (< (count x) initialLength)
      (let [nextPos (get-next-pos lastPos (reverser-dir initialDirection))]
        (recur (conj x {:pos nextPos :dir initialDirection :girth 1}) nextPos))
      x
    )
  )
)

(defn turn-snake
  "returns a map representing the snake with the changed direction read from nextDir"
  [state]
  (let [headDir (-> state snake-head :dir)
        nextDir (:nextDir state)]
    ;(.log js/console "turn-snake called, nextDir: " nextDir " ---  reverse: " (reverser-dir nextDir) " ---- headDir: " headDir)
    (if (and 
          (not= headDir nextDir)
          (not= headDir (reverser-dir nextDir)))
      (assoc-in (:snake state) [:cells 0 :dir] nextDir)
      (:snake state)
            ;move-snake(state, movementDelta);
    )
  )
)

(defn grow-snake
  "returns a vector of cells with a new cell added to the end. Direction of new cell is the same as the last cell before adding."
  [state]
  (let [tail (snake-tail state)
        cells (-> state :snake :cells)]
    (conj cells {
      :pos (get-next-pos (:pos tail) (reverser-dir (:dir tail)))
      :dir (:dir tail)
      :girth 1
    })
  )
)

(defn start-pos
  "returns a starting point calculated from the width and height of a game field"
  [width height]
  {:x (.floor js/Math (/ width 2))
   :y (.floor js/Math (/ height 2))}
)

(defn init-game-state
  "returns a map representing the initial game state"
  [] 
  (let [defaultWidth 50
	defaultHeight 25
        initialDirection :RIGHT
        initialLength 5]
    (.log js/console "init-game-state called")
    {
      :gameState "RUNNING"
      :tickDelay 100
      :field {
        :scaleFactor 25
        :width defaultWidth
        :height defaultHeight
      }
      :score 0
      :snake {
        :cells (create-init-snake-cells (start-pos defaultWidth defaultHeight) initialLength initialDirection)
      }
      :fruit {
        :pos (random-point defaultWidth  defaultHeight)
      }
      :nextDir initialDirection
    }
  )
)

(defn has-eaten-self?
  "returns true if the snake's head is the same cell as any of the other cells of the snake's body, false otherwise"
  [state]
  (let [head (snake-head state) 
        cells (-> state :snake :cells)]
     (loop [i 1]
       (when (< i (count cells))
       (if (check-collision head (nth cells i))
         true
         (recur (inc i)))))))

(defn has-eaten-fruit?
  "returns true if the snake's head is the same cell as the current fruit cell, false otherwise"
  [state]
  (let [head (snake-head state)]
    (do 
      ;(.log js/console "checking if fruit is eaten for" (:x (:pos head)) (:y (:pos head)) " ---  fruit: " (:x (:pos (:fruit state))) (:y (:pos (:fruit state))))
       (check-collision head (:fruit state)))))

(defn tick
  "processes the current game state and mutates the state atom accordingly"
  [state]
  (let [head (snake-head state)]
    (if (has-eaten-self? @state)
      (do (swap! state assoc :gameState "GAMEOVER")
          (.play (new js/Audio "audio/death.wav"))
          (.log js/console "GAMEOVER")))

    (if (= (:gameState @state) "RUNNING")
      (do
        (if (has-eaten-fruit? @state)
          (do (swap! state assoc-in [:fruit :pos] (get-new-fruit-pos @state))
              (swap! state assoc-in [:snake :cells] (grow-snake @state))
              (swap! state assoc :score (+ (:score @state) scoreDelta)) ; increase points
              (swap! state assoc :tickDelay (- (:tickDelay @state) tickDelta)) ; speed up game a bit
              ;(.log js/console "FRUIT EATEN")
              (.play (new js/Audio (rand-nth ["audio/crunch1.wav" "audio/crunch2.wav" "audio/crunch3.wav" "audio/crunch4.wav"])))
          )
        )
        (swap! state assoc :snake (turn-snake @state))
        (swap! state assoc-in [:snake :cells] (move-snake @state))
        (swap! state assoc :tickId (.setTimeout js/window #(tick state) (:tickDelay @state))) ; schedule next call
      )
    )
    (log-state @state)
  )
)

(defn toggle-pause [state]
  "toggles the game state between paused and running"
  (case (:gameState @state)
    "PAUSED" (do (swap! state assoc
                             :gameState "RUNNING"
                             :tickId (.setTimeout js/window #(tick state)))
        false
    )
    "RUNNING" (do (swap! state assoc :gameState "PAUSED")
                (.clearTimeout js/window (:tickId state))
                ;state.tickId = null;
        true
    )
  )
)

(defn draw-frame [ctx state]
  (do (swap! state assoc :animationId (.requestAnimationFrame js/window #(draw-frame ctx state)))
      (clear-ctx ctx)
      (draw-snake ctx @state)
      (draw-fruit ctx @state)
  )
)

(defn pause-button-handler [state e]
        (if (toggle-pause state)
          (-> e :target (set-style! :border-style "inset"))
          (-> e :target (set-style! :border-style "outset"))))

(defn get-action [e]
  "returns the action performed based on the keyCode of the given event
   returns :LEFT, :RIGHT:, :DOWN, :UP for the corresponding keys
   returns :pause for the pause game action
   returns :restart for the restart game action
  "
  (do ;(.log js/console "event" e " and " (.-keyCode e))
  (case (:keyCode e)
    (37 72 65) :LEFT  ; (Left h a) keys
    (38 75 87) :UP    ; (Up k w) keys
    (39 76 68) :RIGHT ; (Right l d) keys
    (40 74 83) :DOWN  ; (Down j s) keys
    80 :pause ; p key
    82 :restart ; r key
    nil))) 

(defn change-dir [state dir]
  "returns the state with nextDir set to given direction"
  (do 
    ;(.log js/console "Changing dir to" dir)
  (assoc state :nextDir dir))
)

(defn start-new-game! [state]
        (if @state (do
                (.cancelAnimationFrame js/window (:animationId @state))
                (.clearTimeout js/window (:tickId @state))
        ))
        (swap! state init-game-state)
        (.log js/console "state second" (.toString @state))

        (let [ctx (init-ctx "canvas" (-> @state :field :width) (-> @state :field :height) (-> @state :field :scaleFactor))]
           (clear-ctx ctx)
           (do (swap! state assoc :animationId (.requestAnimationFrame js/window #(draw-frame ctx state))) ;run the animation sequence
                (swap! state assoc :tickId (.setTimeout js/window #(tick state))) ;start the ticker
           )

        (when (and js/document (.-getElementById js/document))
           (let [pauseToggleButton (by-id "pauseToggle")]
             (unlisten! pauseToggleButton :click)
             (listen! pauseToggleButton :click #(pause-button-handler state %1)))
           (let [keyDownHandler #(case (get-action %1)
                                   :LEFT (swap! state change-dir :LEFT)
                                   :UP (swap! state change-dir :UP)
                                   :RIGHT (swap! state change-dir :RIGHT)
                                   :DOWN (swap! state change-dir :DOWN)
                                   :pause (toggle-pause state)
                                   :restart (start-new-game! state)
                                   nil 
                                   )]
             (unlisten! :keydown)
             (listen! :keydown keyDownHandler)
           ))
        )
)

(defn ^:export init []
  (let [state (atom nil)
        newGameButton (by-id "newGameButton")]
    (.log js/console "game loaded")
    (unlisten! newGameButton :click)
    (listen! newGameButton :click #(start-new-game! state))))

