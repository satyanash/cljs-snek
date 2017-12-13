(ns modern-cljs.snek
  (:require [domina.core :refer [by-id value set-value! set-style!]]
            [domina.events :refer [get-listeners listen! unlisten!]]
            ))

(def movementDelta 1)
(def lengthDelta 1)
(def scoreDelta 1)
(def tickDelta 2)

(def UP "UP")
(def DOWN "DOWN")
(def LEFT "LEFT")
(def RIGHT "RIGHT")

(defn reverseDir [direction]
  (case direction
     UP DOWN
     DOWN UP
     LEFT RIGHT
     RIGHT LEFT
  )
)

(defn logState [state]
  (do 
    (-> "score" by-id .-innerHTML (set! (:score state)))
    (-> "length" by-id .-innerHTML (set! (:snake state)))
    (-> "snake" by-id .-innerHTML (set! (:snake state)))
    (-> "fruit" by-id .-innerHTML (set! (:fruit state)))
    (-> "tick" by-id .-innerHTML (set! (:tickDelay state)))
    (-> "state" by-id .-innerHTML (set! (:gameState state)))
  )
)

(defn snakeHead [state] (first (-> state :snake :cells)))

(defn snakeTail [state] (last (-> state :snake :cells)))

(defn randomPoint [width height]
  {:x (.floor js/Math (* (.random js/Math) width))
    :y (.floor js/Math (* (.random js/Math) height))})

(defn wrap [value limit]
  (cond 
    (< value 0) (+ value limit)
    (>= value limit) (- value limit)
    :else value))

(defn wrapPoint [point width height]
  {:x (wrap (get point :x) width)
   :y (wrap (get point :y) height)})

(defn clear [ctx]
  (do (set! (.-fillStyle ctx) "White") ; set the background color of the field
      (.fillRect ctx 0 0 (-> ctx .-canvas .-clientWidth) (-> ctx .-canvas .-clientHeight))))

(defn getNextPos [startPos direction]
  (case direction
    UP {:x (get startPos :x)
        :y (- (get startPos :y) movementDelta)}
    RIGHT {:x (+ (get startPos :x) movementDelta)
            :y (get startPos :y)}
    DOWN {:x (get startPos :x)
            :y (+ (get startPos :y) movementDelta)}
    LEFT {:x (- (get startPos :x) movementDelta)
          :y (get startPos :y)}
  )
)

(defn drawSnake [ctx state]
  (let [scaleFactor (-> state :field :scaleFactor )
        cells (-> state :snake :cells)]
    (set! (.-lineWidth ctx) (* (:girth (first cells)) (/ scaleFactor 5)))
    (loop [i 1]
      (when (< i (count cells))
        (let [cell (nth cells i)
              x (* (-> cell :pos :x) scaleFactor)
              y (* (-> cell :pos :y) scaleFactor)
              girth (* (:girth cell) scaleFactor)]
          (set! (.-fillStyle ctx) (if (== i 0) "Blue" "Red"))

          (cond 
            (== i 0) (.fillRect ctx x y girth girth)
            (== (mod i 3) 0) (case (:dir cell)
                              (UP DOWN) (.fillRect x y (/ girth 2) girth)
                              (LEFT RIGHT) (.fillRect x y girth (/ girth 2))
                           )
            (== (mod i 3) 1) (case (:dir cell)
                              (UP DOWN) (.fillRect ctx (+ x (* (girth 0.25))) y (/ girth 2) girth)
                              (LEFT RIGHT) (.fillRect ctx x (+ y (* (girth 0.25))) (/ girth 2) girth)
                           )
            :else (case (:dir cell)
                    (UP DOWN) (.fillRect ctx (+ x (/ girth 2)) y (/ girth 2) girth)
                    (LEFT RIGHT) (.fillRect ctx x (+ y (/ girth 2)) girth (/ girth 2)))
          )

          (set! (.-strokeStyle ctx) "White")
          (if i (.strokeRect ctx x y girth girth))
          (recur (inc i))
        )))))

(defn drawFruit [ctx state]
  (let [scaleFactor (-> state :field :scaleFactor)
        x (* (-> state :fruit :pos :x) scaleFactor)
        y (* (-> state :fruit :pos :y) scaleFactor)
	s (* (-> state :snake :cells first :girth) scaleFactor)]
    (do (set! (.-fillStyle ctx) "Green")
        (.fillRect ctx x y s s))))

(defn moveSnake [state movementDelta]
  (let [cells (-> state :snake :cells)]
    (loop [i (count cells)]
          ;for(var i=state.snake.cells.length-1; i >= 0; i--){
      (when (>= i 0) 
        (set! (:pos (nth cells i)) 
              (wrapPoint (getNextPos 
                           (:pos (nth cells i)) 
                           (:dir (nth cells i))) 
                         (-> state :field :width) 
                         (-> state :field :height)))

        (if (not= i 0) (set! (:dir (nth cells i)) (:dir (nth cells (dec i)))))
        (recur (dec i))))))

(defn getNewFruitPos [state]
  (let [cells (-> state :snake :cells)]
    (loop [collision (atom true)]
      (when @collision
          (let [newpoint (randomPoint (-> state :field :width) (-> state :field :height))]
            (reset! collision false)
            (loop [i 0]
              (when (< i (count cells))
                (let [pos (:pos (nth cells i))]
                  (if (not= newpoint pos) 
                    (recur (inc i))
                    (reset! collision true)
                  )
                )
              )
            )
            (if @collision
              (recur true)
              newpoint
            )
          )
      )
    )
  )
)

(defn initializeCtx [canvasId width height scaleFactor]
  (let [canvas (by-id canvasId)]
      (set! (.-width canvas) (* width scaleFactor))
      (set! (.-height canvas) (* height scaleFactor))
      (.getContext canvas "2d"))
)



(defn createInitialSnakeCells [startPos initialLength initialDirection]
  (let [cells (atom [{ :pos startPos :dir initialDirection :girth 1 }])]
    (dotimes [i initialLength]
      (let [nextPos (getNextPos (-> @cells last :pos) (reverseDir initialDirection))]
        (reset! cells (conj @cells { :pos nextPos :dir initialDirection :girth 1 }))
      )
    )
    @cells
    ;(reset! (:nextDir state) initialDirection)
  )
)


(defn turnSnake [state, movementDelta]
  (let [headDir (atom (get (snakeHead state) :dir))
        nextDir (.-nextDir state)]
    (if (and 
          (not= headDir nextDir)
          (not= headDir (reverseDir nextDir)))
      (reset! headDir (.-nextDir state))
            ;moveSnake(state, movementDelta);
    )
  )
)

(defn growSnake [state, lengthDelta]
  (let [tail (snakeTail state)
        cells (-> state .-snake .-cells)]
    (.push cells {
      :pos (getNextPos (.-pos tail), (reverseDir (.-dir tail)))
      :dir (.-dir tail)
      :girth 1
    })
  )
)

(defn startPos [width height]
  {:x (.floor js/Math (/ width 2))
   :y (.floor js/Math (/ height 2))}
)

(defn getInitialGameState [] 
  (let [defaultWidth 50
	defaultHeight 25
        startPos
        initialDirection RIGHT
        initialLength 5]
    (.log js/console "getInitialGameState called")
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
        :cells (createInitialSnakeCells (startPos defaultWidth defaultHeight) initialLength initialDirection)
      }
      :fruit {
        :pos (randomPoint defaultWidth  defaultHeight)
      }
      :nextDir initialDirection
    }
  )
)

(defn checkCollision [head cell]
 (and 
     (== (-> head :pos :x) (-> cell :pos :x))
     (== (-> head :pos :y) (-> cell :pos :y))))

(defn has-eaten-self? [state] ; check if we ate ourselves
  (let [head (snakeHead state) cells (-> state .-snake .-cells)]
     (loop [i 1]
       (when (< i (count cells))
       (if (checkCollision head (nth cells i))
         true
         (recur (inc i)))))))

(defn has-eaten-fruit? [state] ; check if we ate the fruit
  (let [head (snakeHead state)]
       (checkCollision head (.-fruit state))))

(defn tick [state]
  (let [head (snakeHead state)]
    (if (has-eaten-self? state)
      (do (set! (.-gameState state) "GAMEOVER")
          (.log js/console "GAMEOVER")))

    (if (= (.-gameState state) "RUNNING")
      (do
        (if (has-eaten-fruit? state)
          (do (set! (.-pos (.-fruit state)) (getNewFruitPos(state)))
              (growSnake state lengthDelta)
              (set! (.-score state) (+ (.-score state) scoreDelta)) ; increase points
              (set! (.-tickDelay state) (+ (.-tickDelay state) tickDelta)) ; speed up game a bit
              (.log js/console "FRUIT EATEN")
          )
        )
        (turnSnake state movementDelta)
        (moveSnake state movementDelta)
        (set! (.-tickId state) (.setTimeout js/window #(tick state) (.-tickDelay state))) ; schedule next call
      )
    )
    (logState state)
  )
)

(defn togglePause [state]
  (cond 
    (= (.-gameState state) "PAUSED")
    (do (set! (.-gameState state) "RUNNING")
        (set! (.-tickId state) (.setTimeout js/window #(tick state))) ;start the ticker
        false
    )
    (= (.-gameState state) "RUNNING")
    (do (set! (.-gameState state) "PAUSED")
                ;clearTimeout(state.tickId);
                ;state.tickId = null;
        true
    )
  )
)


(defn drawFrame [ctx state]
  (do (set! (.-animationId state) (.requestAnimationFrame js/window #(drawFrame ctx state)))
      (clear ctx)
      (drawSnake ctx state)
      (drawFruit ctx state)
  )
)

(defn pauseButtonHandler [state e]
        (if (togglePause state)
          (-> e .target (set-style! :border-style "inset"))
          (-> e .target (set-style! :border-style "outset"))))

(defn getAction [state e newGameFunction]
  (case (.-keyCode e)
    (37 72 65) LEFT  ; (Left h a) keys
    (38 75 87) UP    ; (Up k w) keys
    (39 76 68) RIGHT ; (Right l d) keys
    (40 74 83) DOWN  ; (Down j s) keys
    80 :pause ; p key
    82 :restart )) ; r key

(defn changeDir! [state nextDir]
  (swap! state #(assoc %1 :nextDir nextDir))
)

(defn startNewGame [state] 
        (if @state (do
                (.cancelAnimationFrame js/window (:animationId @state))
                (.clearTimeout js/window (:tickId @state))
        ))
        (swap! state getInitialGameState)
        (.log js/console "state second" @state )

        (let [ctx (initializeCtx "canvas" (-> state :field :width) (-> state :field :height) (-> state :field :scaleFactor))]
           (clear ctx)
           (do (reset! (:animationId state) (.requestAnimationFrame js/window #(drawFrame ctx state))) ;run the animation sequence
                (reset! (:tickId state) (.setTimeout js/window #(tick state))) ;start the ticker
           )

        (when (and js/document (.-getElementById js/document))
           (let [pauseToggleButton (by-id "pauseToggle")]
             (unlisten! pauseToggleButton :click)
             (listen! pauseToggleButton :click #(pauseButtonHandler state %1)))
           (let [keyDownHandler #(case (getAction %1)
                                   LEFT (changeDir! state LEFT)
                                   UP (changeDir! state UP)
                                   RIGHT (changeDir! state RIGHT)
                                   DOWN (changeDir! state DOWN)
                                   :pause (togglePause state)
                                   :restart (startNewGame state)
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
    (listen! newGameButton :click #(startNewGame state))))

