(ns aoc.day21)

(def positions
  {:size           10
   :turn           0
   :current-player 1
   :number-players 2
   :scores         {}
   :positions      {1 10
                    2 3}})

(defn next-player [{:keys [current-player number-players] :as state}]
  (-> state
      (assoc :current-player
             (inc (mod current-player number-players)))
      (update :turn inc)))

(defn ended? [{:keys [scores]}]
  (->> scores
       vals
       (filter #(>= % 1000))
       first
       some?))

(defn pos-score [pos]
  (if (zero? pos) 10 pos))

(defn inc-score [{:keys [current-player] :as state}]
  (update-in state 
             [:scores current-player]
             (fnil + 0)
             (pos-score (get-in state [:positions current-player]))))

(defn move-player [by {:keys [current-player] :as state}]
  (update-in state
             [:positions current-player]
             (fn [pos amount]
               (mod (+ pos amount) (:size state)))
             by))

(comment
  (-> positions
      next-player
      next-player)
  (ended? positions)
  (move-player 1 6 positions)
  (->> positions
       (move-player 6)
       inc-score
       next-player))

(defn deterministic-die [last]
  (if last
    (inc (mod last 100))
    1))

(comment
  (deterministic-die nil)
  (deterministic-die 1)
  (deterministic-die 99)
  (deterministic-die 100)
  )

(defn play 
  ([state]
   (play state nil))
  ([state last-die]
   (let [roll1  (deterministic-die last-die)
         roll2  (deterministic-die roll1)
         roll3  (deterministic-die roll2)
         state' (->> state
                     (move-player (+ roll1 roll2 roll3))
                     inc-score
                     next-player)]
     (if (ended? state')
       state'
       (recur state' roll3)))))

(comment
  (println (play positions))
  (println (* 999 743)))

(def potential-rolles
  (frequencies (for [x (range 1 4)
                     y (range 1 4)
                     z (range 1 4)]
                 (+ x y z))))

(comment
  potential-rolles)

(defn winner
  "returns the winning player or nil"
  [{:keys [scores]}]
  (some->> scores
           (filter #(>= (val %) 21))
           first
           first))

(defn play2 [state]
  (if-let [win (winner state)]
    (case win
      1 [1 0]
      2 [0 1])
    (reduce-kv (fn [[w1 w2] roll quantity]
                 (let [state' (->> state
                                   (move-player roll)
                                   inc-score
                                   next-player)
                       [dw1 dw2] (play2 state')]
                   [(+ w1 (* quantity dw1))
                    (+ w2 (* quantity dw2))]))
               [0 0]
               potential-rolles)))

(comment
  (play2 positions))

