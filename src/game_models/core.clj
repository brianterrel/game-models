(ns game-models.core
  (:gen-class))

;;The game model contains a vector of global bonuses which modify profit for all game activities,
;;and a vector of generators which provide the primary income stream for the players

(def game-model (atom {:global-bonuses []
                      :gen-num 5
                      :generators [{:start-cost 4,
                                    :cost-mult 1.07,
                                    :base-income 1.67,
                                    :bonuses {25 3, 50 2, 100 10, 200 3, 300 2, 400 2, 500 2, 600 2, 1000 2}}
                                   {:start-cost 60,
                                    :cost-mult 1.15,
                                    :base-income 20,
                                    :bonuses {25 5, 50 10, 100 3, 200 2, 300 2, 400 2, 500 2, 600 2, 1000 2}}
                                   {:start-cost 720,
                                    :cost-mult 1.14,
                                    :base-income 90,
                                    :bonuses {25 4, 50 8, 100 4, 200 2, 300 2, 400 2, 500 2, 600 2, 1000 2}}
                                   {:start-cost 8640,
                                    :cost-mult 1.13,
                                    :base-income 360,
                                    :bonuses {25 2, 50 2, 100 3, 200 2, 300 2, 400 2, 500 2, 600 2, 1000 2}}
                                   {:start-cost 10368,
                                    :cost-mult 1.12,
                                    :base-income 2160,
                                    :bonuses {25 1.5, 50 2, 100 2, 200 2, 300 2, 400 2, 500 2, 600 2, 1000 2}}]
                      }))

;;A generator is represented as a map with the following form:
;; {:start-cost int
;;  :cost-mult  float
;;  :base-income
;;  :bonuses {(int purchase-num) (float multiplier), ... }}


;;The game state atom contains a vector of the successive states of the game as purchase decisions are made
;;Each state contains: the purchase number, current level, active bonuses, and income for each generator; the time
;;elapsed in the game so far; the current player income per second;

;; Lets flesh that out a bit:
;; {:purchase-num some-int
;;  :time-elapsed some-int
;;  :player-income some-float
;;  :gen-states vector-of-gen-states}
;;
;; A generator state looks like:
;; {:level some-int
;;  :current-bonus some-int
;;  :next-cost}



(def game-state (atom [{:purchases 1
                        :income 1.67
                        :time-elapsed 0.0
                        :gen-states [{:level 1,
                                      :bonus 1,
                                      :income 1.67
                                      :next-cost 4.6}
                                     {:level 0,
                                      :bonus 1,
                                      :income 0
                                      :next-cost 69.0}
                                     {:level 0,
                                      :bonus 1,
                                      :income 0
                                      :next-cost 820.8}
                                     {:level 0,
                                      :bonus 1,
                                      :income 0
                                      :next-cost 9763.2}
                                     {:level 0,
                                      :bonus 1,
                                      :income 0
                                      :next-cost 116121.6}]}]))

;; score = cost/current-income + cost/(base-income * n+1bonus)

(defn choose-next-gen [prev-state model]
  (first
    (apply
      min-key #(last %)
      (for [n (range (:gen-num @model))
            :let [score (+
                          (/
                            (:next-cost ((:gen-states prev-state) n))
                            (:income prev-state))
                          (/
                            (:next-cost ((:gen-states prev-state) n))
                            (*
                              (:base-income ((:generators @model) n))
                              (if
                                (contains? (:bonuses ((:generators @model) n)) (inc (:level ((:gen-states prev-state) n))))
                                (* (:bonus ((:gen-states prev-state) n)) ((inc (:level ((:gen-states prev-state) n))) (:bonuses ((:generators @model) n))))
                                (:bonus ((:gen-states prev-state) n))))))]]
      [n score]))))




(defn make-new-gen-state [prev-state model choice]
  "Produce a new generator state for the generator which is purchased in a given round"
  (let [new-level (inc (:level ((:gen-states prev-state) choice)))
        bonus-map (:bonuses ((:generators @model) choice))
        old-bonus (:bonus ((:gen-states prev-state) choice))
        old-cost (:next-cost ((:gen-states prev-state) choice))
        new-bonus (if (contains? bonus-map new-level) (* old-bonus (get bonus-map new-level)) old-bonus)
        cost-multiplier (:cost-mult ((:generators @model) choice))
        base-income (:base-income ((:generators @model) choice))]
    (hash-map
      :level new-level
      :bonus (if (contains? bonus-map new-level) (* old-bonus (get bonus-map new-level)) old-bonus)
      :next-cost (* old-cost cost-multiplier)
      :income (* base-income new-level new-bonus))))


(defn make-new-game-state [prev-state model]
  (let [choice (choose-next-gen prev-state model)
        new-purchase-num (inc (:purchases prev-state))
        new-gen-states (vec (for [n (range (:gen-num @model))]
                         (if (not= n choice) ((:gen-states prev-state) n) (make-new-gen-state prev-state model n))))
        new-income (reduce + (for [n (range (:gen-num @model))] (:income (new-gen-states n))))]
    (swap!
      game-state
      conj
      (hash-map
        :purchases new-purchase-num
        :income new-income
        :time-elapsed (+ (:time-elapsed prev-state)(/ (:next-cost ((:gen-states prev-state) choice)) (:income prev-state)))
        :gen-states new-gen-states))))

(make-new-game-state (last @game-state) game-model)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

