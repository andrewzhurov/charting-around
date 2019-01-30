(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state stages >evt <sub]]
            [charting-around.css]
            [charting-around.chart :as chart]
            [charting-around.stage :as stage]
            [charting-around.stages.participants]
            [charting-around.stages.bets]
            ))

(enable-console-print!)
(defn icon [icon-name] [:i.material-icon icon-name])

(defn l
  ([desc expr]
   (js/console.log desc expr)
   expr))

#_(def spec (r/atom {:range [800 300]
                   :axes [{:domain [2012 2020]
                           :scale scale-linear
                           :tick 1
                           ;:desc "Year"
                           :val-path [:production-year]}
                          {:domain [0 300]
                           :scale scale-linear
                           :tick 30
                           ;:desc "Top speed (km/h)"
                           :desc-side :right
                           :val-path [:top-speed]
                           }]
                   }))



(defn race-stage [state]
  (let [field-x 600
        field-y 400
        history (<sub [:balance-history])

        [domain-min domain-max] (->> history
                                     (map :balance)
                                     sort
                                     ((juxt first last)))
        domain-min (- domain-min 20)
        domain-max (max 100 (+ domain-max 30))
        ->r (fn [d-val] (- field-y (* (/ field-y (- domain-max domain-min)) (- d-val domain-min))))]
    [:div#results
     [:svg {:width field-x
            :height field-y}
      [:line.axis.x {:x1 0 :y1 field-y
                     :x2 field-x :y2 field-y}]
      [:line.axis.y {:x1 0 :y1 field-y
                     :x2 0 :y2 0}]
      [:line {:x1 0 :y1 (->r 0)
              :x2 field-x :y2 (->r 0)
              :stroke "gray"}]

      [:path {:d (reduce (fn [path{:keys [idx balance]}]
                           (let [x (* 30 idx)
                                 y (int (->r balance))]
                             (str path " T " x " " y)))
                         (str "M0 " (int (->r 0)) " " "Q 0 " (int (->r 0)) ", 30 " (->r (:balance (second history))))
                         (rest (rest history)))
              :fill "none"
              :stroke "blue"}]
      (for [{:keys [idx balance]} history
            :let [r (->r balance)]]
        ^{:key idx}
        [:circle.balance {:cx (* 30 idx) :cy r}])]]))

(defmethod stage/stage :race
  [state]
  [:div#race.stage-content "RACE PROGRESS"
   [race-stage state]
   ])

(defmethod stage/stage :results
  [state]
  [:div#results.stage-content "RESULTS"
   ])

(defmethod stage/stage :welcome
  [state]
  [:div#welcome.stage-content
   "Welcome to a death race betting interface!"
   [:button.btn {:on-click #(>evt [:new-race])} "Proceed"]
   ])

(defn root []
  [:div#root
   [:style (garden.core/css charting-around.css/styles)]
   [:link {:rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"}]
   [:link {:rel "stylesheet"
           :href "https://fonts.googleapis.com/icon?family=Material+Icons"}]

   [:button.btn.checkpoint {:on-click #(>evt [:make-checkpoint])
                            :style {:position "absolute"
                                    :top "10px"
                                    :left "10px"
                                    :z-index 10}} "MAKE"]
   [:button.btn.checkpoint-to {:on-click #(>evt [:to-checkpoint])
                               :style {:position "absolute"
                                       :top "10px"
                                       :left "100px"
                                       :z-index 10}} "TO"]
   [:div.layout
    [charting-around.stages.participants/participants-stage @logic/state]
    [charting-around.stages.bets/bets-stage @logic/state]
    [race-stage @logic/state]]
   #_[stage/stage @state]
   ])


(r/render [root]
          (document.getElementById "app"))


