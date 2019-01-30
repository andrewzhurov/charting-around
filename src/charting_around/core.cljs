(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state >evt <sub]]
            [charting-around.css]
            [charting-around.chart :as chart]
            [charting-around.stages.participants]
            [charting-around.stages.bets]
            ))

(enable-console-print!)

(defn l
  ([desc expr]
   (js/console.log desc expr)
   expr))

(defn results [state]
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

(defn dev-checkpoint []
  [:div {:style {:position "absolute"
                 :top "10px"
                 :left "10px"
                 :z-index 10}}
   [:button.btn.checkpoint    {:on-click #(>evt [:make-checkpoint])} "SAVE"]
   [:button.btn.checkpoint-to {:on-click #(>evt [:to-checkpoint])}   "LOAD"]])

(defn root []
  [:div#root
   [:style (garden.core/css charting-around.css/styles)]
   [:link {:rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"}]
   [:link {:rel "stylesheet"
           :href "https://fonts.googleapis.com/icon?family=Material+Icons"}]

   [dev-checkpoint]
   [:div.layout
    [charting-around.stages.participants/participants-stage @logic/state]
    [charting-around.stages.bets/bets-stage @logic/state]
    [results @logic/state]]
   ])


(r/render [root]
          (document.getElementById "app"))


