(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state >evt <sub]]
            [charting-around.chart :as chart]
            [charting-around.stages.participants]
            [charting-around.stages.bets]
            ))

(enable-console-print!)

(defn l
  ([desc expr]
   (js/console.log desc expr)
   expr))

(defmulti results :results-presentation)
(defmethod results :wave [state]
  (let [field-y 400
        history (<sub [:balance-history])

        [domain-min domain-max] (->> history
                                     (map :balance)
                                     sort
                                     ((juxt first last)))
        domain-min (- domain-min 20)
        domain-max (max 100 (+ domain-max 30))
        ->r (fn [d-val] (- field-y (* (/ field-y (- domain-max domain-min)) (- d-val domain-min))))]
    [:div#results
     [:svg {:width "100%"
            :height field-y}
      [:line.axis.x {:x1 0 :y1 field-y
                     :x2 1000 :y2 field-y}]
      [:line.axis.y {:x1 0 :y1 field-y
                     :x2 0 :y2 0}]
      [:line {:x1 0 :y1 (->r 0)
              :x2 1000 :y2 (->r 0)
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

(defmethod results :stacks [state]
  (let [content-height 228
        full-height 270
        stack-width 20
        ->r (fn [delta] (-> (/ content-height 100) (* (js/Math.abs delta))))
        bets-results (<sub [:bets-results])
        deltas (map val bets-results)
        pos-val (->> deltas
                     (filter pos?)
                     (reduce +))
        pos-r (->r pos-val)
        neg-val (->> deltas
                     (filter neg?)
                     (reduce +))
        neg-r (->r neg-val)]
    ^{:key [pos-val neg-val]}
    [:div
     [:div.stacks
      [:div.pos
       (for [[idx delta] (sort-by key (<sub [:bets-results]))
             :let [p? (pos? delta)
                   h (->r delta)]]

         ^{:key idx}
         [:div.stack {:class (when-not p? "space")
                      :style {:height h
                              :margin-bottom (- full-height h)}}])
       [:div.val pos-val]]

      [:div.neg
       (for [[idx delta] (sort-by key (<sub [:bets-results]))
             :let [n? (neg? delta)
                   h (->r delta)]]

         ^{:key idx}
         [:div.stack {:class (when-not n? "space")
                      :style {:height h
                              :margin-bottom (- full-height h)}}])
       [:div.val neg-val]]
      ]
     [:div.rank (pr-str (<sub [:rank]))]
     #_(let [mx 50
           my 50
           rx 10
           ry 20
           x 100
           y 100]
       [:svg {:height "400px"}
        [:circle {:cx mx :cy my :r 2}]
        [:circle {:cx (+ mx rx) :cy (+ my ry) :r 2}]
        [:circle {:cx x  :cy y  :r 2}]
        [:path {:d (gstr/format
                    "M%s %s
                  A %s %s 0 1 0 %s %s"
                    mx my rx ry x y)
                                        ;              L 110 215
                                        ;              A 36 60 0 0 1 150.71 170.29
                                        ;              L 172.55 152.45
                                        ;              A 30 50 -45 0 1 215.1 109.9
                                        ;              L 315 10
                :stroke "black"
                :fill "green"
                :stroke-width "2"
                :fill-opacity "0.5"}]])[:div "BLAH"]]))

(defn dev-checkpoint []
  [:div {:style {:position "absolute"
                 :top "10px"
                 :left "10px"
                 :z-index 10}}
   [:button.btn.checkpoint    {:on-click #(>evt [:make-checkpoint])} "SAVE"]
   [:button.btn.checkpoint-to {:on-click #(>evt [:to-checkpoint])}   "LOAD"]])

(defn root []
  [:div#root
   [dev-checkpoint]
   [:div.layout
    [charting-around.stages.participants/participants-stage @logic/state]
    [charting-around.stages.bets/bets-stage @logic/state]
    [:div.results
     [:div.results-presentation-picker
      [:button.btn {:on-click #(>evt [:set-results-presentation :wave])} "WAVE"]
      [:button.btn {:on-click #(>evt [:set-results-presentation :stacks])} "STACKS"]]
     [results @logic/state]]
    ]
   ])


(r/render [root]
          (document.getElementById "app"))


