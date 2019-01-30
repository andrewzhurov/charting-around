(ns charting-around.stages.bets
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state stages >evt <sub]]
            [charting-around.css]
            [charting-around.chart :as chart]
            [charting-around.stage :as stage]
            ))

(defn driver [{:keys [id car-name driver-name avatar skill color]}]
  [:div.participant.card {:id id
                          :draggable true
                          :on-drag-start (fn [evt] (.setData (.-dataTransfer evt) "pt-id" id))

                          :style {:border-color color
                                  :border-style "solid"}}

   [:div.driver-name driver-name]
   [:div.car-name car-name]])

(defn bets-stage [state]
  (let [drag? (r/atom false)]
    (fn [state]
      (js/console.log "STATE:" state)
      (let [rs (<sub [:racers])
            pts (<sub [:participants])
            bets (<sub [:bets])]
        (js/console.log "PTS:" pts)
        (js/console.log "BETS:" bets)
        [:div#bets2
         (doall
          (for [place (range 1 (inc (count rs)))
                :let [in-display? (<= place (count pts))
                      {:keys [pt-id chance] :as bet} (get bets place)]]
            ^{:key place}
            [:div.row.collapsable {:class (when-not in-display? "collapse")}
             [:div.col.s12.m6
              [:div.card.blue-grey.darken-1 {:style {:width "fit-content"}}
               [:div.card-content.white-text
                [:span.card-title place "st place question"]
                [:p
                 (str "Who do you think finished the " place "st? Pellentesque tristique imperdiet tortor.  Pellentesque tristique imperdiet tortor.  Integer placerat tristique nisl.  ")
                 ]]
               [:div.card-action
                [:div.answers
                 (for [[id {:keys [driver-name in-select?]}] rs]
                   ^{:key id}
                   [:a.answer {:class (str (when-not in-select? " hidden")
                                           (when (= pt-id id) " betted"))
                               :on-click #(>evt [:toggle-bet {:place place
                                                              :pt-id id}])
                               :on-mouse-enter #(swap! logic/state assoc-in [:racers id :in-inspect?] true) ;; Bit a hack
                               :on-mouse-leave #(swap! logic/state assoc-in [:racers id :in-inspect?] false)} driver-name])]
                [:input.vanishable {:class (when-not bet "vanish")
                                    :style {:margin "0px"}
                                    :type "range"
                                    :value (or chance 80)
                                    :on-change #(>evt [:bet {:place place
                                                             :chance (int (.-value (.-target %)))}])}]]]]]))

         #_[:div (pr-str pt)]]
        #_[:div#bets.stage-content {:class (when @drag? "dragging")
                                  :on-drag-over (fn [evt] (.preventDefault evt))
                                  :on-drop done-drag!}
         [:div#drivers.drivers
          {:on-drop (fn [evt]
                      (let [pt-id (->> (.getData (.-dataTransfer evt) "pt-id")
                                       rest
                                       (apply str)
                                       keyword)]
                        (>evt [:cancel-bet pt-id])))
           :on-drag do-drag!
           :on-drag-over (fn [evt] (.preventDefault evt))}
          (for [[id dr] (remove (comp betted-pts key) pts)]
            ^{:key id}
            [driver dr])
          [:div.drop-overlay
           [:text "No bet"]]]

         ; (>evt [:place-bet place place pt-id 80])


         #_[:ul.bets
          (for [place (range 1 (inc (count pts)))
                :let [[bet-id {:keys [pt-id chance]}] (first (filter (fn [[_ {bet-place :place}]] (= bet-place place)) bets))
                      dr (get pts pt-id)]
                ]
            ^{:key place}
            [:li.bet.card {:id place
                           :on-drop (fn [evt]
                                      (let [pt-id (->> (.getData (.-dataTransfer evt) "pt-id")
                                                       rest
                                                       (apply str)
                                                       keyword)]
                                        (>evt [:place-bet place place pt-id 80])))
                           :on-drag do-drag!
                           :on-drag-over (fn [evt] (.preventDefault evt))
                           }
             [:div.place place]
             (when dr [driver dr])
             (when chance [:input.chance {:type :range
                                          :value chance
                                          :on-change #(>evt [:place-bet place place pt-id (int (.-value (.-target %)))])}])
             [:div.drop-overlay
              [:text (case place
                       1 "1st"
                       2 "2nd"
                       3 "3rd"
                       (str place "th"))]]])]
         [:button.btn.to-race {:on-click #(do (>evt [:to-stage :race]))} "SEE RACE"]]))))

(defmethod stage/stage :bets
  [state]
  [bets-stage state])

