(ns charting-around.stages.participants
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.chart :as chart]
            [charting-around.logic :refer [>evt <sub] :as logic]
            ))

(defn l [desc expr] (js/console.log desc expr) expr)

(defn criteria-list [{:keys [criteria]
                      {{:keys [display inspect]} :criteria} :api :as spec}] ;; TODO
  [:div.criteria-list
   (for [[val-path {:keys [in-display?]}] criteria]
     ^{:key val-path}
     [:div.criteria {:class (when in-display? "in-display")
                     :on-mouse-enter #(inspect val-path true)
                     :on-mouse-leave #(inspect val-path false)

                     :on-click #(display val-path)
                     }
      (pr-str val-path)]
     )])

(defn participants-list [{:keys [data]
                          {{:keys [inspect select]} :data} :api :as spec}] ;; TODO
  [:div.participants-list
   (doall
    (for [{:keys [id driver-name car-name color in-select? in-inspect?]} (vals data)]
      ^{:key id}
      [:div.participant.card {:class (str (when in-select? "in-select ")
                                          (when in-inspect? "in-inspect "))
                              :on-click #(select id)
                              :on-mouse-enter #(inspect id true)
                              :on-mouse-leave #(inspect id false)
                              :style {:border-color color
                                      :border-style "solid"}}
       [:div.background {:style {:background-color color}}]
       [:div.driver-name driver-name]
       [:div.car-name car-name]]))])

(defn participants-stage []
  (let [{:keys [chart-comp _spec]} (chart/init-chart {:field [400 400]
                                                      :criteria [:top-speed :horsepower :production-year :weight :transmission :skill]
                                                      :*data (r/cursor logic/state [:racers])
                                                      })]
    (fn []
      [:div.participants-stage
       [chart-comp]
       [criteria-list @_spec]
       [participants-list @_spec]
       [:button.btn {:on-click #(>evt [:regen-racers])} "SHOW ME OTHERS"]
       ])))
