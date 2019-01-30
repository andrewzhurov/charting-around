(ns charting-around.stages.participants
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.chart :as chart]
            [charting-around.logic :refer [>evt <sub] :as logic]
            [charting-around.stage :as stage]
            ))

(defn l [desc expr] (js/console.log desc expr) expr)

;; TODO inspect
(defn criteria-list [{:keys [criteria]
                      {{:keys [display inspect]} :criteria} :api :as spec}] ;; TODO
  [:div.criteria-list
   (for [[val-path {:keys [in-display?]}] criteria #_(sort-by key @criteria-meta)]
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
                                                      }
                                                     #_{:field [400 400]
                                                      :criteria [:at :money :blah]
                                                      :data [{:at 1 :money 50 :blah 2}
                                                             {:at 2 :money 60 :blah 5}
                                                             {:at 3 :money 30 :blah 8}
                                                             {:at 4 :money 40 :blah 10}]})]
    (fn []
      #_(l "SPECCCCCC:" @_spec)
      [:div.participants-stage
       [chart-comp]
       [criteria-list @_spec]
       [participants-list @_spec]
       [:button.btn {:on-click #(>evt [:regen-racers])} "SHOW ME OTHERS"]
       ])))

(defmethod stage/stage :participants
  [{:keys [racers]}]
  [participants-stage])
