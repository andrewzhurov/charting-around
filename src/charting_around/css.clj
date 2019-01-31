(ns charting-around.css
  (:require [garden.def :refer [defkeyframes]]
            [garden.color :as color :refer [rgb hsl]]))

(def active-color "orange")


(defkeyframes move1
  [:from {:margin-left "0px"}]
  [:to {:margin-left"20px"}])


(defn grid [& strs]
  (let [rows (butlast strs)
        columns (last strs)
        escaped-rows (for [row rows]
                       (let [[areas size] (clojure.string/split row #" (?=[\w\d]+$)")]
                         (format "\"%s\" %s" areas size)))]
    (str (clojure.string/join "\n"  (conj (vec escaped-rows)  (str "/ " columns))))))

(comment
  {:grid-template (grid "nav-section auto"
                        "content-section 1fr"
                        "1fr")})

(def styles
  [move1
   ;; RM ::after setup
   ["div#app::after" {:visibility "hidden"}] ;; Hide setup hint
   [:#root {:position "absolute"
            :top 0 :left 0 :right 0 :bottom 0
            :transition "0.2s"}]
   [:.stage {:height "100%"
             :width "100%"
             :display "grid"
             :grid-template (grid "prev stage-content next 1fr"
                                  "prev stats         next 1fr"
                                  "100px 1fr           100px")}]
   [:.prev {:grid-area "prev"}]
   [:.next {:grid-area "next"}]
   [:.stage-content {:grid-area "stage-content"}
    [:&.participants
     [:a {:cursor "pointer"
          :title "Add participant"}]]]
   [:.stats {:grid-area "stats"}]

   [:svg {:display "block"
          :margin-left "auto"
          :margin-right "auto"}]
   [:.axis
    [:line {:stroke "red"}]
    [:text {:font-size "15px"}]
    [:.tick
     [:.dash {:r 3 :fill "orange"}]
     [:.val {:font-size "10px"}]]]

   [:#bets {:width "100%"
            :height "100%"
            :display "flex"
            :justify-content "space-around"
            :padding-top "10px"}
    [:.drop-overlay {:position "absolute"
                     :left "0px" :top "0px" :right "0px" :bottom "0px"
                     :background-color "gray"
                     :opacity 0
                     :display "flex" :flex-direction "column"
                     :justify-content "space-around"
                     :align-items "center"
                     :pointer-events "none"
                     :border "3px dashed lightgray"}
     [:text {:font-size "30px"
             :color "white"
             }]]
    [:&.dragging
     [:.drop-overlay {:opacity 0.8}]
     [:.drop-overlay:hover {:border-style "solid"}]]]

   [:.drivers {:position "relative"
               :min-height "40%"
               :min-width "45%"
               :height "fit-content"
               :border "1px solid gray"
               :border-radius "5px"}
    ]
   [:.to-race {:position "absolute"
               :bottom "10px"
               :left "50%"}]
   [".drivers[hover-over=true]"
    [:.overlay {:opacity 0.7}]
    [:.participant {:pointer-events "none"}]]
   [:.bets {:display "flex"
            :min-width "45%"
            :flex-direction "column"
            :width "fit-content"}]
   [:.bet {:position "relative"
           :min-height "140px"
           :margin-bottom "5px"}]
   [:.place {:grid-area "place"
             :font-size "24px"
             :text-decoration "underline"
             :pointer-events "none"}]

   [:&#p1 {:border-color "gold"
           :background-color "gold"}]
   [:&#p2 {:border-color "silver"
           :background-color "silver"}]
   [:&#p3 {:border-color "bronze"
           :background-color "bronze"}]
   [".bet[hover-over=true]" {:border "2px dashed cadetblue !important"}]
    

   [:.entity-polygon
    [:.criteria {:visibility "hidden"}
     [:&.in-inspect {:visibility "visible"}]]
    [:polygon {:fill-opacity 0.3
               :stroke-width "0px"
               :cursor "pointer"}]

    [:&.in-inspect
     [:polygon {:fill-opacity 0.4
                :stroke-width "1px"}]
     [:.criteria {:visibility "visible"}]]
    [:&.in-select
     [:polygon {:stroke-width "2px !important"}]
     ]]

   [:.criteria-list
    [:.criteria {:background-color "lightgray"
                 :cursor "pointer"
                 :border-width "2px"
                 :border-style "solid"
                 :border-color "darkgray"
                 :border-radius "20px"
                 :display "inline-block"
                 :padding "6px"
                 :margin-right "4px"
                 :transition "0.4s border-color"
                 }]
    [:.criteria.in-display {:border-color active-color
                            ;:background-color #_active-color "#2bbbad"
                            }]]

   [:.participants-list
    {:display "flex"}
    [:.participant {:position "relative"
                    :margin-right "5px"
                    :padding "3px"
                    :border-width "1px"
                    :cursor "pointer"
                    :transition "border-width 0.2s"}]
    [:.participant.in-select {:border-width "2px"}]
    [:.participant
     [:.background {:position "absolute"
                    :top "0px" :right "0px" :bottom "0px" :left "0px"
                    :opacity 0.1}]]
    [:.participant.in-inspect
     [:.background {:opacity 0.3}]]]

   [:.axes
    [:line {:stroke-width "2px"
            :stroke active-color #_"cadetblue"}]]
   [:.btn.disabled {:pointer-events "all !important"}]

   [:.dp
    [:.info {:visibility "hidden"}]
    [:.dot {:fill-opacity "0.5"
            :r 4
            :stroke-width 0
            :cursor "pointer"}]]
   [:.dp.in-inspect
    [:.info {:visibility "visible"}]
    [:.dot {:fill-opacity "0.6"
            :stroke-width "2px"}]]
   [:.dp.in-select
    [:.dot {:fill-opacity "1"
            :r 6}]]
   #_[:.chart.radar
    [:line.axis {:stroke-width "1px" :stroke "lightgray"}]
    [:line.domain {:stroke-width "2px" :stroke "cadetblue" #_"orange"}]
    [:.tick.min {:r 2 :fill "gray"}]
    [:.tick.max {:r 2 :fill "red"}]
    [:text {:font-size "10px"}]
      ]

   [:#welcome {:display "flex"
               :flex-direction "column"}]
   [:#results
    [:.axis {:stroke "gray"}]
    [:.balance {:r 5}]]

   [:.collapsable {:overflow "hidden"
                   :max-height "300px"
                   :transition "0.5s"
                   }
    [:&.collapse {:max-height "0px"}]]

   [:.vanishable {:opacity "1"
                  :transition "0.4s"}
    [:&.vanish {:opacity "0"}]]

   [:.layout {:display "flex"}]
   [:#bets2 {:width "100%"
             :display "flex"
             :flex-direction "column"
             :align-items "center"}
    [:.answers {:display "flex"}]
    [:.answer {:cursor "pointer"
               :opacity "1"
               :transition "0.3s"
               :border-bottom "2px solid transparent"}
     [:&.hidden {:opacity "0"
                 :pointer-events "none"}]
     [:&.betted {:border-bottom "2px solid orange"}]]]

   [:.stack {:background-color "gray"
             :height "30px"
             :width "20px"
             :margin-left "0px"
             :transition "2s margin-left"
             }
    [:&.move1 {:margin-left "20px"}]]
   ])


