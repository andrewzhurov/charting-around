(ns charting-around.css
  (:require [garden.def :refer [defkeyframes]]
            [garden.color :as color :refer [rgb hsl]]))

(def active-color "orange")


(defkeyframes move1
  ["0%" {:margin-left "0px"}]
  ["50%" {:margin-left"20px"}]
  ["100%" {:margin-left"0px"}])

(defkeyframes collapse
  [:to {:margin-bottom "0px"
        :padding-bottom "0px"}])

(defkeyframes merge-to-left
  [:to {:margin-left "10px"}])
(defkeyframes merge-to-right
  [:to {:margin-left "10px"}])

(defkeyframes to-stone
  [:to {:background-color "gray"
        :opacity 1}])

(defkeyframes test-anim
  ["33%" {:background-color "red"}]
  ["66%" {:background-color "green"}]
  ["100%" {:background-color "blue"}]
  )

(defkeyframes appear
  ["100%" {:transform "scale(1.2)"
           :opacity 1}]
  ["66%" {:transform "scale(1.5)"
          :opacity 1}]
  ["0%" {:transform "scale(1)"
         :opacity 0}]
   )

(defkeyframes appear-val
  ["100%" {:transform "scale(1.1)"
           :opacity 1}]
  ["66%" {:transform "scale(1.2)"
          :opacity 1}]
  ["66%" {:transform "scale(1)"
          :opacity 0}]
  )

(defkeyframes collapse-val-pos
  ["100%" {:opacity 0}]
  ["50%" ])

(defkeyframes shine
  ["0%" {:opacity 1}]
  ["8%" {:opacity 0.6}]
  ["30%" {:opacity 1}]
  ["38%" {:opacity 0.5}]
  ["70%" {:opacity 1}]
  ["100%" {:opacity 1}]
  )
(defkeyframes rainbow
  ["22%" {:border-color "chocolate"
          :color "chocolate"}]
  ["44%" {:border-color "cornflowerblue"
          :color "cornflowerblue"}]
  ["66%" {:border-color "cadetblue"
          :color "cadetblue"}#_{:border-color "crimson"
          :color "crimson"}]
  ["88%" {:border-color "darkgoldenrod"
          :color "darkgoldenrod"}]
  #_["100%" ])

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
   collapse
   merge-to-left
   merge-to-right
   to-stone
   appear
   appear-val
   rainbow
   shine
   test-anim
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
            :padding-top "10px"
            :flex-basis 0}
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
                 :margin-bottom "3px"
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
   [:#bets2 {:width "min-content"
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


   [:.results {:position "relative"
               :width "100%"
               :margin-left "10px"}]
   [:.results-presentation-picker {:position "absolute"
                                   :top "10px"
                                   :right "10px"}]
   [:.participants-stage {:flex-basis 0}]


   [:.rank-view {:display "flex"
                 :height "60px"
                 :width "60px"
                 :align-items "center"
                 :justify-content "space-around"
                 :border "2px solid gray"
                 :border-radius "50%"
                 :opacity 0
                 :transition "0.3s"
                 :animation-fill-mode "forwards !important"}
    [:&.S {:animation [[appear "0.7s"]
                       [rainbow "3s" :infinite]]
           :animation-delay "1.5s"
           }
     ]
    [:&.A {:color "gold"
           :animation [[appear "0.7s"]
                       [shine "4s" :infinite]]
           :animation-delay "1.2s"}]
    [:&.B {:color "silver"
           :animation [[appear "0.7s"]]
           :animation-delay "1s"
           }]
    [:&.C {:color "bronze"
           :animation [[appear "0.7s"]]
           :animation-delay "1s"
           }]
    [:&.D {:color "lead"
           :animation [[appear "0.7s"]]
           :animation-delay "1s"
           }]
    [:&.E {:color "steel"
           :animation [[appear "0.7s"]]
           :animation-delay "1s"
           }]
    [:&.F {:color "coal"
           :animation [[appear "0.7s"]]
           :animation-delay "1s"
           }]

    [:.rank {:color "inherit"
             :font-size "22px"
             }
     ]]
   [:.stacks {:margin-top "7px"
              :position "relative"}
    [:.pos {:position "absolute"
            :top "0px"
            :left "0px"
            :animation [[merge-to-right "0.4s"]]
            :animation-delay "1s"
            :animation-fill-mode :forwards
            :z-index 5}
     [:.stack {:background-color "darkgreen"}]
     [:.val {:margin-left "25px"
             :animation [[appear-val "2s"]]
             :animation-fill-mode :forwards
             :opacity 0
             :color "darkgreen"
             :font-weight "700"}]
     ]

    [:.neg {:position "absolute"
            :top "0px"
            :margin-left "20px"
            :animation [[merge-to-left "0.4s"]]
            :animation-delay "1s"
            :animation-fill-mode :forwards
            :z-index 5}
     [:.stack {:background-color "firebrick"}]
     [:.val {:margin-left "25px"
             :animation [[appear-val "2s"]]
             :animation-fill-mode :forwards
             :opacity 0
             :color "firebrick"
             :font-weight "700"
             }]
     ]]

   [:.stack {:width "20px"
             :animation [[collapse "0.4s"]]
             :animation-delay "0.5s"
             :animation-timing-function :ease-in-out
             :animation-fill-mode :forwards

             ;:transition-delay "2s"
             ;:transition "2s"
             ;:margin-bottom "0px !important"
             :opacity "0.6"
             }
    [:&.space {:height "0px !important"
               :padding-bottom "270px !important"
               :margin-bottom "0px !important"
               :visibility "hidden"}]
    ]

   [:.test {:height "20px"
            :width "20px"
            :position "absolute"
            :top "200px"
            :left "60px"
            :animation [[test-anim "3s" :infinite]]
            :animation-delay "0.5s"
            :animation-timing-function "linear"
            }]

   [:.results {:transition "1s"}]
   [:.results.hidden {:opacity 0}]
   [:.row {:width "min-content"}]

   [:.btn.pressed {:background-color "#1d7d74 !important"}]
   ])

