(ns charting-around.css
  (:require #?(:cljs [goog.string :refer [format]])
            [garden.color :as color :refer [rgb hsl]]))

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
  [[:#root {:position "absolute"
            :top 0 :left 0 :right 0 :bottom 0}]
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

   [:.driver
    {:display "flex"
     :align-items :center
     :width "200px"
     :height "50px"
     :border "1px solid gray"
     :cursor "grab"}]
   [:#bets {:width "fit-content"}
    [:.drivers {:float :left}]
    [:.bets {:float :right}]
    [:.bet {
            :background-color "lightgray"
            :opacity "0.7"
            :border "2px solid gray"
            :display "grid"
            :grid-template (grid "place  driver 50px"
                                 "chance chance 30px"
                                 "50px   200px")}
     [:.place {:grid-area "place"
               :font-size "24px"
               :text-decoration "underline"
               :pointer-events "none"}]
     [:.driver
      {:grid-area "driver"
       :display "flex"
       :align-items :center
       :width "200px"
       :height "50px"
       :border "1px solid gray"
       :cursor "grab"}]
     [:.chance {:grid-area "chance"}]

     [:&#p1 {:border-color "gold"
             :background-color "gold"}]
     [:&#p2 {:border-color "silver"
             :background-color "silver"}]
     [:&#p3 {:border-color "bronze"
             :background-color "bronze"}]
     ]
    [".bet[hover-over=true]" {:border "2px dashed cadetblue !important"}]
    ]])


