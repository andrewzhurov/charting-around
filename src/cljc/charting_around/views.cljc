(ns charting-around.views
  (:require [garden.core :refer [css]]
            [garden.compiler :as gc]
            [garden.types :as gt]
            [garden.units :refer [px pt em ms percent defunit]]
            [garden.color :refer [hsl rgb rgba hex->rgb as-hex]]
            [charting-around.core :as c :refer [handle-message! ]]
            #?(:cljs [reagent.core :as r])
            #?(:cljs [goog.string :as gstr])))


#?(:cljs (def format gstr/format))

(defunit rad)
(defunit deg)

(defn rotate [& d]      (gt/->CSSFunction "rotate" d))
(defn translate [& d]   (gt/->CSSFunction "translate" d))
(defn scale [& d]       (gt/->CSSFunction "scale" d))

(def css-str (comp rest gc/render-css gc/expand))

(def styles
  [[:svg {:display :block
          :margin-left :auto
          :margin-right :auto
          :font-family ["Gill Sans" "Helvetica" "Verdana" "Sans Serif"]}]
   [:.axis
    [:line {:stroke :red}]
    [:text {:font-size (px 15)}]
    [:.tick
     [:.dash {:r 3 :fill :orange}]
     [:.val {:font-size (px 10)}]]]])

(defn axis [{:keys [range angle tick desc ->range]
             [domain-start domain-end] :domain
             [[begin-x begin-y] [end-x end-y]] :coords :as all}]
  (let [label-offset -10 ;; TODO
        ]
    [:g.axis
     {:transform (css-str [(rotate angle begin-x begin-y) (translate begin-x begin-y)])}
     ;{:transform (format  "rotate(%s, %s, %s) translate(%s, %s)" angle begin-x begin-y begin-x begin-y)}
     [:line {:x1 0 :y1 0
             :x2 range :y2 0}]
     [:path.pointer {:d (format "M%s %s L%s %s L%s %s" (- range 15) -5 range 0 (- range 15) 5)}]
     [:text {:x (/ range 2)
             :y (* 2.5 label-offset)} desc]
     [:g
      (let [amount-fit (/ (- domain-end domain-start) tick)]
        (map (fn [tick-idx]
               (let [domain-val (+ (* tick-idx tick) domain-start)
                     range-pos (->range domain-val)]
                 ^{:key tick-idx}
                 [:g.tick
                  [:circle.dash {:cx range-pos
                                 :cy 0}]
                  [:text.val {:x range-pos :y label-offset} domain-val]]))
             (clojure.core/range (inc amount-fit))))]]))

(defn data-points [{:keys [derived-data axes]}]
  [:g.data-points
   (if (= 2 (count axes))
     (for [[dp-id axes-result] derived-data
           :let [x (-> axes-result vals first :coords first)
                 y (-> axes-result vals second :coords second)]]
       ^{:key dp-id}
       [:circle {:cx x :cy y :r 5}])
     (for [[dp-id axes-result] derived-data
           [axis-id {[x y] :coords}] axes-result]
       ^{:key [dp-id axis-id]}
       [:circle {:cx x :cy y :r 5}]))])

(defn svg [{[x y] :range} & childs]
  (into [:svg {:width x
               :height y
               :on-mouse-move (fn [e] (handle-message! {:n (.. e -clientY)}))}]
        childs))

(defn root
  ([state]
    [:div#root
    [:style (garden.core/css styles)]
    [svg state
     [axis (get-in state [:axes :production-axis])]
     [axis (get-in state [:axes :speed-axis])]
     [data-points state]]]))
