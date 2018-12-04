(ns charting-around.core
  (:require [reagent.core :as r]
            [garden.core]
            [goog.string :as gstr]))

(defn l [desc expr]
  (js/console.log desc expr)
  expr)

(defn deep-merge [coll1 coll2]
  (if (not (and (map? coll1) (map? coll2)))
    coll2
    (merge-with deep-merge coll1 coll2)))

(def styles
  [[:svg {:display "block"
          :margin-left "auto"
          :margin-right "auto"}]
   [:.axis
    [:line {:stroke "red"}]
    [:text {:font-size "15px"}]
    [:.tick
     [:.dash {:r 3 :fill "orange"}]
     [:.val {:font-size "10px"}]]]])

(defn scale-linear [[domain-start domain-end] range]
  (fn [domain-val]
    (* range (/ (- domain-val domain-start) (- domain-end domain-start)))))

(defn scale-linear-reverse [[domain-start domain-end] range]
  (fn [domain-val]
    (js/Math.abs (- (* range (/ (- domain-val domain-start) (- domain-end domain-start))) range))))

(def spec
  {:range [800 300]
   :x-axis {:domain [2012 2020]
            :scale scale-linear
            :tick 1
            :desc "Year"
            :val-path [:production-year]
            }
   :y-axis {:domain [0 300]
            :scale scale-linear
            :tick 30
            :desc "Top speed (km/h)"
            :val-path [:top-speed]
            }
   :data #{{:car-name "Volkswagen Polo"
            :production-year 2018
            :top-speed 200}
           {:car-name "Jaguar MM"
            :production-year 2017
            :top-speed 280}
           {:production-year 2016
            :car-name "Subaru impreza"
            :top-speed 250}}
   })


(defn label-defaults [position direction]
  (case [position direction]
    [:bottom-left :right] {:label-side :left}
    [:bottom-left :top] {:label-side :right}
    [:bottom-right :left] {:label-side :right}
    [:bottom-right :top] {:label-side :left}
    [:top-left :right] {:label-side :right}
    [:top-left :bottom] {:label-side :left}
    [:top-right :left] {:label-side :left}
    [:top-right :bottom] {:label-side :right}))

(def axis-defaults
  {:x-axis {:position :bottom-left
            :direction :right
            }
   :y-axis {:position :bottom-left
            :direction :top
            }})

(def spec*
  (let [[range-x range-y] (get spec :range)]
    (-> (merge-with merge axis-defaults spec)
        (update :x-axis (fn [{:keys [position direction] :as axis}]
                          (merge axis (label-defaults position direction))))
        (update :y-axis (fn [{:keys [position direction] :as axis}]
                          (merge axis (label-defaults position direction))))
        (update :x-axis (fn [{:keys [domain scale direction] :as axis}]
                          (let [range (case direction
                                        :right range-x
                                        :left range-x
                                        :top range-y
                                        :bottom range-y)]
                            (assoc axis :->range (scale domain range)))))
        (update :y-axis (fn [{:keys [domain scale direction] :as axis}]
                          (let [range (case direction
                                        :right range-x
                                        :left range-x
                                        :top range-y
                                        :bottom range-y)]
                            (assoc axis :->range (scale domain range)))))
        )))

(defn svg [{[x y] :range} & childs]
  (into [:svg {:width x
               :height y}]
        childs))

(defn axis [[range-x range-y]
            {:keys [scale tick desc ->range direction position label-side]
             [domain-start domain-end] :domain}]
  (let [[begin-x begin-y] (case position
                            :top-left [0 0]
                            :top-right [range-x 0]
                            :bottom-right [range-x range-y]
                            :bottom-left [0 range-y])
        range (case direction
                :right range-x
                :left range-x
                :top range-y
                :bottom range-y)
        rotate (case direction
                 :right 0
                 :top -90
                 :left -180
                 :bottom -270)
        label-offset (case label-side
                       :right 10
                       :left -10)]
    [:g.axis {:transform (gstr/format "rotate(%s, %s, %s) translate(%s, %s)" rotate begin-x begin-y begin-x begin-y)}
     [:line {:x1 0 :y1 0
             :x2 range :y2 0}]
     [:path.pointer {:d (gstr/format "M%s %s L%s %s L%s %s" (- range 15) -5 range 0 (- range 15) 5)}]
     [:text {:x (/ range 2)
             :y (* 2.5 label-offset)} desc]
     [:g
      (let [amount-fit (/ (- domain-end domain-start) tick)]
        (map (fn [tick-idx]
               (let [domain-val (+ (* tick-idx tick) domain-start)
                     scale-pos (->range domain-val)]
                 ^{:key tick-idx}
                 [:g.tick
                  [:circle.dash {:cx scale-pos
                                 :cy 0}]
                  [:text.val {:x scale-pos :y label-offset} domain-val]]))
             (clojure.core/range (inc amount-fit))))]]))

(defn data-points [{{->x-range :->range
                     x-val-path :val-path} :x-axis
                    {->y-range :->range
                     y-val-path :val-path} :y-axis
                    :keys [data]
                    [_ range-y] :range}]
  [:g.data-points {:transform (gstr/format "translate(0, %s) scale(1, -1)" range-y)} ;; Not that good
   (doall
    (for [dp data]
      (let [x (->x-range (get-in dp x-val-path))
            y (->y-range (get-in dp y-val-path))]
        ^{:key [x y]} ;; Not that unique
        [:circle {:cx x :cy y :r 5}])))])

(defn root []
  [:div#root
   [:style (garden.core/css styles)]
   [svg spec*
    [axis (:range spec*) (:x-axis spec*)]
    [axis (:range spec*) (:y-axis spec*)]
    [data-points spec*]]])

(r/render [root]
          (.-body js/document))
