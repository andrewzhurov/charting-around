(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :refer [state stages >evt <sub]]
            [charting-around.css]
            ))

(enable-console-print!)

(defn l
  ([desc expr]
   (js/console.log desc expr)
   expr))

(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (apply merge-with deep-merge colls)))



(defn scale-linear [[domain-start domain-end] range]
  (fn [domain-val]
    (* range (/ (- domain-val domain-start) (- domain-end domain-start)))))

(defn rand-in
  "Int in range, inclusive"
  [left-bound right-bound]
  (+ left-bound (rand-int (inc (- right-bound left-bound)))))

(defn gen-data []
  [{:id :vp
    :driver-name "Dalas"
    :car-name "Volkswagen Polo"
    :production-year (rand-in 2014 2018)
    :top-speed (rand-in 150 220)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 150 280)
    :weight (rand-in 1200 1400)}

   {:id :jmm
    :driver-name "Paul"
    :car-name "Jaguar MM"
    :production-year (rand-in 2013 2017)
    :top-speed (rand-in 180 240)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 300 400)
    :weight (rand-in 1300 1600)}

   {:id :su
    :car-name "Subaru impreza"
    :driver-name "No face 1"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)}

   {:id :su2
    :car-name "Subaru impreza"
    :driver-name "No face 2"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)}

   {:id :su3
    :car-name "Subaru impreza"
    :driver-name "No face 3"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)}

   ]
  )


;; Logic
(defn calc-angle [[x y]]
  (* (js/Math.atan2 y x) (/ 180 js/Math.PI)))


(def min-pad 20) ; % of domain
(def max-pad 10) ; % of domain
(defn ready-axis [{:keys [domain min-domain-viz max-domain-viz data] :as axis} [[begin-x begin-y] [end-x end-y] :as coords]]
  (let [displacement [(- end-x begin-x) (- end-y begin-y)]
        range (js/Math.hypot (first displacement) (second displacement))
        ->range (scale-linear domain range)
        ->coords (fn [range-val]
                   (let [ratio (/ range-val range)]
                     (mapv #(+ (* ratio %)
                               %2)
                           displacement
                           (first coords))))]
    (merge axis {:range range
                 :coords coords
                 :min-range-viz (->range min-domain-viz)
                 :max-range-viz (->range max-domain-viz)
                 :angle (calc-angle displacement)
                 :->range ->range
                 :->coords ->coords})))

(defn decide-on-viz [{:keys [axes data] :as spec
                      [size-x size-y] :range}]
  (assoc spec :axes (vec (map-indexed (fn [idx {:keys [desc val-path domain] :as axis}]
                                        (let [[min max] (->> data
                                                              (map #(get-in % val-path))
                                                              (sort)
                                                              ((juxt first last)))
                                              size (- max min)
                                              min (- min
                                                     (* (/ size 100) min-pad))
                                              max (+ max
                                                     (* (/ size 100) max-pad))]
                                          (cond-> (merge axis
                                                         {:domain (or domain [min max])
                                                          :min-domain-viz min
                                                          :max-domain-viz max
                                                          :desc (or desc (pr-str val-path))})
                                            (and (<= (count axes) 2)
                                                 (= 0 idx))
                                            (ready-axis [[0 size-y] [size-x size-y]])

                                            (and (<= (count axes) 2)
                                                 (= 1 idx))
                                            (ready-axis [[0 size-y] [0 0]]))))
                                      axes))))

(defn derive-dps [spec]
  (let [new-dps (->> (for [{:keys [id] :as dp} (:data spec)
                           {:keys [val-path ->range ->coords]} (:axes spec)
                           :let [range (->range (get-in dp val-path))
                                 coords (->coords range)]]
                       {[id val-path] {:range range
                                       :desired-coords coords}})
                     (apply merge))
        ]
    (update spec :dps deep-merge new-dps)))

;0-100
;402m time
;100-200

(defn complete-spec [spec]
  (-> spec
      (decide-on-viz)
      (derive-dps)))

(def spec (r/atom {:range [800 300]
                   :axes [{:domain [2012 2020]
                           :scale scale-linear
                           :tick 1
                           ;:desc "Year"
                           :val-path [:production-year]}
                          {:domain [0 300]
                           :scale scale-linear
                           :tick 30
                           ;:desc "Top speed (km/h)"
                           :desc-side :right
                           :val-path [:top-speed]
                           }]
                   }))




;; WAY 2
(defn ready-axis2 [{:keys [domain min-domain-viz max-domain-viz] :as axis} [[begin-x begin-y] [end-x end-y] :as coords]]
  (let [displacement [(- end-x begin-x) (- end-y begin-y)]
        range (js/Math.hypot (first displacement) (second displacement))
        ->range (scale-linear domain range)
        ->coords (fn [range-val]
                   (let [ratio (/ range-val range)]
                     (mapv #(+ (* ratio %)
                               %2)
                           displacement
                           (first coords))))]
    (merge axis {:range range
                 :coords coords
                 :min-range-viz (->range min-domain-viz)
                 :max-range-viz (->range max-domain-viz)
                 :angle (calc-angle displacement)
                 :->range ->range
                 :->coords ->coords})))

(defn rad [deg] (* (/ js/Math.PI 180) deg))

(defn center [[x y]] [(/ x 2) (/ y 2)])
(defn point-coords [[from-x from-y] angle size]
  [(->> angle
        (rad)
        (Math.cos)
        (* size)
        (+ from-x))
   (->> angle
        (rad)
        (Math.sin)
        (* size)
        (+ from-y))
   ])

(defn decide-on-viz2 [data criteria-meta field]
  (let [criteria (sort (keys (filter (comp :in-display? val) criteria-meta)))]
    (vec
     (map-indexed
      (fn [idx val-path]
        (let [[domain-min domain-max] (->> data
                                           (map #(get-in % val-path))
                                           (sort)
                                           ((juxt first last)))
              size (- domain-max domain-min)
              domain-min (- domain-min
                            (* (/ size 100) min-pad))
              domain-max (+ domain-max
                            (* (/ size 100) max-pad))
              angle (* (/ 360 (count criteria)) idx)]
          (ready-axis2 {:angle angle
                        :val-path val-path
                        :domain [domain-min domain-max]
                        :min-domain-viz min
                        :max-domain-viz max
                        :desc (pr-str val-path)}
                       [(center field)
                        (point-coords (center field) angle (-> (apply min field)
                                                               (/ 2)))])))
      criteria))))

;; Participants chart
(def current-at (r/atom (js/Date.now)))

(def colors ["red" "green" "blue" "gray" "orange" "purple"])
(def participants-meta (r/atom {}))
(def p* (r/atom (gen-data)))
(def p (ra/reaction (vec (map-indexed (fn [idx {:keys [id] :as a-p}]
                                        (-> a-p
                                            (merge (get @participants-meta id))
                                            (assoc :color (get colors idx))))
                                      @p*))))

(def criteria-meta (r/atom {[:production-year] {:in-display? false
                                                :in-inspect? false}
                            [:top-speed]       {:in-display? true
                                                :in-inspect? false}
                            [:transmission]    {:in-display? true
                                                :in-inspect? false}
                            [:horsepower]      {:in-display? true
                                                :in-inspect? false}
                            [:weight]          {:in-display? true
                                                :in-inspect? false}}))

(def p-field [400 400])
(def p-axes
  (ra/reaction (decide-on-viz2 @p
                               @criteria-meta
                               p-field)))

(defn derive-dps2 [p p-axes]
  (->> (for [{:keys [id] :as dp} p
             {:keys [val-path ->range ->coords]} p-axes
             :let [domain-val (get-in dp val-path)
                   range (->range domain-val)
                   coords (->coords range)]]
         {[id val-path] {:range range
                         :desired-coords coords
                         :domain-val domain-val}})
       (apply merge)))

(def p-dps
  (ra/reaction (derive-dps2 @p @p-axes)))


(def p-spec
  (ra/reaction {:range p-field
                :axes @p-axes
                :data @p
                :dps @p-dps
                :events {:select  (fn [id] (swap! participants-meta update-in [id :in-select?] not))
                         :inspect (fn [id] (swap! participants-meta update-in [id :in-inspect?] not))}}))


(defn fill-spec [data]
  (swap! spec (fn [spec] (-> (assoc spec :data data)
                             (complete-spec)))))
(fill-spec (gen-data))


;; Magnet dps
(defn displacement [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])
(defn sys-magnet
  "Magnets entities to their :desired-coords"
  [spec]
  (update spec :dps (fn [dps]
                      (apply merge
                             (for [[id {:keys [desired-coords current-coords] :as result}] dps]
                               {id (if (= desired-coords current-coords)
                                     result
                                     (assoc result :current-coords (if (empty? current-coords)
                                                                     desired-coords
                                                                     (let [[desired-x desired-y] desired-coords
                                                                           [dx dy] (displacement desired-coords current-coords)
                                                                           [current-x current-y] current-coords]
                                                                       [(if (< (Math.abs dx) 0.2)
                                                                          desired-x
                                                                          (+ current-x (/ dx 20)))
                                                                        (if (< (Math.abs dy) 0.2)
                                                                          desired-y
                                                                          (+ current-y (/ dy 20)))]))))})))))

(js/setInterval #(swap! spec sys-magnet) 16)

;; Magent axis
(defn magnet [current desired]
  (if (nil? current)
    desired
    (let [d (- desired current)]
      (if (< (Math.abs d) 0.2)
        desired
        (+ current (/ d 20))))))

(defn sys-axis-magnet
  "Magnets viz of axes to their :desired-vizmin :desired-viz-max"
  [spec]
  (update spec :axes (fn [axes]
                       (vec
                        (for [{:keys [min-range-viz max-range-viz] :as axis} axes]
                          (-> axis
                              (update :current-min-range-viz magnet min-range-viz)
                              (update :current-max-range-viz magnet max-range-viz)))))))

(js/setInterval #(swap! spec sys-axis-magnet) 16)



;; View
(defn axes [{:keys [axes dps]
             [field-w field-h] :range}]
  [:g
   (doall
    (for [{:keys [val-path]
           [[x1 y1] [x2 y2]] :coords} axes]
      ^{:key (pr-str val-path)}
      [:g
       [:line {:id (pr-str val-path)
               :x1 x1 :y1 y1
               :x2 x2 :y2 y2
               :stroke-width "2px"
               :stroke "orange"
               }]
       (let [char-w 7
             w (* (count (pr-str val-path)) char-w)
             h 16]
         [:text {:x (if (> (+ x2 w) field-w)
                      (- field-w w)
                      x2)
                 :y (if (> (+ y2 h) field-h)
                      (- field-h h)
                      y2)
                 :fill "#606060"}
          (pr-str val-path)])]))])

(declare data-points)
(defn radar-chart [{[x y] :range :as spec}]
  (reset! current-at (js/Date.now))
  [:svg {:width x
         :height y}
   [axes spec]
   [data-points spec]])




(defn axis [{:keys [range angle tick desc desc-side ->range current-min-range-viz current-max-range-viz]
             [domain-start domain-end] :domain
             [[begin-x begin-y] [end-x end-y]] :coords :as all}]
  (let [label-offset (case desc-side
                       :right 10
                       -10) ;; TODO
        ]
    [:g.axis {:transform (gstr/format "rotate(%s, %s, %s) translate(%s, %s)" angle begin-x begin-y begin-x begin-y)}
     [:line {:x1 current-min-range-viz :y1 0
             :x2 current-max-range-viz :y2 0}]
     #_[:path.pointer {:d (gstr/format "M%s %s L%s %s L%s %s" (- range 15) -5 range 0 (- range 15) 5)}]
     [:text {:x (/ range 2)
             :y (* 2.5 label-offset)} desc]
     [:g
      (let [amount-fit (/ (- domain-end domain-start) tick)]
        (for [tick-idx (clojure.core/range (inc amount-fit))
              :let [domain-val (+ (* tick-idx tick) domain-start)
                    range-pos (->range domain-val)]
              :when (and (<= range-pos current-max-range-viz)
                         (>= range-pos current-min-range-viz))]
          ^{:key tick-idx}
          [:g.tick
           [:circle.dash {:cx range-pos
                          :cy 0}]
           [:text.val {:x range-pos :y label-offset} domain-val]])
             )]]))

(defn info [data [field-x field-y] [point-x point-y]]
  (let [stroke-height 12
        char-w 6
        pad 5
        h (* stroke-height (count data))
        w1 (->> data keys (map (comp count pr-str)) sort last (* char-w))
        w-gap 10
        x2 (+ w1 w-gap)
        w2 (->> data vals (map (comp count pr-str)) sort last (* char-w))
        entities (map-indexed (fn [idx [k v]] {:k (pr-str k)
                                               :v (pr-str v)
                                               :y (* idx stroke-height)})
                              data)
        total-w (+ w1 w-gap w2 (* pad 2))
        total-h (+ h (* pad 2))
        start-x (if (> (+ point-x total-w) field-x)
                  (- (- total-w) 10)
                  10)
        start-y (if (> (+ point-y total-h) field-y)
                  (- total-h)
                  0)]
    [:g.info {:transform (gstr/format "translate(%s, %s)" start-x start-y) :fill "gray"}
     (doall
      (for [{:keys [k v y]} entities]
        ^{:key k}
        [:g.pair {:transform (gstr/format "translate(%s, %s)" 0 y)}
         [:text {:x 0} k]
         [:text {:x x2} v]]))])
  )

(defn id->color [id] ;; TODO
  (rand-nth ["orange" "red" "blue" "gray" "yellow" "green" "purple" "brown"]))




(defn data-points [{:keys [range dps axes data]
                    {:keys [select inspect]} :events}]
  [:g.data-points
   (if (= 2 (count axes))
     (let [x-val-path (-> axes first :val-path)
           y-val-path (-> axes second :val-path)]
       (for [{:keys [id]} data
             :let [x (get-in dps [[id x-val-path] :current-coords 0])
                   y (get-in dps [[id y-val-path] :current-coords 1])
                   ]]
         ^{:key id}
         [:g.dp {:transform (gstr/format "translate(%s, %s)" x y)}
          [:circle.dp {:r 5 :fill "gray"}]
          [info (first (filter #(= id (:id %)) data))]]))
     (for [{:keys [id in-inspect? in-select? color]} data
           :let [ent-dps (filter (fn [[[ent-id]]] (= ent-id id)) dps)
                 polygon (->> ent-dps
                              (sort-by (fn [[[_ val-path]]] val-path))
                              (map (fn [[_ {[x y] :desired-coords}]]
                                     (str x "," y)))
                              (clojure.string/join " "))
                 ;color (id->color id)
                 ]]
       ^{:key id}
       [:g.entity-polygon {:class (str (when in-inspect? "in-inspect ")
                                       (when in-select? "in-select "))}
        [:polygon {:on-click #(select id)
                   :on-mouse-over #(inspect id)
                   :on-mouse-out #(inspect id)
                   :points polygon
                   :fill color
                   :stroke color}]
        (for [[dp-id {[x y] :desired-coords
                      :keys [domain-val]}] ent-dps]
          ^{:key dp-id}
          [:g.criteria {:transform (gstr/format "translate(%s, %s)" x y)}
           [:circle {:r 3 :fill color}]
           [:text {:fill "#404040"} (pr-str domain-val)]]
          )])
     )])



(defn chart [{[x y] :range :as spec}]
  (conj [:svg {:width x
               :height y}]
        [axis (get-in spec [:axes 0])]
        [axis (get-in spec [:axes 1])]
        [data-points spec]))


(defn chart-panel []
  [:div
   [chart @spec]
   [:button {:on-click #(fill-spec (gen-data))} "Different dataset"]])

#_(defn chart []
  (let [[center-x center-y] [315 120]
        data [{:name :a :domain-min 0 :domain-max 100  :value-min 15 :value-max 75}
              {:name :b :domain-min 5 :domain-max 120  :value-min 75 :value-max 110}
              {:name :c :domain-min 15 :domain-max 30  :value-min 20 :value-max 25}
              {:name :d :domain-min -10 :domain-max 10 :value-min -5 :value-max 7}
              {:name :e :domain-min 3 :domain-max 30   :value-min 5 :value-max 25}]
        range 100
        sector-angle (/ 360 (count data))
        start-angle -90
        domain->range (fn [domain-min domain-max]
                        (let [domain-range (- domain-max domain-min)]
                          (fn [range]
                            (fn [x]
                              (* (/ range domain-range) (- x domain-min))))))]
    [:svg {:width "100%"
           :height "100%"}
     [:g.chart.radar {:transform (gstr/format "translate(%s,%s)" center-x center-y)}
      (map-indexed
       (fn [idx {:keys [name domain-min domain-max value-min value-max]}]
         (let [->r ((domain->range domain-min domain-max) range)
               r-min (->r value-min)
               r-max (->r value-max)]
           ^{:key name}
           [:g {:transform (gstr/format "rotate(%s)" (+ start-angle (* idx sector-angle)))}
            [:line.axis {:x1 0 :y1 0 :x2 range :y2 0
                         }]
            [:line.domain {:x1 r-min :y1 0 :x2 r-max :y2 0
                           }]
            [:text {:x (->r value-min)} value-min]
            [:circle.tick.min {:cx (->r value-min)
                               :cy 0}]
            [:circle.tick.max {:cx (->r value-max)
                               :cy 0}]]))
       data)]]))

(defn stats []
  [:div.stats [chart-panel]])

(defmulti stage-content :stage)

(defmethod stage-content :participants
  [{:keys [racers]}]
  [:div.stage-content.participants
   [:div.collection.with-header
    [:div.collection-header [:h4 "Participants"]]
    (for [[pt-id {:keys [name skill participates?]}] racers]
      ^{:key pt-id}
      [:a.collection-item {:class (when participates? "active")
                           :on-click #(>evt [:toggle-participant pt-id])}
       (str name " "  skill)])]
   ])

(defn driver [{:keys [id name avatar skill]}]
  [:div.driver {:draggable true
                :on-drag-start (fn [evt] (.setData (.-dataTransfer evt) "pt-id" id))
                :id id}
   [:div.avatar avatar]
   [:div.name name]
   [:div.skill skill]])

(defmethod stage-content :bets
  [state]
  (let [pts (filter (comp :participates? val) (:racers state))
        {left-drivers false
         betted-drivers true} (group-by (comp boolean :bet val) pts)]
    [:div#bets.stage-content "BETS:"
     [:div.drivers
      (for [[pt-id dr] left-drivers]
        ^{:key pt-id}
        [driver dr])]

     [:div.bets
      (for [place (range 1 (inc (count pts)))
            :let [[id {{:keys [chance]} :bet :as dr}] (first (filter (comp #{place} :place :bet val) betted-drivers))]]
        (let [node-id (str "p" place)]
          ^{:key place}
          [:div.bet {:id node-id
                     :on-drop (fn [evt]
                                (println "DROP!")
                                (.setAttribute (js/document.getElementById node-id) "hover-over" false)
                                (let [pt-id (.getData (.-dataTransfer evt) "pt-id")]
                                  (>evt [:place-bet pt-id {:place place :chance 80}])))
                     :on-drag-over (fn [evt] (.preventDefault evt))
                     :on-drag-enter (fn [evt] (.setAttribute (js/document.getElementById node-id) "hover-over" true))
                     :on-drag-leave (fn [evt] (.setAttribute (js/document.getElementById node-id) "hover-over" false))
                     }
           [:div.place place]
           (when dr [driver dr])
           (when chance [:input.chance {:type :range
                                        :value chance
                                        :on-change #(>evt [:place-bet id {:place place :chance (.-value (.-target %))}])}])]))]]))

(defmethod stage-content :race
  [state]
  [:div#race.stage-content "RACE PROGRESS"
   ])
(defmethod stage-content :results
  [state]
  [:div#results.stage-content "RESULTS"
   (str (<sub [:wins-history]))])

#_(defmulti stage :stage)
#_(defmethod stage :participants
  [state]
  [:div.stage.participants
   ])

(defn stage []
  (let [stage-id (:stage @state)
        {:keys [name next prev]} (get stages stage-id)]
    [:div.stage
     [stats]
     [stage-content @state]
     (when prev [:button.prev {:on-click #(>evt [:to-stage prev])} prev])
     (when next [:button.next {:on-click #(do (>evt [:to-stage next])
                                              (when (= :race next)
                                                (>evt [:race])))} next])]))


;; TODO inspect
(defn criteria-list []
  [:div.criteria-list
   (for [[val-path {:keys [in-display?]}] (sort-by key (l 0 @criteria-meta))]
     ^{:key val-path}
     [:div.criteria.chip {:class (when in-display? "in-display")
                          :on-mouse-over
                          #(swap! criteria-meta update-in [val-path :in-inspect?] not)
                          :on-click
                          #(swap! criteria-meta update-in [val-path :in-display?] not)}
      (pr-str val-path)]
     )])

(defn participants-list [{{:keys [inspect select]} :events}]
  [:div.participants-list
   (doall
    (for [{:keys [id driver-name car-name color in-select?]} @p]
      ^{:key id}
      [:div.participant.card {:class (when in-select? "in-select")
                              :on-click #(select id)
                              :on-mouse-enter #(inspect id)
                              :on-mouse-leave #(inspect id)
                              :style {:border-color color
                                      :border-style "solid"}}
       [:div.driver-name driver-name]
       [:div.car-name car-name]]))])

(defn root []
  [:div#root
   [:style (garden.core/css charting-around.css/styles)]
   [:link {:rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"}]

   [radar-chart @p-spec]
   [criteria-list]
   [participants-list @p-spec]
   #_[stage]
   ])

(r/render [root]
          (.-body js/document))


