(ns charting-around.chart
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state stages >evt <sub]]
            [charting-around.css]
            ))

(defn l
  ([desc expr]
   (js/console.log desc expr)
   expr))
(defn !vec [?vec] (if (vector? ?vec) ?vec [?vec]))

(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (let [all-keys (reduce into #{} (map keys colls))
          only (reduce (fn [only-keys coll] (if (:merge/only (meta coll))
                                              (clojure.set/intersection only-keys (set (keys coll)))
                                              only-keys))
                       all-keys
                       colls)]
      (apply merge-with deep-merge (map (fn [coll] (select-keys coll only)) colls)))))


(defn scale-linear [[domain-start domain-end] range]
  (fn [domain-val]
    (if (= domain-start domain-end)
      range
      (* range (/ (- domain-val domain-start) (- domain-end domain-start))))))

(defn calc-angle [[x y]]
  (* (js/Math.atan2 y x) (/ 180 js/Math.PI)))


;; Defs
(def p-field [400 400])
(def center-coords [(/ (get p-field 0) 2) (/ (get p-field 1) 2)])
(def min-pad 20) ; % of domain
(def max-pad 10) ; % of domain



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
                 :min-viz-coords (->coords (->range min-domain-viz))
                 :max-viz-coords (->coords (->range max-domain-viz))
                 :angle (calc-angle displacement)
                 :->range ->range
                 :->coords ->coords})))

(defn rad [deg] (* (/ js/Math.PI 180) deg))

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
  (let [criteria-in-display (sort-by first (filter (comp :in-display? val) criteria-meta))
        criteria (deep-merge criteria-meta
                             (apply merge (map-indexed (fn [idx [k _]] {k {:idx idx}}) criteria-in-display)))]
    (mapv
     (fn [[val-path {:keys [domain idx] :as cr}]]
       (let [[domain-min domain-max] (->> data
                                          vals
                                          (map #(get-in % val-path))
                                          (sort)
                                          ((juxt first last)))
             size (- domain-max domain-min)
             domain-min2 (- domain-min
                            (* (/ size 100) min-pad))
             domain-max2 (+ domain-max
                            (* (/ size 100) max-pad))
             angle (* (/ 360 (count criteria-in-display)) idx)]
         (ready-axis2 (merge
                       {:angle angle
                        :val-path val-path
                        :domain (or domain [domain-min2 domain-max2])
                        :min-domain-viz domain-min
                        :max-domain-viz domain-max
                        :desc (pr-str val-path)}
                       cr)
                      (cond (and (= (count criteria-in-display) 2)
                                 (= idx 0))
                            [[0 0]
                             [(first field)]]
                            #_[[0 (second field)]
                               field]
                            (and (= (count criteria-in-display) 2)
                                 (= idx 1))
                            #_[[0 (second field)]
                               [0 0]]
                            [[0 0]
                             [0 (second field)]]

                            :else
                            [center-coords
                             (point-coords center-coords angle (-> (apply min field)
                                                                   (/ 2)))]))))
     criteria)))

;; Participants chart
(def colors ["red" "green" "blue" "gray" "orange" "purple"])

(defn derive-dps2 [p p-axes]
  (merge (apply merge
                (for [[id dp] p
                      {:keys [val-path ->range ->coords in-inspect? in-display?]} p-axes
                      :let [domain-val (get-in dp val-path)
                            range (->range domain-val)
                            coords (->coords range)]]
                  {[id val-path] {:val-path val-path
                                  :range range
                                  :in-display? in-display?
                                  :in-inspect? in-inspect?
                                  :desired-coords (if in-display? coords center-coords)
                                  :domain-val domain-val}}))
         (apply merge
                (for [{:keys [val-path min-viz-coords max-viz-coords in-display?] :as spec} p-axes]
                  {[:axis val-path] (merge
                                     spec
                                     {:desired-coords (if (and in-display?
                                                               (not-empty min-viz-coords)
                                                               (not-empty max-viz-coords))
                                                        [min-viz-coords max-viz-coords]
                                                        [center-coords center-coords])})}))))


(declare chart)
(declare sys-magnet)
(defn init-chart [{:keys [*data field criteria]}]
  (let [chart-id (keyword (random-uuid))

        *criteria-meta (r/atom (into (array-map) (map (fn [cr] [(!vec cr) {:in-display? true :in-inspect? false}]) criteria)))
        *data-meta (r/atom {})

        _data2 (ra/reaction (into {} (map-indexed (fn [idx [vk vv]]
                                                    [vk (assoc vv :color (get colors idx))])
                                                  @*data)))
        _data3 (ra/reaction (deep-merge @_data2
                                        @*data-meta))

        _axes (ra/reaction (decide-on-viz2 @_data3
                                           @*criteria-meta
                                           field))

        *temp (r/atom nil)
        _ (ra/run! (swap! *temp deep-merge ^:merge/only (derive-dps2 @_data3 @_axes)))

        api {:data {:select  (fn [id]     (swap! *data update-in [id :in-select?]  not))
                    :inspect (fn [id on?] (swap! *data assoc-in  [id :in-inspect?] on?))}
             :criteria {:inspect (fn [val-path on?] (swap! *criteria-meta assoc-in  [val-path :in-inspect?] on?))
                        :display (fn [val-path]     (swap! *criteria-meta update-in [val-path :in-display?] not))}
             }

        _spec
        (ra/reaction {:range field
                      :data     @_data3
                      :criteria @*criteria-meta
                      :axes     @_axes
                      :temp     @*temp
                      :api api})

        ]

    (js/setInterval #(swap! *temp sys-magnet) 16)
    {:_spec _spec
     :chart-comp (partial chart _spec)}))




;; Magnet system
(defn displacement [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])
(defn magnet [current desired]
  (if (nil? current)
    desired
    (let [d (- desired current)]
      (if (< (Math.abs d) 0.2)
        desired
        (+ current (/ d 20))))))
(defn magnet-coll [current desired]
  (if current
    (mapv (fn [c d]
            (if (sequential? d)
              (magnet-coll c d)
              (magnet c d)))
          current desired)
    desired))

(defn sys-magnet
  "Magnets entities to their :desired-coords"
  [old]
  (into {}
                   (map (fn [[id {:keys [desired-coords current-coords] :as s}]]
                          [id (assoc s :current-coords (magnet-coll current-coords desired-coords))])
                        old))
  #_(apply merge
         (for [[id {:keys [desired-coords current-coords] :as ent}] (cond-> old (and static* @static*) (deep-merge @static*))]
           {id (update ent :current-coords
                       (fn [current-coords]
                         (magnet-coll current-coords desired-coords)
                         ))})))







; View

;; Axes
(defn axes [{:keys [axes temp]
             {{:keys [inspect]} :criteria} :api
             [field-w field-h] :range}]
  [:g.axes
   (doall
    (for [[[axis? val-path] ent] temp
          :when (and (= :axis axis?) (:in-display? ent))
          :let [{[[x1 y1] [x2 y2]] :current-coords} ent]]
      ^{:key (pr-str val-path)}
      [:g
       [:line {:id (pr-str val-path)
               :x1 x1 :y1 y1
               :x2 x2 :y2 y2
               :on-mouse-enter #(inspect val-path true)
               :on-mouse-leave #(inspect val-path false)
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


;; DPS
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

(defn vec2+ [[a1 b1] [a2 b2]]
  [(+ a1 a2) (+ b1 b2)])

(defn data-points [{:keys [range axes data dps temp]
                    {{:keys [select inspect]} :data} :api}]
  [:g.data-points
   (if (= 2 (count (filter :in-display? axes)))
     (for [[ent-id raw-dps] (group-by (comp first first) temp)
           :when (not= ent-id :axis)
           :let [{:keys [in-select? in-inspect? color] :as dat} (get data ent-id)
                 dps (->> raw-dps
                          vals
                          (filter :in-display?))
                 all-coords (map :current-coords dps)
                 [x y] (reduce vec2+ [0 0] all-coords)
                 ]]
       ^{:key ent-id}
       [:g.dp {:transform (gstr/format "translate(%s, %s)" x y)
               :class (str (when in-select? "in-select ")
                           (when in-inspect? "in-inspect "))}
        [:circle.dot {:r 5
                      :fill color
                      :stroke color
                      :on-mouse-enter #(inspect ent-id true)
                      :on-mouse-leave #(inspect ent-id false)
                      :on-click #(select ent-id)}]
        [info dat]])

     (for [[ent-id raw-dps] (group-by (comp first first) temp)
           :when (not= ent-id :axis)
           :let [{:keys [in-select? in-inspect? color]} (get data ent-id)
                 dps (->> raw-dps
                          (filter (comp :in-display? val))
                          (sort-by (comp :val-path val)))
                 polygon (->> dps
                              vals
                              (map (fn [{[x y] :current-coords}]
                                     (str x "," y)))
                              (clojure.string/join " "))
                                        ;color (id->color id)
                 ]]
       ^{:key ent-id}
       [:g.entity-polygon {:class (str (when in-inspect? "in-inspect ")
                                       (when in-select? "in-select "))}
        [:polygon {:on-click #(select ent-id)
                   :on-mouse-enter #(inspect ent-id true)
                   :on-mouse-out #(inspect ent-id false)
                   :points polygon
                   :fill color
                   :stroke color}]
        (for [[dp-id {[x y] :current-coords #_desired-coords
                        :keys [domain-val in-inspect?]}] dps]
            ^{:key dp-id}
            [:g.criteria {:class (when in-inspect? "in-inspect")
                          :transform (gstr/format "translate(%s, %s)" x y)}
             [:circle {:r 3 :fill color}]
             [:text {:fill "#404040"} (pr-str domain-val)]]
            )]))
     ])


(defn chart [_spec]
  (let [{[x y] :range :as spec} @_spec]
    [:svg {:width x
           :height y}
     [axes spec]
     [data-points spec]]))
