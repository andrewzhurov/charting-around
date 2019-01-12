(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as ra]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :as logic :refer [state stages >evt <sub]]
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



;; Defs
(def p-field [400 400])
(def center-coords [(/ (get p-field 0) 2) (/ (get p-field 1) 2)])
(def min-pad 20) ; % of domain
(def max-pad 10) ; % of domain



;; Logic
(defn calc-angle [[x y]]
  (* (js/Math.atan2 y x) (/ 180 js/Math.PI)))

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

(declare decide-on-viz)
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

(def current-participants (ra/reaction (:participants (logic/current-race @logic/state))))

(def participants-meta (r/atom {}))
(def p (ra/reaction (apply merge (map-indexed (fn [idx [id a-p]]
                                                {id (-> a-p
                                                        (merge (get @participants-meta id))
                                                        (assoc :color (get colors idx)))})
                                              @current-participants))))

(def criteria-meta (r/atom {[:production-year] {:in-display? false
                                                :in-inspect? false
                                                ;:domain [2000 2020]
                                                }
                            [:top-speed]       {:in-display? true
                                                :in-inspect? false
                                                ;:domain [0 300]
                                                }
                            [:transmission]    {:in-display? true
                                                :in-inspect? false
                                                ;:domain [3 8]
                                                }
                            [:horsepower]      {:in-display? true
                                                :in-inspect? false
                                                ;:domain [50 500]
                                                }
                            [:weight]          {:in-display? true
                                                :in-inspect? false
                                                ;:domain [600 2000]
                                                }}))
(def inspect-criteria (fn [val-path on?] (swap! criteria-meta assoc-in [val-path :in-inspect?] on?)))
(def display-criteria (fn [val-path] (swap! criteria-meta update-in [val-path :in-display?] not)))



(def p-axes
  (ra/reaction (decide-on-viz2 @p
                               @criteria-meta
                               p-field)))

(def temp (r/atom {}))

(defn derive-dps2 [p p-axes]
  (swap! temp
               (fn [old]
                 (deep-merge old
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
                                                                {:desired-coords (if in-display? [min-viz-coords max-viz-coords] [center-coords center-coords])})})))))))

(def p-dps
  (ra/reaction (derive-dps2 @p @p-axes)))

(def p-spec
  (ra/reaction {:range p-field
                :axes @p-axes
                :data @p
                :temp @temp
                :dps @p-dps
                :events {:select  (fn [id] (>evt [:toggle-participant id]))
                         :inspect (fn [id on?] (swap! participants-meta assoc-in [id :in-inspect?] on?))}}))


(defn fill-spec [data]
  (swap! spec (fn [spec] (-> (assoc spec :data data)
                             (complete-spec)))))
#_(fill-spec (gen-data))


;; Magnet dps
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
  (apply merge
         (for [[id {:keys [desired-coords current-coords] :as ent}] old]
           {id (update ent :current-coords
                       (fn [current-coords]
                         (magnet-coll current-coords desired-coords)
                         ))})))

(js/setInterval #(swap! temp sys-magnet) 16)

;; Magent axis


#_(defn sys-axis-magnet
  "Magnets viz of axes to their :desired-vizmin :desired-viz-max"
  [spec]
  (update spec :axes (fn [axes]
                       (vec
                        (for [{:keys [min-range-viz max-range-viz] :as axis} axes]
                          (-> axis
                              (update :current-min-range-viz magnet min-range-viz)
                              (update :current-max-range-viz magnet max-range-viz)))))))

#_(js/setInterval #(swap! spec sys-axis-magnet) 16)



;; View
(defn axes [{:keys [axes dps]
             [field-w field-h] :range}]
  [:g.axes
   (doall
    (for [[[axis? val-path] ent] @temp
          :when (and (= :axis axis?) (:in-display? ent))
          :let [{[[x1 y1] [x2 y2]] :current-coords} ent]]
      ^{:key (pr-str val-path)}
      [:g
       [:line {:id (pr-str val-path)
               :x1 x1 :y1 y1
               :x2 x2 :y2 y2
               :on-mouse-enter #(inspect-criteria val-path true)
               :on-mouse-leave #(inspect-criteria val-path false)
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
      (let [amount-fit (/ (- domain-end domain-start) (or tick 5))]
        (for [tick-idx (clojure.core/range (inc amount-fit))
              :let [domain-val (+ (* tick-idx tick) domain-start)
                    range-pos (->range domain-val)]
              :when (and (<= range-pos current-max-range-viz)
                         (>= range-pos current-min-range-viz))
              ]
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




(defn vec2+ [[a1 b1] [a2 b2]]
  [(+ a1 a2) (+ b1 b2)])

(defn data-points [{:keys [range dps axes data]
                    {:keys [select inspect]} :events}]
  [:g.data-points
   (if (= 2 (count (filter :in-display? axes)))
     (for [[ent-id raw-dps] (group-by (comp first first) @temp)
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

     (for [[ent-id raw-dps] (group-by (comp first first) @temp)
           :when (not= ent-id :axis)
           :let [{:keys [in-select? in-inspect? color]} (get data ent-id)
                 dps (->> raw-dps
                          vals
                          (filter :in-display?)
                          (sort-by :val-path))
                 polygon (->> dps
                              (map (fn [{[x y] :current-coords}]
                                     (str x "," y)))
                              (clojure.string/join " "))
                                        ;color (id->color id)
                 ]]
       ^{:key ent-id}
       [:g.entity-polygon {:class (str (when in-inspect? "in-inspect ")
                                       (when in-select? "in-select "))}
        [:polygon {:on-click #(select ent-id)
                   :on-mouse-over #(inspect ent-id true)
                   :on-mouse-out #(inspect ent-id false)
                   :points polygon
                   :fill color
                   :stroke color}]
        #_(for [[dp-id {[x y] :desired-coords
                        :keys [domain-val in-inspect?]}] dps]
            ^{:key dp-id}
            [:g.criteria {:class (when in-inspect? "in-inspect")
                          :transform (gstr/format "translate(%s, %s)" x y)}
             [:circle {:r 3 :fill color}]
             [:text {:fill "#404040"} (pr-str domain-val)]]
            )]))
     ])



(defn chart [{[x y] :range :as spec}]
  [:svg {:width x
         :height y}
   [axes spec]
   ;[axis (l 0 (get-in spec [:axes 0]))]
   ;[axis (l 1 (get-in spec [:axes 1]))]
   [data-points spec]])


(defn chart-panel []
  [:div
   [chart @spec]
   #_[:button {:on-click #(fill-spec (gen-data))} "Different dataset"]])

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

(declare participants-stage)
(defmethod stage-content :participants
  [{:keys [racers]}]
  [participants-stage]
  #_[:div.stage-content.participants
   [:div.collection.with-header
    [:div.collection-header [:h4 "Participants"]]
    (for [[pt-id {:keys [name skill participates?]}] racers]
      ^{:key pt-id}
      [:a.collection-item {:class (when participates? "active")
                           :on-click #(>evt [:toggle-participant pt-id])}
       (str name " "  skill)])]
   ])

(defn driver [{:keys [id car-name driver-name avatar skill color]}]
  [:div.participant.card {:id id
                          :draggable true
                          :on-drag-start (fn [evt] (.setData (.-dataTransfer evt) "pt-id" id))

                          :style {:border-color color
                                  :border-style "solid"}}

   [:div.driver-name driver-name]
   [:div.car-name car-name]]

  #_[:div.driver {:draggable true
                :on-drag-start (fn [evt] (.setData (.-dataTransfer evt) "pt-id" id))
                :id id}
   [:div.avatar avatar]
   [:div.driver-name driver-name]
   [:div.car-name car-name]
   [:div.skill skill]])

(defn bets-stage [state]
  (let [drag? (r/atom false)]
    (.addEventListener js/window "drop" (fn [_] (js/console.log "BLAH!")))
    (fn [state]
      (let [race (logic/current-race state)
            pts (into {} (filter (comp :in-select? val) @p))
            betted-pts (set (map (comp :pt-id val) (:bets race)))
            bets (:bets race)
            do-drag! #(reset! drag? true)
            done-drag! #(reset! drag? false)
            ]
        [:div#bets.stage-content {:class (when @drag? "dragging")
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

         [:ul.bets
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
                                          :on-change #(>evt [:place-bet place place pt-id (.-value (.-target %))])}])
             [:div.drop-overlay
              [:text (case place
                       1 "1st"
                       2 "2nd"
                       3 "3rd"
                       (str place "th"))]]])]
         [:button.btn.to-race {:on-click #(do (l "BLAH:" 22) (>evt [:to-stage :race]))} "SEE RACE"]]))))

(defmethod stage-content :bets
  [state]
  [bets-stage state])

(defmethod stage-content :race
  [state]
  [:div#race.stage-content "RACE PROGRESS"
   (pr-str (<sub [:race-results]))
   (pr-str (<sub [:bet-results]))])
(defmethod stage-content :results
  [state]
  [:div#results.stage-content "RESULTS"
   (str (<sub [:wins-history]))])

(defn stage []
  (let [stage-id (:stage @state)
        {:keys [name next prev]} (get stages stage-id)]
    [:div.stage
     [stage-content @state]
     (when prev [:button.prev {:on-click #(>evt [:to-stage prev])} prev])
     (when next [:button.next {:on-click #(do (>evt [:to-stage next])
                                              (when (= :race next)
                                                (>evt [:race])))} next])]))


;; TODO inspect
(defn criteria-list []
  [:div.criteria-list
   (for [[val-path {:keys [in-display?]}] (sort-by key @criteria-meta)]
     ^{:key val-path}
     [:div.criteria {:class (when in-display? "in-display")
                     :on-mouse-enter #(inspect-criteria val-path true)
                     :on-mouse-leave #(inspect-criteria val-path false)

                     :on-click #(display-criteria val-path)
                     }
      (pr-str val-path)]
     )])

(defn participants-list [{{:keys [inspect select]} :events}]
  [:div.participants-list
   (doall
    (for [{:keys [id driver-name car-name color in-select? in-inspect?]} (vals @p)]
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


;;
(def game (ra/reaction
           (let [participant-ids (keys (filter (comp :in-select? val) @participants-meta))]
             )))

(defn participants-stage []
  [:div.participants-stage
   [radar-chart @p-spec]
   [criteria-list]
   [participants-list @p-spec]
   [:button.btn {:on-click #(>evt [:regen-participants])} "SHOW ME OTHERS"]
   [:button.btn {:on-click #(when (<sub [:fine-to-bet?]) (>evt [:to-stage :bets]))
                 :class (when-not (<sub [:fine-to-bet?]) "disabled")
                 :title (when-not (<sub [:fine-to-bet?]) "Race needs at least 2 participants for it to be... a race.")}
    "PLACE BETS"]])


(defn race-stage []
  [:div "race stage"])

(defn root []
  #_(l "SPEC:" @p-spec)
  [:div#root
   [:style (garden.core/css charting-around.css/styles)]
   [:link {:rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"}]

   [stage-content @state]
   ])

(r/render [root]
          (document.getElementById "app"))


