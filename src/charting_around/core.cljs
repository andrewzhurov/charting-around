(ns charting-around.core
  (:require [reagent.core :as r]
            [reagent.ratom :as rr]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.logic :refer [state stages >evt <sub]]
            [charting-around.css]
            ))

(enable-console-print!)

(defn l [desc expr]
  (js/console.log desc expr)
  expr)

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
  {:vp {:car-name "Volkswagen Polo"
        :production-year (rand-in 2014 2018)
        :top-speed (rand-in 150 220)}

   :jmm {:car-name "Jaguar MM"
         :production-year (rand-in 2012 2017)
         :top-speed (rand-in 180 240)}

   :si {:car-name "Subaru impreza"
        :production-year (rand-in 2016 2018)
        :top-speed (rand-in 220 260)}})


;; Logic
(defn calc-angle [[x y]]
  (* (js/Math.atan2 y x) (/ 180 js/Math.PI)))

(defn ready-axis [{:keys [domain] :as axis} [[begin-x begin-y] [end-x end-y] :as coords]]
  (let [displacement [(- end-x begin-x) (- end-y begin-y)]
        range (js/Math.hypot (first displacement) (second displacement))]
    (merge axis {:coords coords
                 :hide-unused true
                 :displacement displacement
                 :range range
                 :angle (calc-angle displacement)
                 :->range (scale-linear domain range)
                 :->coords (fn [range-val]
                             (let [ratio (/ range-val range)]
                               (mapv #(+ (* ratio %)
                                         %2)
                                     displacement
                                     (first coords))))})))

(defn decide-on-viz [{:keys [axes] :as spec
                      [size-x size-y] :range}]
  (cond
    (= 1 (count axes)) (update spec :axes (fn [axes]
                                            (let [[f-id f-axis] (first axes)
                                                  f-coords [[0 size-y] [size-x size-y]]
                                                  ]
                                              {f-id (ready-axis f-axis f-coords)
                                               })))

    (= 2 (count axes)) (update spec :axes (fn [axes]
                                            (let [[f-id f-axis] (first axes)
                                                  f-coords [[0 size-y] [size-x size-y]]
                                                  [s-id s-axis] (second axes)
                                                  s-coords [[0 size-y] [0 0]]
                                                  ]
                                              {f-id (ready-axis f-axis f-coords)
                                               s-id (ready-axis s-axis s-coords)
                                               })))
    ))

(defn derive-dts [spec]
  (let [new-dps (->> (for [[dp-id dp] (:data spec)
                           [axis-id {:keys [val-path ->range ->coords]}] (:axes spec)
                           :let [range (->range (get-in dp val-path))
                                 coords (->coords range)]]
                       {dp-id {axis-id {:range range
                                        :desired-coords coords}}})
                     (apply deep-merge))]
    (update spec :dps deep-merge new-dps)))

(defn complete-spec [spec]
  (-> spec
      (decide-on-viz)
      (derive-dts)))




(def spec (r/atom {:range [800 300]
                   :axes {:production-axis
                          {:domain [2012 2020]
                           :scale scale-linear
                           :tick 1
                           :desc "Year"
                           :val-path [:production-year]}

                          :speed-axis
                          {:domain [0 300]
                           :scale scale-linear
                           :tick 30
                           :desc "Top speed (km/h)"
                           :desc-side :right
                           :val-path [:top-speed]
                           }}

                   :ents {:ent-id {:comp :param}}
                   }))

(defn fill-spec [data]
  (swap! spec (fn [spec] (-> (assoc spec :data data)
                             (complete-spec)))))
(fill-spec (gen-data))

(defn displacement [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])
(defn sys-magnet
  "Magnets entities to their :desired-coords"
  [spec]
  (update spec :dps (fn [dps]
                      (apply deep-merge
                       (for [[ent-id axes-results] dps
                             [axis-id {:keys [desired-coords current-coords] :as result}] axes-results]
                         {ent-id {axis-id (assoc result :current-coords (if (nil? current-coords)
                                                                          desired-coords
                                                                          (let [[dx dy] (displacement desired-coords current-coords)
                                                                                [current-x current-y] current-coords]
                                                                            [(+ current-x (/ dx 10)) (+ current-y (/ dy 10))])))}})))))

(js/setInterval #(swap! spec sys-magnet) 16)



;; View
(defn axis [{:keys [range angle tick desc desc-side ->range]
             [domain-start domain-end] :domain
             [[begin-x begin-y] [end-x end-y]] :coords :as all}]
  (let [label-offset (case desc-side
                       :right 10
                       -10) ;; TODO
        ]
    [:g.axis {:transform (gstr/format "rotate(%s, %s, %s) translate(%s, %s)" angle begin-x begin-y begin-x begin-y)}
     [:line {:x1 0 :y1 0
             :x2 range :y2 0}]
     [:path.pointer {:d (gstr/format "M%s %s L%s %s L%s %s" (- range 15) -5 range 0 (- range 15) 5)}]
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

(defn data-points [{:keys [dps axes]}]
  [:g.data-points
   (if (= 2 (count axes))
     (for [[dp-id axes-result] dps
           :let [x (-> axes-result vals first :current-coords first)
                 y (-> axes-result vals second :current-coords second)]]
       ^{:key dp-id}
       [:circle {:cx x :cy y :r 5}])
     (doall
      (for [[dp-id axes-result] dps
            [axis-id {[x y] :coords}] axes-result]
        ^{:key [dp-id axis-id]}
        [:circle {:cx x :cy y :r 5}])))])

(defn svg [{[x y] :range :as spec}]
  (conj [:svg {:width x
               :height y}]
        [axis (get-in spec [:axes :production-axis])]
        [axis (get-in spec [:axes :speed-axis])]
        [data-points spec]))


(defn chart []
  [:div
   [svg @spec]
   [:button {:on-click #(fill-spec (gen-data))} "Different dataset"]])

(defn stats []
  [:div.stats (pr-str @state) "STATS:" [chart]])

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
         betted-drivers true} (l 0 (group-by (comp boolean :bet val) pts))]
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



(defn root []
  [:div#root
   [:style (garden.core/css charting-around.css/styles)]
   [:link {:rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"}]

   [stage]
   ])

(r/render [root]
          (.-body js/document))


