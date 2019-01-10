(ns charting-around.logic
  (:require [clojure.set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check.generators]
            #?(:cljs [reagent.core])))

(defn l [desc expr] (println desc expr) expr)
(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (apply merge-with deep-merge colls)))

(def stages {:participants
             {:name "Participants"
              :prev nil
              :next :bets}
             :bets
             {:name "Bets"
              :prev :participants
              :next :race}
             :race
             {:name "Race"
              :prev nil
              :next :results}
             :results
             {:name "Results"
              :prev nil
              :next :participants}})

(defn gen-uuid [] #?(:cljs (keyword (str (random-uuid)))
                     :clj  (keyword (str (java.util.UUID/randomUUID)))))

(defn rand-in
  "Int in range, inclusive"
  [left-bound right-bound]
  (+ left-bound (rand-int (inc (- right-bound left-bound)))))

(defn gen-participants []
  (array-map
   :vp {:id :vp
        :driver-name "Dallas"
        :car-name "Porshe 911"
        :production-year (rand-in 2014 2018)
        :top-speed (rand-in 150 220)
        :transmission (rand-in 4 8)
        :horsepower (rand-in 150 280)
        :weight (rand-in 1200 1400)
        :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :jmm {:id :jmm
         :driver-name "Paul"
         :car-name "Jaguar MM"
         :production-year (rand-in 2013 2017)
         :top-speed (rand-in 180 240)
         :transmission (rand-in 4 8)
         :horsepower (rand-in 300 400)
         :weight (rand-in 1300 1600)
         :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su {:id :su
        :car-name "Subaru impreza"
        :driver-name "No face 1"
        :production-year (rand-in 2016 2018)
        :top-speed (rand-in 220 260)
        :transmission (rand-in 4 8)
        :horsepower (rand-in 250 400)
        :weight (rand-in 900 1300)
        :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su2 {:id :su2
         :car-name "Subaru impreza"
         :driver-name "No face 2"
         :production-year (rand-in 2016 2018)
         :top-speed (rand-in 220 260)
         :transmission (rand-in 4 8)
         :horsepower (rand-in 250 400)
         :weight (rand-in 900 1300)
         :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su3 {:id :su3
         :car-name "Subaru impreza"
         :driver-name "No face 3"
         :production-year (rand-in 2016 2018)
         :top-speed (rand-in 220 260)
         :transmission (rand-in 4 8)
         :horsepower (rand-in 250 400)
         :weight (rand-in 900 1300)
         :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])})
  )

(def init-state {:stage :participants
                 :current-race-id 1
                 :races {1 {:participants (gen-participants)
                            :bets {}
                            :race-result {}}}
                 })
(defn current-race [state]
  (get-in state [:races (:current-race-id state)]))
(defn current-participants [state]
  (filter (comp :in-select? val) (:participants (current-race state))))

(def state #?(:cljs (reagent.core/atom init-state)
              :clj (atom init-state)))

;;
(defmulti drive (fn [_ [evt-id]] evt-id))
(defmethod drive :toggle-participant
  [state [_ pt-id]]
  (update-in state [:races (:current-race-id state) :participants pt-id :in-select?] not))

(defmethod drive :to-stage [state [_ stage-id]] (assoc state :stage stage-id))

;; Bets logic a bit bloody
(defmethod drive :cancel-bet [state [_ pt-id]]
  (update-in state [:races (:current-race-id state) :bets]
             (fn [bets]
               (into {} (remove (fn [[bet-id {bet-pt-id :pt-id}]] (= bet-pt-id pt-id)) bets)))))

(defmethod drive :place-bet [state [_ bet-id place pt-id chance]]
  (update-in state [:races (:current-race-id state) :bets]
             (fn [bets]
               (let [without-the-pt (->> bets
                                         (remove (fn [[bet-id {bet-pt-id :pt-id
                                                               bet-place :place}]] (or (= bet-pt-id pt-id)
                                                                                       (= bet-place place))))
                                         (into {})
                                         )
                     new-bet-id (or bet-id (gen-uuid))]
                 (assoc without-the-pt bet-id {:pt-id pt-id
                                               :place place
                                               :chance chance})))))

(defmethod drive :default [state evt]
  (println "No EVT" evt)
  state)

(defn >evt [evt]
  (swap! state drive evt))


;;
(declare derive-node)
(def subscriptions
  {:race-results (fn [state [_ race-id]]
                   (let [race (get-in state [:races race-id])]
                     (->> (:participants race)
                          (sort-by (comp :skill val))
                          reverse
                          keys
                          (map-indexed (fn [idx pt-id]
                                         {(inc idx) pt-id}))
                          (apply merge))))
   :bet-results (fn [state [_ race-id]]
                  (let [bets (get-in state [:races race-id :bets])
                        rr (derive-node state [:race-results race-id])]
                    (reduce
                     (fn [acc [bet-id {:keys [pt-id place chance]}]]
                       (assoc acc bet-id (if (= (get rr place) pt-id)
                                           chance (- chance))))
                     {}
                     bets)))
   :fine-to-bet? (fn [state _]
                   (>= (count (current-participants state)) 2))})

(defn derive-node [state [sub-id :as sub]]
  (if-let [sub-fn (get subscriptions sub-id)]
    (sub-fn state sub)
    (println "No SUB" sub)))

(defn <sub [sub]
  (derive-node @state sub))

(defn -main [] (println "BLAH"))
