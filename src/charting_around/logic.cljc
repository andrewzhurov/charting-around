(ns charting-around.logic
  (:require [clojure.set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check.generators]
            #?(:cljs [reagent.core])))

(defn l [desc expr]
  #?(:clj (do (println desc expr) expr)
     :cljs (do (js/console.log desc expr) expr)))

(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (apply merge-with deep-merge colls)))

(defn gen-uuid [] #?(:cljs (keyword (str (random-uuid)))
                     :clj  (keyword (str (java.util.UUID/randomUUID)))))

(defn rand-in
  "Int in range, inclusive"
  [left-bound right-bound]
  (+ left-bound (rand-int (inc (- right-bound left-bound)))))

(defn gen-racers []
  {:vp {:id :vp
        :driver-name "Dallas"
        :car-name "Porshe 911"
        :production-year (rand-in 2014 2018)
        :top-speed (rand-in 150 220)
        :transmission (rand-in 4 8)
        :horsepower (rand-in 150 280)
        :weight (rand-in 1200 1400)
        :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :jmm
   {:id :jmm
    :driver-name "Paul"
    :car-name "Jaguar MM"
    :production-year (rand-in 2013 2017)
    :top-speed (rand-in 180 240)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 300 400)
    :weight (rand-in 1300 1600)
    :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su
   {:id :su
    :car-name "Subaru impreza"
    :driver-name "Man1"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)
    :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su2
   {:id :su2
    :car-name "Subaru impreza"
    :driver-name "Man2"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)
    :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}

   :su3
   {:id :su3
    :car-name "Subaru impreza"
    :driver-name "Man3"
    :production-year (rand-in 2016 2018)
    :top-speed (rand-in 220 260)
    :transmission (rand-in 4 8)
    :horsepower (rand-in 250 400)
    :weight (rand-in 900 1300)
    :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}}
  )

(def init-state {:racers (gen-racers)
                 :bets {}
                 :results-presentation :wave})

(def state #?(:cljs (reagent.core/atom init-state)
              :clj (atom init-state)))

;;
(defmulti drive (fn [_ [evt-id :as evt]] (l ">" evt) evt-id))
(defmethod drive :regen-racers
  [state _]
  (update state :racers deep-merge (gen-racers)))

(defmethod drive :make-checkpoint
  [state _]
  (assoc state :checkpoint state))

(defmethod drive :to-checkpoint
  [state _]
  (if-let [checkpoint-state (:checkpoint state)]
    (assoc checkpoint-state
           :checkpoint checkpoint-state)
    (do
      #?(:cljs (js/console.log "No checkpoint state found"))
      state)))

(defmethod drive :toggle-bet
  [state [_ {:keys [place pt-id] :as bet}]]
  (update-in state [:bets place] (fn [old]
                                   (if (= (:pt-id old) pt-id)
                                     nil
                                     (merge {:chance 80}
                                            old
                                            bet)))))
(defmethod drive :bet
  [state [_ {:keys [place pt-id chance] :as bet}]]
  (update-in state [:bets place] merge bet))

(defmethod drive :set-results-presentation
  [state [_ presentation-id]]
  (assoc state :results-presentation presentation-id))


(defmethod drive :default [state evt]
  (println "No EVT" evt)
  state)

(defn >evt [evt]
  (swap! state drive evt))


;;
(declare derive-node)
(def subscriptions
  {:race-results (fn [state]
                   (let [pts (derive-node state [:participants])]
                     (->> pts
                          vals
                          (sort-by :skill)
                          reverse
                          (map-indexed (fn [idx {:keys [id]}]
                                         {(inc idx) id}))
                          (apply merge))))

   :bets-results (fn [state]
                   (let [bets (derive-node state [:bets])
                         rr (derive-node state [:race-results])]
                     (reduce
                      (fn [acc [bet-id {:keys [pt-id place chance]}]]
                        (assoc acc bet-id (if (= (get rr place) pt-id)
                                            chance (- chance))))
                      {}
                      bets)))


   :balance-history (fn [state _]
                      [{:idx 0 :balance 0}
                       {:idx 1 :balance 30}
                       {:idx 2 :balance 50}
                       {:idx 4 :balance -40}
                       {:idx 5 :balance 40}
                       {:idx 6 :balance 140}]
                      (reduce (fn [acc [idx delta]]
                                (let [{:keys [idx balance]} (last acc)]
                                  (conj acc {:idx (inc idx)
                                             :balance (+ balance delta)})))
                              [{:idx 0
                                :balance 0}]
                              (derive-node state [:bets-results])))

   :racers (fn [state _] (:racers state))
   :participants (fn [state _]
                   (into {} (filter (comp :in-select? val) (:racers state))))
   :bets (fn [state] (:bets state))})

(defn derive-node [state [sub-id :as sub]]
  #_(l "<" sub)
  (if-let [sub-fn (get subscriptions sub-id)]
    (sub-fn state sub)
    (println "No SUB" sub)))

(defn <sub [sub]
  (derive-node @state sub))

(defn -main [] (println "BLAH"))
