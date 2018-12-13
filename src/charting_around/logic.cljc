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

(defn gen-uuid [] #?(:cljs (str (random-uuid))
                     :clj  (str (java.util.UUID/randomUUID))))

(defn gen-racers []
  (apply deep-merge
         (repeatedly 6 (fn [] (let [dr-id (gen-uuid)]
                                {dr-id {:id dr-id
                                        :name (rand-nth ["Jack" "Monroe" "Tall" "Bob" "Matt"])
                                        :skill (rand-nth [0.5 0.6 0.7 0.8 0.9])}})))))


(def init-state {:stage :participants
                 :racers (gen-racers)
                 :wins-history []})

(def state #?(:cljs (reagent.core/atom init-state)
              :clj (atom init-state)))


;;
(defmulti drive (fn [_ [evt-id]] evt-id))
(defmethod drive :toggle-participant
  [state [_ pt-id]]
  (update-in state [:racers pt-id :participates?] not))

(defmethod drive :to-stage [state [_ stage-id]] (assoc state :stage stage-id))
(defmethod drive :place-bet [state [_ pt-id {:keys [place chance]}]]
  (assoc-in state [:racers pt-id :bet] {:place place
                                        :chance chance}))

(defmethod drive :race [{:keys [racers] :as state} _]
  (let [results (->> racers
                     (sort-by (comp :skill val))
                     reverse
                     (map-indexed (fn [idx [id part]]
                                    (let [place (inc idx)]
                                      {id (deep-merge part
                                                      {:place place}
                                                      {:bet (when-let [bet-place (get-in part [:bet :place])]
                                                              {:guessed? (= bet-place place)})})})))

                     (apply deep-merge))
        money-win (reduce (fn [acc-sum [_ {{:keys [guessed? chance]} :bet}]]
                            (cond (nil? chance) acc-sum
                                  guessed? (+ acc-sum chance)
                                  :not-gussed (- acc-sum chance)))
                          0 results)]
    (-> state
        (deep-merge {:racers results})
        (update :wins-history conj money-win))))

(defmethod drive :default [state evt]
  (println "No EVT" evt)
  state)

(defn >evt [evt]
  (swap! state drive evt))


;;
(def subscriptions
  {:results (fn [{:keys [racers]} _]
              (into {} (filter (comp :bet val) racers)))
   :wins-history :wins-history})

(defn <sub [[sub-id :as sub]]
  (if-let [sub-fn (get subscriptions sub-id)]
    (sub-fn @state sub)
    (println "No SUB" sub)))

(defn -main [] (println "BLAH"))
