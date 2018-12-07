(ns charting-around.logic
  (:require [clojure.set]))

(defn l [desc expr] (println desc expr) expr)
(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (apply merge-with deep-merge colls)))

(def stages [:participants
             :bets
             :race
             :results])

(def init-state {:stage (first stages)
                 :participants []
                 :bets []})
(def state (atom nil))
(defn reset-state! [] (reset! state init-state))
(reset-state!)

;;
(defmulti drive (fn [_ [evt-id]] evt-id))
(defmethod drive :add-participant
  [state [_ participant]]
  (update state :participants conj participant))

(defmethod drive :to-bets [state _] (assoc state :stage :bets))
(defmethod drive :add-bet [state [_ bet]]
  (update state :bets conj bet))
(defmethod drive :race [{:keys [participants bets] :as state} _]
  (let [race-results (->> participants
                          (sort-by :skill)
                          reverse
                          (map-indexed (fn [idx part] {:actual-place (inc idx)
                                                       :who (:id part)})))
        bet-results (apply merge
                           (mapv (fn [{:keys [bet-id bet-place actual-place chance] :as result}]
                                   {bet-id (merge result
                                                  {:guessed? (= bet-place actual-place)
                                                   :money chance})})
                                 (clojure.set/join race-results bets {:who :on})))]
    (merge state {:results bet-results})))

(defmethod drive :default [state evt]
  (println "No EVT" evt)
  state)

(defn >evt [evt]
  (swap! state drive evt))


;;
(def subs
  {:results :results})

(defn derive [state [sub-id :as sub]]
  (if-let [sub-fn (get subs sub-id)]
    (sub-fn state sub)
    (println "No SUB" sub)))

(defn <sub [sub]
  (derive @state sub))

(defn -main [] (println "BLAH"))
