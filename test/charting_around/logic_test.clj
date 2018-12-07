(ns charting-around.logic-test
  (:require  [clojure.test :refer [deftest testing is run-tests]]
             [charting-around.logic :refer :all]
             #_[brawl-haus.subs :refer [derive]]
             [matcho.core :refer [assert match]]
             ))


(defn wait [db ms]
  (Thread/sleep ms)
  db)

(defn failed-test-line-number []
  (let [trace (->> (.. Thread currentThread getStackTrace)
                   (map (fn [el] (str (.getFileName el) ":" (.getLineNumber el))))
                   vec)]
    (println (get trace 5))))

(defn expect [db an-assert {:keys [conn-id sub]}]
  (when-not (assert an-assert
                    (derive db sub conn-id))
    #_(failed-test-line-number))
  db)


(reset-state!)
(deftest logic-test
  (testing "BLAH"
    (>evt [:add-participant {:id "Rokko" :skill 0.6}])
    (>evt [:add-participant {:id "Mokko" :skill 0.8}])
    (>evt [:add-participant {:id "Kokko" :skill 1}])
    (>evt [:add-bet {:bet-id :bet1 :on "Rokko" :bet-place 3 :chance 80}])
    (>evt [:add-bet {:bet-id :bet2 :on "Mokko" :bet-place 1 :chance 90}])
    (>evt [:race])
    (is (= 1 2))
    (assert {:bet1 {:on "Rokko"
                    :guessed? true
                    :bet-place 3
                    :actual-place 3
                    :money 80}
             :bet2 {:on "Mokko"
                    :guessed? false
                    :bet-place 1
                    :actual-place 2
                    :money 90}} (<sub [:results]))))



(run-tests 'charting-around.logic-test)
