(ns charting-around.logic-test
  (:require  [clojure.test :refer [deftest testing is run-tests]]
             [charting-around.logic :refer :all]
             #_[brawl-haus.subs :refer [derive]]
             [matcho.core :refer [assert match]]
             ))

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


(reset! state (merge init-state
                     {:racers {:rokko {:name "Rokko" :skill 0.6}
                               :kokko {:name "Kokko" :skill 1}
                               :mokko {:name "Mokko" :skill 0.8}}}))

(deftest logic-test
  (testing "BLAH"
    (>evt [:toggle-participant :rokko])
    (>evt [:toggle-participant :mokko])
    (>evt [:toggle-participant :kokko])
    (>evt [:place-bet :rokko {:place 3 :chance 80}])
    (>evt [:place-bet :mokko {:place 1 :chance 90}])
    (>evt [:race])
    (assert {:rokko {:bet {:guessed? true}}
             :mokko {:bet {:guessed? false}}} (l 11 (<sub [:results])))
    (assert [-10]
            (<sub [:wins-history])))

  (testing "Bet swap"
    (>evt [:place-bet :rokko {:place 3}])
    (>evt [:place-bet :mokko {:place 1}])
    (assert {:rokko {:bet {:place 3}}
             :mokko {:bet {:place 1}}} (l 22 (<sub [:results])))
    (>evt [:place-bet :rokko {:place 1}])
    (assert {:rokko {:bet {:place 1}}
             :mokko {:bet {:place 3}}} (l 33 (<sub [:results])))

    ))



(run-tests 'charting-around.logic-test)
