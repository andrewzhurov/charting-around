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


(reset! state (assoc-in init-state
                        [:races 1 :participants]
                        {:rokko {:name "Rokko" :skill 0.6}
                         :kokko {:name "Kokko" :skill 1}
                         :mokko {:name "Mokko" :skill 0.8}}))

(deftest logic-test
  (testing "BLAH"
    (assert false (<sub [:fine-to-bet?]))
    (>evt [:toggle-participant :rokko])
    (assert false (<sub [:fine-to-bet?]))
    (>evt [:toggle-participant :mokko])
    (assert true (<sub [:fine-to-bet?]))
    (>evt [:toggle-participant :kokko])
    (>evt [:place-bet :bet1 3 :rokko 80])
    (>evt [:place-bet :bet2 1 :mokko 90])
    ;(>evt [:race])
    nil?
    (assert {:bet1 80
             :bet2 -90} (<sub [:bet-results 1]))
    (>evt [:place-bet :bet3 1 :kokko 50])
    (assert ^:matcho/strict
            {:bet1 80
             :bet3 50} (<sub [:bet-results 1]))
    (>evt [:cancel-bet :kokko])
    (assert ^:matcho/strict
            {:bet1 80
             } (<sub [:bet-results 1]))

    ))




(run-tests 'charting-around.logic-test)
