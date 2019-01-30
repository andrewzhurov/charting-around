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

(defn expect [sub to-match]
  (assert to-match (<sub sub)))




;; WIP on tests
;; Maybe I should not write them for sketches
(deftest logic-test
  (testing "Base test"
    (>evt [:to-presentation :granular])
    (>evt [:gen-node {:pt-id1 {:skill 0}
                      :pt-id2 {:skill 1}}])
    (>evt [:gen-node {:pt-id1 {:skill 0}
                      :pt-id2 {:skill 1}}])
    (expect [:nodes]
            {1 {:began? true
                :complete 0}
             2 {:began? false}})
    (expect [:subnodes 1]
            {1 {:type :chart
                :active? true
                :complete? false}
             2 {:type :quiz
                :text string?
                :active? false
                :complete? false}
             3 {:type :quiz}})
    (>evt [:submit])
    (expect [:subnodes 1]
            {1 {:type :chart
                :active? false
                :complete? true}
             2 {:active? true}})

    "Submit question answer
     'Who finishes 1st?'
     Show hint on wrong answer
     On wrong answer stay and explain fail reason"
    (expect [:variants [1 2]]
            {1 {:question-text string?
                :explanation-text string?
                :to-select? true
                :selected? false}
             2 map?})
    (>evt [:submit :pt-id1])
    (expect [:variants [1 2]]
            {:pt-id2 {:submitted? true}})
    (expect [:subnodes 1]
            {2 {:active? true
                :complete? false}})

    (>evt [:submit :pt-id1])
    (expect [:variants [1 2]]
            {:pt-id1 {:submitted? true}
             :pt-id2 {:submitted? true}})
    (expect [:subnodes 1]
            {2 {:active? false
                :complete? true}
             3 {:active? true}})

    (>evt [:to-node ])


    #_(assert [1 1] (<sub [:]))))

#_(deftest logic-test
  (testing "Drag and drop way of display testing"
    (>evt [:to-presentation :drag-and-drop])
    (reset! state (assoc-in init-state
                            [:races 1 :participants]
                            {:rokko {:name "Rokko" :skill 0.6}
                             :kokko {:name "Kokko" :skill 1}
                             :mokko {:name "Mokko" :skill 0.8}}))

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
