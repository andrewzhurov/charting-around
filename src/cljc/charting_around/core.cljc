(ns charting-around.core
  (:require [clojure.string]
            #?(:cljs [reagent.core :as r])
            #?(:cljs [goog.string :as gstr])))

(def app-state #?(:clj (atom {}) :cljs (r/atom {})))

#?(:cljs (def format gstr/format))

(defn l [desc expr]
  (println desc expr)
  expr)

(defn deep-merge [& colls]
  (if (not (every? map? colls))
    (last colls)
    (apply merge-with deep-merge colls)))

(defn scale-linear [[domain-start domain-end] range]
  (fn [domain-val]
    (* range (/ (- domain-val domain-start) (- domain-end domain-start)))))

(def spec
  {:range [800 300]
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
           :val-path [:top-speed]
           }}

   :data {:vp {:car-name "Volkswagen Polo"
               :production-year 2018
               :top-speed 200}

          :jmm {:car-name "Jaguar MM"
                :production-year 2017
                :top-speed 280}

          :si {:car-name "Subaru impreza"
               :production-year 2016
               :top-speed 250}}

   :derived-data {:vp {:production-axis {}
                       :speed-axis {}}
                  :jmm {:production-axis {}}
                  :si {:production-axis {}}}
   })

(defn calc-angle [[x y]]
  (* (Math/atan2 y x) (/ 180 Math/PI)))

(defn ready-axis [{:keys [domain] :as axis} [[begin-x begin-y] [end-x end-y] :as coords]]
  (let [displacement [(- end-x begin-x) (- end-y begin-y)]
        range (Math/hypot (first displacement) (second displacement))]
    (merge axis {:coords coords
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
  (let [new-derived-data (->> (for [[dp-id dp] (:data spec)
                                    [axis-id {:keys [val-path ->range ->coords]}] (:axes spec)
                                    :let [range (->range (get-in dp val-path))]]
                                {dp-id {axis-id {:range range
                                                 :coords (->coords range)}}})
                              (apply deep-merge))]
    (assoc spec :derived-data new-derived-data)))

(def spec*
  (-> spec
      (decide-on-viz)
      (derive-dts)))

(defn make-state []
  spec*)

(defn update-top-speeds [state {n :n}]
  (-> state
    (assoc-in [:data :vp :top-speed] (- 200 n))
    (assoc-in [:data :jmm :top-speed] (mod n 300))
    (assoc-in [:data :si :top-speed] (+ 100 (* 100 (Math/cos (/ 200 n)))))
    decide-on-viz
    derive-dts))

(defn update-fn [message]
  update-top-speeds)

(defn handle-message!
  ([message]
    (handle-message! message (update-fn message)))
  ([message f]
    (swap! app-state (fn [s] (f s message)))))


