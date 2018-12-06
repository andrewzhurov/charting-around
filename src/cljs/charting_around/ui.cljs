(ns charting-around.ui
  (:require [reagent.core :as r]
            [garden.core]
            [goog.string :as gstr]
            [charting-around.core :as c]
            [charting-around.views :as v]))

(defn root-view
  ([] (v/root @c/app-state)))

(reset! c/app-state (c/make-state))

(r/render [root-view]
          (.-body js/document))