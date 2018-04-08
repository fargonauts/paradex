(ns paradex.kernel.formulas
  (:require [paradex.kernel.base :refer [wrand drop-nth]]))

(defn weighted-pick [coll]
  (let [index      (wrand (map first coll)) 
        [_ picked] (nth coll index)
        remaining  (drop-nth index coll)]
    [picked remaining]))

(defn slipnet-decay-formula [activation depth]
  (max 0 (- activation (int (* activation (/ (- 100 depth) 100))))))

(defn slipnet-urgency-post-formula [activation]
  (quot activation 10))

(defn slipnet-activation-post-threshold [activation]
  (> activation 50))
