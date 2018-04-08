(ns paradex.kernel.formulas
  (:require [paradex.kernel.base :refer [wrand drop-nth]]))

(defn weighted-pick [coll]
  (let [index      (wrand (map first coll)) 
        [_ picked] (nth coll index)
        remaining  (drop-nth index coll)]
    [picked remaining]))

(defn slipnet-decay-formula [activation depth]
  (max 0 (- activation (* activation (/ 100 (- 100 depth))))))
