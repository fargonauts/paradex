(ns paradex.kernel.coderack.formulas
  (:require [paradex.kernel.base :refer [wrand drop-nth]]))

(defn weighted-pick [coll]
  (let [index      (wrand (map first coll)) 
        [_ picked] (nth coll index)
        remaining  (drop-nth index coll)]
    [picked remaining]))

