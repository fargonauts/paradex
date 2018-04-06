(ns paradex.kernel.drivers
  (:require 
    [paradex.kernel.workspace :refer :all]
    [paradex.kernel.coderack  :refer :all]
    [paradex.kernel.slipnet   :refer :all]))

(declare x)
(declare y)

(def coderack (atom {:codelets '([1 "x"])}))
(def codelet-library (atom {}))

(def-codelet codelet-library "x" (do (println "ran x") (add-codelet coderack :y 1)))
(def-codelet codelet-library "y" (do (println "ran y") (add-codelet coderack :z 1)))
(def-codelet codelet-library "z" (do (println "ran z") (add-codelet coderack :x 1)))

(defn main-loop []
  (if false
    nil
    (do
      (let [picked (pick-codelet coderack)]
        (do 
          (println picked)
          (run-codelet codelet-library picked)
          (println @coderack)
          (recur))))))
