(ns paradex.kernel.central
  (:require 
    [paradex.kernel.workspace.workspace :refer :all]
    [paradex.kernel.coderack.coderack   :refer :all]
    [paradex.kernel.slipnet.slipnet     :refer :all]
    [paradex.kernel.library             :refer :all]))

(defrecord Central [slipnet workspace coderack library iterations])

(defn init-central []
  (let [central (atom (Central. 
          (init-slipnet)
          (init-workspace)
          (init-coderack 100)
          (init-library)
          0))]
    (def-codelet central :x [central] (println "Ran x"))
    (post central :x :test-codelet 1)
    central))

(defn run [central domain]
  (println "Running central")
  (loop []
    (when-not (empty? (:codelets (:coderack @central)))
      (coderack-step central)
      (recur))))
