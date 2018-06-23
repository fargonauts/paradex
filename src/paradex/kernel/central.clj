(ns paradex.kernel.central
  (:require 
    [paradex.kernel.workspace.workspace :refer :all]
    [paradex.kernel.coderack.coderack   :refer :all]
    [paradex.kernel.slipnet.slipnet     :refer :all]
    [paradex.kernel.library             :refer :all]))

(defrecord Central [slipnet workspace coderack library iterations])

(defn init-central [domain-init]
  (if (nil? domain-init)
      (atom (Central. 
            (init-slipnet)
            (init-workspace)
            (init-coderack 100)
            (init-library)
            0))
      (domain-init)))

(defn run [central]
  (println "Running central")
  (loop []
    (when-not (empty? (:codelets (:coderack @central)))
      (println @central)
      (coderack-step central)
      (recur))))
