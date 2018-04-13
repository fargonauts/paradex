(ns paradex.kernel.central
  (:require 
    [paradex.kernel.workspace.workspace :refer :all]
    [paradex.kernel.coderack.coderack   :refer :all]
    [paradex.kernel.slipnet.slipnet     :refer :all]
    [paradex.kernel.library             :refer :all]))

(defrecord Central [slipnet workspace coderack library iterations])

(defn init-central []
  (atom (Central. 
          (init-slipnet)
          (init-workspace)
          (init-coderack 100)
          (init-library)
          0)))

(defn run [central domain]
  (print "Running central")
  (loop []
    (when-not (empty? (:codelets (:coderack @central)))
      (coderack-step (:library @central) central)
      (recur))))

;(def-clet "x" [central n] (do (println (str "ran x" n))))
;(def-clet "y" [central n] (do (println (str "ran y" n))))
;(def-clet "z" [central n] (do (println (str "ran z" n))))
;
;(def-clet "status" [central] (do (println "status!") (println central)))
;(def-clet "slipnet-update" [central] (slipnet-update central))
;
;(def central (create-central))
;
;;(defn post [central codelet-type category urgency & args]
;
;(defn initialize [central]
;  (post central :x nil 1 200)
;  (post central :x nil 1 300)
;  (create-updater central :status)
;  (create-updater central :slipnet-update)
;  (create-node central "a" 100 90 [:x])
;  (create-node central "b" 100 90 [:x])
;  (create-link central "a" "b" nil nil 100 true))
;
;(def-clet "even-odd-split" [central]
;  (let [ws    (:workspace @central)
;        left  (:left ws)
;        right (:right ws)]
;    (if (and (every? odd? left)
;             (every? even? right))
;      (println "Even-odd split detected")
;      (println "Unknown sequence"))))
;
;
;(defn initialize-nbongard [central]
;  (add-codelet central :even-odd-split 1)
;  (add-object  central [:left] '(1 3 5))
;  (add-object  central [:left] '(2 4 6)))
