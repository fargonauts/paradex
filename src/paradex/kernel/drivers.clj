(ns paradex.kernel.drivers
  (:require 
    [paradex.kernel.workspace :refer :all]
    [paradex.kernel.coderack  :refer :all]
    [paradex.kernel.slipnet   :refer :all]))

(defn create-central []
  (atom {:slipnet   {:nodes {} :links {}}
         :workspace {}
         :coderack  {:codelets '()
                     :updaters '()}}))

(def codelet-library (atom {}))

(defmacro def-clet [id args body]
  `(def-codelet codelet-library ~id ~args ~body))

(def-clet "x" [central] (do (println "ran x") ));(add-codelet central :y 1)))
(def-clet "y" [central] (do (println "ran y") ));(add-codelet central :z 1))) 
(def-clet "z" [central] (do (println "ran z") ));(add-codelet central :y 1) (add-codelet central :y 2)))

(def-clet "status" [central] (do (println "status!") (println central)))
(def-clet "slipnet-update" [central] (slipnet-update central))

(def central (create-central))

(defn initialize [central]
  (add-codelet central :x 1)
  (add-codelet central :x 1)
  (add-updater central :status)
  (add-updater central :slipnet-update)
  (create-node central "a" 100 90 [:x])
  (create-node central "b" 100 90 [:x])
  (create-link central "a" "b" nil nil 100 true))

(def-clet "even-odd-split" [central]
  (let [ws    (:workspace @central)
        left  (:left ws)
        right (:right ws)]
    (if (and (every? odd? left)
             (every? even? right))
      (println "Even-odd split detected")
      (println "Unknown sequence"))))


(defn initialize-nbongard [central]
  (add-codelet central :even-odd-split 1)
  (add-object  central [:left] '(1 3 5))
  (add-object  central [:left] '(2 4 6)))

(defn main-loop []
  (initialize central)
  ;(initialize-nbongard central)
  (println central) 
  (loop []
    (let [picked (pick-codelet central)]
      ;(println picked)
      (run-codelet codelet-library picked central)
      (run-updates codelet-library central))
      ;(println central))
    (if (empty? (:codelets (:coderack @central)))
      (println "Empty coderack, finishing..")
      (recur))))
