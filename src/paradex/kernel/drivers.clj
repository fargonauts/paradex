(ns paradex.kernel.drivers
  (:require 
    [paradex.kernel.workspace :refer :all]
    [paradex.kernel.coderack  :refer :all]
    [paradex.kernel.slipnet   :refer :all]))

(defn create-central []
  (atom {:slipnet   {:nodes {} :links {}}
         :workspace {}
         :coderack  {:codelets '()}}))

(def codelet-library (atom {}))

(defmacro def-clet [id args body]
  `(def-codelet codelet-library ~id ~args ~body))
(def-clet "x" [central] (do (println "ran x") (add-codelet central :y 1)))
(def-clet "y" [central] (do (println "ran y") (add-codelet central :z 1)))
(def-clet "z" [central] (do (println "ran z") (add-codelet central :x 1)))

;(def central (atom {:slipnet slipnet :workspace workspace :coderack coderack}))
(def central (create-central))

(defn main-loop []
  (add-codelet central :x 1)
  (create-node central "a" 100 10 [])
  (create-node central "b" 100 10 [])
  (create-link central "a" "b" nil nil 100 true)
  (loop []
    (do
      (let [picked (pick-codelet central)]
        (println picked)
        (run-codelet codelet-library picked central)
        (println central) 
        (recur)))))
