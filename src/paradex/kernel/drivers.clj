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

;(defn create-node [slipnet id activation depth associated]
;  (add-node slipnet id (build-node activation depth id associated)))
;
;(defn create-link [slipnet from to t label length fixed]
;  (add-link slipnet from (build-link to t label length fixed)))

(def slipnet (atom {:nodes {} :links {}}))

(defn main-loop []
  (create-node slipnet "a" 100 10 [])
  (create-node slipnet "b" 100 10 [])
  (create-link slipnet "a" "b" nil nil 100 true)
  (if false
    nil
    (do
      (let [picked (pick-codelet coderack)]
        (do 
          (println picked)
          (run-codelet codelet-library picked)
          (println @coderack)
          (println @slipnet)
          (recur))))))
