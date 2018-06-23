(ns paradex.kernel.coderack.formulas
  (:require [paradex.kernel.base :refer [wrand drop-nth]]))

(defn weighted-pick 
  ([coll]
   (weighted-pick coll #(:urgency %)))
  ([coll formula]
   (println coll)
   (println (map formula coll))
   (let [index      (wrand (map formula coll)) 
         picked     (nth coll index)
         remaining  (drop-nth index coll)]
     [picked remaining])))

;(defmethod (codelet :remove-probability) ()
;; Returns the probability of removing this codelet from the coderack
;; (a function of the codelet's urgency and age).
;; The 1+ allows some probability for codelets with the highest urgency
;; to be removed.
;  (* (- *codelet-count* time-stamp) 
;     (1+ (- (send *extremely-high-bin* :urgency-value)
;	    (send urgency-bin :urgency-value)))))

; wait-time = (iterations - time-stamp)
; urgency (given)
; urgency * (wait-time / total-wait-times)
