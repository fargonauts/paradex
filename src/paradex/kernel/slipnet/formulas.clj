(ns paradex.kernel.slipnet.formulas)

;
;(defmethod (slipnode :decay) ()
;; A node loses (100 - conceptual-depth) percent of its activation.
;  (send self :subtract-activation-from-buffer 
;             (round (* (/ (fake-reciprocal (send self :conceptual-depth)) 100)
;		       (send self :activation)))))

(defn decay-formula [activation depth]
  (max 0 (- activation (int (* activation (/ (- 100 depth) 100))))))


(defn urgency-post-formula [activation]
  (quot activation 10))

(defn activation-post-threshold [activation]
  (> activation 50))
