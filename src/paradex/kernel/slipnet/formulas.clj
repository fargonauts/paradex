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

(defn dopost-formula [id central]
  true)

(defn calc-codelet-multiplier [id central]
  1)

;(defmethod (slipnode :get-codelets) ()
;  (loop for codelet in codelets do 
;; Decide whether or not to post this codelet, and if so, how many
;; copies to post.
;        (if* (eq (flip-coin (get-post-codelet-probability 
;				(send codelet :structure-category))) 'heads)
;         then (loop for i from 1 to (get-num-of-codelets-to-post
;					(send codelet :structure-category)) do
;                    (push (make-codelet (send codelet :codelet-type)
;			                (send codelet :arguments)
;                                        (get-urgency-bin 
;                                                (* (send self :activation)
;						   (/ (send self :conceptual-depth) 
;						      100))))
;                           *codelets-to-post*)))))

