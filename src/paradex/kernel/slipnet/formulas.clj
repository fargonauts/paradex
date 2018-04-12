(ns paradex.kernel.slipnet.formulas)

(defn slipnet-decay-formula [activation depth]
  (max 0 (- activation (int (* activation (/ (- 100 depth) 100))))))

(defn slipnet-urgency-post-formula [activation]
  (quot activation 10))

(defn slipnet-activation-post-threshold [activation]
  (> activation 50))
