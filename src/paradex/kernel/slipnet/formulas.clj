(ns paradex.kernel.slipnet.formulas)

(defn decay-formula [activation depth]
  (max 0 (- activation (int (* activation (/ (- 100 depth) 100))))))

(defn urgency-post-formula [activation]
  (quot activation 10))

(defn activation-post-threshold [activation]
  (> activation 50))
