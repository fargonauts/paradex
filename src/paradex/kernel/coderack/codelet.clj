(ns paradex.kernel.coderack.codelet)

(defrecord Codelet [codelet-type category args urgency time-stamp])

(defn run-codelet [library codelet central]
  "Runs a codelet"
  (let [skeleton ((:codelet-type codelet) library)]
    (apply skeleton (concat [central] (:args codelet)))))

(defn init-codelet 
  ([central codelet-type category args]
   (init-codelet codelet-type category args 1))
  ([central codelet-type category urgency args]
   (let [time-stamp (-> central :iterations)]
     (Codelet. codelet-type category args urgency time-stamp))))
