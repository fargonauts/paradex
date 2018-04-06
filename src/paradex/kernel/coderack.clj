(ns paradex.kernel.coderack)

; {:codelets [[urgency codelet]]} sorted by urgency?

(defn pick-codelet [coderack]
  (let [codelets   (:codelets @coderack)
        [_ picked] (first codelets)
        remaining  (rest codelets)]
    (swap! coderack
           (fn [coderack]
             (assoc coderack :codelets remaining)))
    picked))

(defn add-codelet [coderack codelet urgency]
  (let [codelets  (:codelets @coderack)]
    (swap! coderack
           (fn [coderack]
             (assoc coderack :codelets (cons [urgency codelet] codelets))))
    "ran"))

(defmacro def-codelet [library id body-list]
  `(swap! ~library
     (fn [state#]
       (assoc state# (keyword ~id)
         (fn [] ~body-list)))))

(defn run-codelet [library id]
  (apply ((keyword id) @library) []))

