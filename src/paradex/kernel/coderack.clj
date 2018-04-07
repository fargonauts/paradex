(ns paradex.kernel.coderack)

; {:codelets [[urgency codelet]]} sorted by urgency?

(defn pick-codelet [central]
  (let [codelets   (:codelets (:coderack @central))
        codelets   (sort-by first codelets)
        [_ picked] (first codelets)
        remaining  (rest codelets)]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] remaining)))
    picked))

(defn add-codelet [central codelet urgency]
  (let [codelets  (:codelets (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] (concat [[urgency codelet]] codelets))))
    "ran"))

(defmacro def-codelet [library id args body-list]
  `(swap! ~library
     (fn [state#]
       (assoc state# (keyword ~id)
         (fn ~args ~body-list)))))

(defn run-codelet [library id central]
  (apply ((keyword id) @library) [central]))

