(ns paradex.kernel.library)

; A very simple datatype for holding codelet templates

(defrecord Library [codelet-templates])
(defn init-library [] (Library. {}))

(defmacro def-codelet [central id args body-list]
  "Defines a codelet in a codelet library"
  `(swap! ~central
     (fn [state#]
       (assoc-in state# [:library (keyword ~id)]
         (fn ~args ~body-list)))))

(defmacro def-clet [id args body]
  `(def-codelet codelet-library ~id ~args ~body))

