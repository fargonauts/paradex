(ns paradex.kernel.workspace.workspace)

(defn add-object [central ks object]
  (swap! central
         (fn [central]
           (assoc central (concat [:workspace] ks) object))))