(ns paradex.lib.nbongard
  (:require 
    [paradex.kernel.central :refer :all]
    [paradex.kernel.workspace.workspace :refer :all]
    [paradex.kernel.coderack.coderack   :refer :all]
    [paradex.kernel.slipnet.slipnet     :refer :all]
    [paradex.kernel.library             :refer :all]))

(defn nbg-init-library [central]
  central)

(defn nbg-init-coderack [central]
  central)

(defn nbg-init-slipnet [central]
    ;(create-node)
    ;(create-link)
    central)

;(defn init-node [id activation length depth codelets iterate-group]
;  (Node. id activation 0 length length depth (init-links) codelets iterate-group))
;(defn create-node [central id & args]
;  (add-node central id (apply init-node (concat [id] args))))
;
;(defn init-link [from to kind extra]
;(defn create-link [central & args]
;  (add-link central (apply init-link args)))

(defn nbg-init-workspace [central]
  (reset-workspace central
    (->Workspace {:left [1 3 5 7] :right [2 4 6 8]} {}))
  central)

(defn nbongard []
  (->> (init-central nil)
       nbg-init-slipnet
       nbg-init-workspace
       nbg-init-coderack
       nbg-init-library))
