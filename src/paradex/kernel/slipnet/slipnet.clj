(ns paradex.kernel.slipnet.slipnet
  (:require [paradex.kernel.slipnet.formulas   :refer :all]
            [paradex.kernel.coderack.coderack  :refer :all]))

; Node
;  - activation
;  - (conceptual) depth
;  - name
;  - links
;  - codelets associated

; Link
;  - from
;  - to
;  - type
;  - label
;  - fixed length

; Slipnet
;  - Nodes
;  - Links

; That easy
;
; (def slipnet (atom {
;     :nodes {:a {:activation 100 :depth 50 :associated ["x" "y"]}
;             :b {:activation 100 :depth 50 :associated ["x" "y"]}}
;     :links {:a {:to :b :type nil :label nil :length 50 :fixed false}}
;   }))

(defn build-node [activation depth id associated]
  {:activation activation
   :depth      depth
   :id         id
   :associated associated 
   })

(defn build-link [to t label length fixed]
  {:to     to
   :type   t
   :label  label
   :length length
   :fixed  fixed
   })

(defn- add-nested [central k1 k2 v]
  (swap! central
         (fn [central]
           (assoc-in central [:slipnet k1 k2] v))))

(defn add-node [central k v]
  (add-nested central :nodes k v))

(defn add-link [central k v]
  (add-nested central :links k v))

(defn create-node [central id activation depth associated]
  (add-node central id (build-node activation depth id associated)))

(defn create-link [central from to t label length fixed]
  (add-link central from (build-link to t label length fixed)))

(defn decay [node]
  (let [depth      (:depth node)
        activation (:activation node)]
    (assoc node :activation (decay-formula activation depth))))

(defn post-codelets [central node]
  (let [activation (:activation node)]
          (when (slipnet-activation-post-threshold activation)
            (doseq [id (:associated node)]
              (add-codelet central id (slipnet-urgency-post-formula activation))))))

(defn update-node [central node]
  (post-codelets central node)
  (decay node))

(defn update-link [central link]
  ; TODO: Shrink in proportion to label node
  link)

(defn slipnet-update [central]
  (let [slipnet (:slipnet @central)
        nodes   (:nodes slipnet)
        links   (:links slipnet)]
    (doseq [[k node] nodes]
      (add-node central k (update-node central node)))
    (doseq [[k link] links]
      (add-link central k (update-link central link)))))

; TODO: organize once file is bigger
