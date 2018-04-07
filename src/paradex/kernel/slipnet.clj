(ns paradex.kernel.slipnet)

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

(defn- add-node [central k v]
  (add-nested central :nodes k v))

(defn- add-link [central k v]
  (add-nested central :links k v))

(defn create-node [central id activation depth associated]
  (add-node central id (build-node activation depth id associated)))

(defn create-link [central from to t label length fixed]
  (add-link central from (build-link to t label length fixed)))
