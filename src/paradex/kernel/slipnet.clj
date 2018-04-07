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

(defn- add-nested [slipnet k1 k2 v]
  (swap! slipnet
         (fn [slipnet]
           (assoc slipnet k1 
                  (assoc (k1 slipnet) k2 v)))))

(defn- add-node [slipnet k v]
  (add-nested slipnet :nodes k v))

(defn- add-link [slipnet k v]
  (add-nested slipnet :links k v))

(defn create-node [slipnet id activation depth associated]
  (add-node slipnet id (build-node activation depth id associated)))

(defn create-link [slipnet from to t label length fixed]
  (add-link slipnet from (build-link to t label length fixed)))
