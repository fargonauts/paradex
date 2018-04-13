(ns paradex.kernel.slipnet.slipnet
  (:require [paradex.kernel.slipnet.formulas   :refer :all]
            [paradex.kernel.coderack.coderack  :refer :all]))

; Slipnet
;  - Nodes
;  - Links

(defrecord Slipnet [nodes links])

(defn init-slipnet [] (Slipnet. {} {}))

;(defrecord Node [activation 
;                 intrinsic-length 
;                 shrunk-length 
;                 depth 
;                 id 
;                 links 
;                 codelets 
;                 iterate-group])

;(defrecord Link [from to kind label fixed-length])
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

;(defn post-codelets [central node]
;  )
  ;(let [activation (:activation node)]
  ;        (when (activation-post-threshold activation)
  ;          (doseq [id (:associated node)]
  ;            (add-codelet central id (urgency-post-formula activation))))))

(defn update-node [central node]
  ;(post-codelets central node)
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

;---------------------------------------------
;(defun get-label-node (from-node to-node)
;; Returns the node representing the label of the link from FROM-NODE to 
;; TO-NODE.  Returns nil if the link has no label. For now, I am assuming 
;; that there is only one link from the FROM-NODE to the TO-NODE.
;  (if* (eq from-node to-node)
;   then plato-identity
;   else (loop for link in (send from-node :outgoing-links)
;              when (eq (send link :to-node) to-node)
;              return (send link :label))))
;
;;---------------------------------------------
;
;(defun clear-slipnet ()
;; Sets activation to 0 for each node in the slipnet.
;  (loop for node in *slipnet* do
;	(send node :set-activation-buffer 0)
;	(send node :set-activation 0)))
;
;;---------------------------------------------
;

;--------------------------------------------- 
; SLIPNET-FUNCTIONS: This file contains functions for the Slipnet.
;---------------------------------------------

;(in-package 'user)
;
;(defmethod (slipnode :activate-from-workspace) ()
;  (incf activation-buffer %workspace-activation%))
;
;;---------------------------------------------
;
;(defmethod (slipnode :add-activation-to-buffer) (activation-to-add)
;  (incf activation-buffer activation-to-add))
;
;;---------------------------------------------
;
;(defmethod (slipnode :subtract-activation-from-buffer) (activation-to-subtract)
;  (decf activation-buffer activation-to-subtract))
;
;;---------------------------------------------
;
;(defun update-slipnet (&aux amount-to-spread full-activation-probability)
;
;; Decay and spread activation (change buffers, not actual activation, until
;; all nodes have been updated).
;  (loop for node in *slipnet* do
;
;        (send node :decay)
;
;	; If node is active, spread activation to neighbors.
;        ; Note that activation spreading uses the intrinsic link-length,
;        ; not the shrunk link length.
;	(if* (= (send node :activation) %max-activation%)
;         then  ; Give each neighbor the percentage of the activation
;               ; proportional to the inverse of its distance from the 
;	       ; original node.
;	       (loop for link in (send node :outgoing-links) do
;                    (setq amount-to-spread 
;			  (round (* (/ (send link 
;					     :intrinsic-degree-of-association)
;				        100.0)
;			            (send node :activation))))
;		    (send (send link :to-node) 
;			  :add-activation-to-buffer amount-to-spread))))
;		    
;  ; Next, the actual activation of each node is updated for the next time step.
;  (loop for node in *slipnet* do
;        (send node :set-activation 
;	           (min %max-activation% 
;			(+ (send node :activation) 
;			   (send node :activation-buffer))))
;
;        ; If node is still clamped, then activate it.
;	(if* (send node :clamp)
;	 then (send node :set-activation %max-activation%)
;
;	 else ; See if node should become active.  The decision is
;	      ; is a probabilistic function of activation.
;              (if* (>= (send node :activation) %full-activation-threshold%)
;	       then (setq full-activation-probability
;			  (cube (/ (send node :activation) 100)))
;                    (if* (eq (flip-coin full-activation-probability) 'heads)
;	             then (send node :set-activation %max-activation%))))
;
;        (send node :set-activation-buffer 0)))
;
;
;;---------------------------------------------
;
;(defun get-top-down-codelets ()
;; Returns a list of top-down codelets, attached to active nodes, to be posted.
;  (loop for node in *slipnet* do
;        (if* (and (>= (send node :activation) %full-activation-threshold%)
;		  (send node :codelets))
;	 then (send node :get-codelets))))
;  
;;---------------------------------------------
;
;(defmethod (slipnode :get-codelets) ()
;  (loop for codelet in codelets do 
;        ; Decide whether or not to post this codelet, and if so, how many
;	; copies to post.
;        (if* (eq (flip-coin (get-post-codelet-probability 
;				(send codelet :structure-category))) 'heads)
;         then (loop for i from 1 to (get-num-of-codelets-to-post
;					(send codelet :structure-category)) do
;                    (push (make-codelet (send codelet :codelet-type)
;			                (send codelet :arguments)
;                                        (get-urgency-bin 
;                                                (* (send self :activation)
;						   (/ (send self :conceptual-depth) 
;						      100))))
;                           *codelets-to-post*)))))
;						   
;;---------------------------------------------
