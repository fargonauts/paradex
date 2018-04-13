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

;(defflavor slipnode 
;    (activation 
;     activation-buffer ; A buffer for storing activation between updates.
;     (clamp nil) ; If this is t, then the activation of the node is clamped
;                 ; to 100.
;
;     (intrinsic-link-length nil) ; The intrinsic link-length of this links
;                                 ; labeled by this node.
;     (shrunk-link-length nil)  ; For now this is .4 of the intrinsic 
;                               ; link-length
;     conceptual-depth
;
;     pname  ; A string giving the name of this node.
;     symbol-name ; A symbol giving the name of this node.
;     short-name ; A string to use for slipnet graphics.
;     cm-name ; A string to use for concept-mapping graphics.
;
;     (category-links nil)  
;     (instance-links nil)
;     (has-property-links nil)
;     (lateral-slip-links nil)
;     (lateral-nonslip-links nil)
;     (incoming-links nil)
;
;     (codelets nil)  ; A list of codelets attached to this node.
;
;     (description-tester nil) ; A function for testing if this node
;                              ; can be used as a descriptor for some object.
;
;     (iterate-group nil) ; For nodes representing groups, a function used to 
;                         ; iterate the group (e.g., if succgrp is given "a", it
;                         ; will return "b").
;
;     graphics-obj ; The graphics object representing this node.
;    )
;					   
;  ()
;  :gettable-instance-variables
;  :settable-instance-variables
;  :initable-instance-variables)
;
;;---------------------------------------------
;
;(defmethod (slipnode :outgoing-links) ()
;; Returns a list of the links emanating from this node.
;  (append category-links instance-links has-property-links lateral-slip-links 
;	  lateral-nonslip-links))
;
;;---------------------------------------------
;
;(defmethod (slipnode :active?) ()
;  (= activation 100))
;
;;---------------------------------------------
;
;(defmethod (slipnode :directed?) ()
;; Returns t if the slipnode represents a directed bond or group.
;  (or (eq self plato-predecessor) (eq self plato-successor) 
;      (eq self plato-predgrp) (eq self plato-succgrp)))
;
;;---------------------------------------------
;
;(defmethod (slipnode :category) ()
;; Returns the category that this node belongs to (e.g., "leftmost"
;; belongs to "string-position-category").  For now this assumes that
;; each node belongs to at most one cateogry.  
;; For the time being this doesn't affect anything, but it eventually should be
;; fixed.
;  (if* category-links then (send (car category-links) :to-node) else nil))
;
;;---------------------------------------------
;
;(defflavor slipnet-link 
;    (from-node to-node (label nil) (fixed-length nil))
;    ; If a link has no label, then it is assigned a fixed length.
;    ()
;  :gettable-instance-variables
;  :settable-instance-variables
;  :initable-instance-variables)
;
;;---------------------------------------------
;
;(defmethod (slipnet-link :intrinsic-degree-of-association) ()
;  (if* fixed-length
;   then (fake-reciprocal fixed-length)
;   else (send label :intrinsic-degree-of-association)))
;
;;---------------------------------------------
;
;(defmethod (slipnode :intrinsic-degree-of-association) ()
;  (fake-reciprocal intrinsic-link-length))
;
;;---------------------------------------------
;
;(defmethod (slipnet-link :degree-of-association) ()
;  (if* fixed-length
;   then (fake-reciprocal fixed-length)
;   else (send label :degree-of-association)))
;
;;---------------------------------------------
;
;(defmethod (slipnode :degree-of-association) ()
;; Returns the degree-of-association encoded in the links this node labels.
;  (if* (send self :active?) 
;   then (fake-reciprocal shrunk-link-length)
;   else (fake-reciprocal intrinsic-link-length)))
;   
;;---------------------------------------------
;
;(defmethod (slipnode :similar-has-property-links) ()
;  (loop for link in has-property-links 
;	when (eq (flip-coin (get-temperature-adjusted-probability
;				(/ (send link :degree-of-association) 100))) 
;		 'heads)
;	collect link))
;
;;---------------------------------------------

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
;(defmethod (slipnode :get-related-node) (relation)
;; Returns the node related to the given node by the given relation
;; (e.g., if given "left" and "opposite", returns "right").
;  (if* (eq relation plato-identity)
;   then self
;   else (loop for link in (send self :outgoing-links)
;	      when (eq (send link :label) relation)
;              return (send link :to-node))))
;
;;---------------------------------------------
;
;(defmethod (slipnode :apply-slippages) (slippage-list)
;; Returns the node that is the translation of the given node
;; according to the given slippage list.
;  (loop for s in slippage-list 
;	when (eq (send s :descriptor1) self)
;	return (send s :descriptor2)
;	finally (return self)))
;
;;---------------------------------------------
;
;(defmethod (slipnode :get-possible-descriptors) (object &aux instance)
;; Returns a list of the instances of the given node that could be used
;; as descriptors for the given object.
;  (loop for link in instance-links do    
;	(setq instance (send link :to-node))
;	when (funcall (send instance :description-tester) object)
;	collect instance))
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
;(defmethod (slipnode :decay) ()
;; A node loses (100 - conceptual-depth) percent of its activation.
;  (send self :subtract-activation-from-buffer 
;             (round (* (/ (fake-reciprocal (send self :conceptual-depth)) 100)
;		       (send self :activation)))))
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
