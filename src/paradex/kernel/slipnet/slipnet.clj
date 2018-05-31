(ns paradex.kernel.slipnet.slipnet
  (:require [paradex.kernel.coderack.coderack :refer :all]
            [paradex.kernel.slipnet.formulas  :refer :all]
            [paradex.kernel.slipnet.link      :refer :all]
            [paradex.kernel.slipnet.links     :refer :all]
            [paradex.kernel.slipnet.node      :refer :all]
            [paradex.kernel.base              :refer :all])
  (:import  [paradex.kernel.slipnet.link Link]
            [paradex.kernel.slipnet.node Node]))

(defrecord Slipnet [nodes links])
; Nodes is a dictionary by id
; links is dictionary by from key

(defn init-slipnet [] (Slipnet. {} (->Links {} {})))

(defn- add-nested [central k1 k2 v]
  (swap! central
    (fn [central]
      (assoc-in central [:slipnet k1 k2] v))))

(defn add-node [central k v]
  (add-nested central :nodes k v))

(defn get-node [central k]
  (get-in @central [:nodes k]))

(defn create-node [central id & args]
  (add-node central id (apply init-node (concat [id] args))))

(defn create-link [central & args]
  (add-link central (apply init-link args)))

(defn decay [node]
  (let [depth      (:depth node)
        activation (:activation node)]
    (assoc node :activation (decay-formula activation depth))))

(defn attempt-post-codelets [central node]
  (let [activation (:activation node)]
    (when (activation-post-threshold activation)
      (doseq [id (:codelets node)]
        (when (dopost-formula id central)
          (dotimes [_ (calc-codelet-multiplier id central)]
            (add-codelet central id (urgency-post-formula activation))))))))

(defn update-node [central node]
  (attempt-post-codelets central node)
  (decay node))

;(defn update-link [central link]
;  ; TODO: Shrink in proportion to label node
;  link)


(defn reset-slipnet 
  "Sets activation to 0 for every node in the slipnet"
  [central]
  (let [slipnet (:slipnet @central)
        nodes   (:nodes slipnet)
        links   (:links slipnet)] ; TODO: unify
    (doseq [[k node] nodes]
      (add-node central k (reset-node node)))))

(defn get-label-node [central from to]
  (let [slipnet (:slipnet @central)
        nodes   (:nodes slipnet)
        links   (:links slipnet)
        link    (from links)
        label   (:label link)]
    (label nodes)))

(defn node-category [node]
  (let [category-links (-> node :links :category)]
    (first category-links)))

;get-temperature-adjusted-probability -> workspace feature

;(defn node-similar-property-links [central node]
;  (let [property-links (get-in @central [:slipnet :nodes node :links :property])]
;    ()
;    ))

;(defmethod (slipnode :similar-has-property-links) ()
;  (loop for link in has-property-links 
;	when (eq (flip-coin (get-temperature-adjusted-probability
;				(/ (send link :degree-of-association) 100))) 
;		 'heads)
;	collect link))

(defn node-get-related [central node relation]
  (if (= relation :identity)
    node
    (let [[from to] (get-links-for central node)]
      (for [link from :when (= (:label link) relation)] link))))

(defn apply-slippages [node slippage-list]
  (loop [slippages slippage-list]
    (if (nil? slippages)
      node
      (let [[a b] (first slippages)]
        (if (= a node)
          b
          (recur (rest slippages)))))))

; instance-links??

;(defn get-possible-descriptors [node ])
;(defmethod (slipnode :get-possible-descriptors) (object &aux instance)
;; Returns a list of the instances of the given node that could be used
;; as descriptors for the given object.
;  (loop for link in instance-links do    
;	(setq instance (send link :to-node))
;	when (funcall (send instance :description-tester) object)
;	collect instance))
;
;;---------------------------------------------

(defprotocol Association
  (intrinsic-association [central x])
  (association           [central x]))

(extend-protocol Association 

  Link
  (intrinsic-assocation [link central]
    (if (:fixed-length link)
      (inv-100 (:fixed-length link))
      (association central (get-node central (:label link)))))
  (association [link central]
    (intrinsic-association central link))

  Node
  (intrinsic-association [node central]
    (inv-100 (:intrinsic-length node)))
  (association [node central]
    (if (active? node)
      (inv-100 (:shrunk-length node))
      (intrinsic-association central node))))


(defn update-slipnet [central]
  ; TODO: Synchronous activation change via buffers
  (let [slipnet (:slipnet @central)
        nodes   (:nodes slipnet)
        links   (:links slipnet)]
    (doseq [[k node] nodes]
      (add-node central k (update-node central node)))))
    ;(doseq [[k link] links]
    ;  (add-link central k (update-link central link)))))

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

; Seems domain specific:
;(defmethod (slipnode :directed?) ()
;; Returns t if the slipnode represents a directed bond or group.
;  (or (eq self plato-predecessor) (eq self plato-successor) 
;      (eq self plato-predgrp) (eq self plato-succgrp)))

