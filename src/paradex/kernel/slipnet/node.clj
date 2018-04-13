(ns paradex.kernel.slipnet.node)

(defrecord Node [id
                 activation 
                 intrinsic-length 
                 shrunk-length 
                 depth 
                 links ; Shorted from several link-specific members
                 codelets 
                 iterate-group])

; Unimplemented node features:
;
;     Clamp used??
;     (clamp nil) ; If this is t, then the activation of the node is clamped
;                 ; to 100.
;
;     I'm not sure I like these (seems ad-hoc). Is there a better way? 
;
;     (description-tester nil) ; A function for testing if this node
;                              ; can be used as a descriptor for some object.
;
;
;     (iterate-group nil) ; For nodes representing groups, a function used to 
;                         ; iterate the group (e.g., if succgrp is given "a", it
;                         ; will return "b").
;

(defn reset-node [node]
  (assoc node :activation 0))

(defn active? [node] (= (:activation node) 100))


; Seems domain specific:
;(defmethod (slipnode :directed?) ()
;; Returns t if the slipnode represents a directed bond or group.
;  (or (eq self plato-predecessor) (eq self plato-successor) 
;      (eq self plato-predgrp) (eq self plato-succgrp)))


;(defmethod (slipnode :category) ()
;; Returns the category that this node belongs to (e.g., "leftmost"
;; belongs to "string-position-category").  For now this assumes that
;; each node belongs to at most one cateogry.  
;; For the time being this doesn't affect anything, but it eventually should be
;; fixed.
;  (if* category-links then (send (car category-links) :to-node) else nil))
;
;;---------------------------------------------

;(defmethod (slipnode :similar-has-property-links) ()
;  (loop for link in has-property-links 
;	when (eq (flip-coin (get-temperature-adjusted-probability
;				(/ (send link :degree-of-association) 100))) 
;		 'heads)
;	collect link))
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
;(defmethod (slipnode :intrinsic-degree-of-association) ()
;  (fake-reciprocal intrinsic-link-length))
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
