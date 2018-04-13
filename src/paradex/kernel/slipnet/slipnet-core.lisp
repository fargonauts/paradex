
;---------------------------------------------
; SLIPNET-DEF: This file contains definitions and methods for nodes
;              in the Slipnet.  
;---------------------------------------------


	
;---------------------------------------------

(defflavor slipnode 
    (activation 
     activation-buffer ; A buffer for storing activation between updates.
     (clamp nil) ; If this is t, then the activation of the node is clamped
                 ; to 100.

     (intrinsic-link-length nil) ; The intrinsic link-length of this links
                                 ; labeled by this node.
     (shrunk-link-length nil)  ; For now this is .4 of the intrinsic 
                               ; link-length
     conceptual-depth

     pname  ; A string giving the name of this node.
     symbol-name ; A symbol giving the name of this node.
     short-name ; A string to use for slipnet graphics.
     cm-name ; A string to use for concept-mapping graphics.

     (category-links nil)  
     (instance-links nil)
     (has-property-links nil)
     (lateral-slip-links nil)
     (lateral-nonslip-links nil)
     (incoming-links nil)

     (codelets nil)  ; A list of codelets attached to this node.

     (description-tester nil) ; A function for testing if this node
                              ; can be used as a descriptor for some object.

     (iterate-group nil) ; For nodes representing groups, a function used to 
                         ; iterate the group (e.g., if succgrp is given "a", it
                         ; will return "b").

     graphics-obj ; The graphics object representing this node.
    )
					   
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defun get-label-node (from-node to-node)
; Returns the node representing the label of the link from FROM-NODE to 
; TO-NODE.  Returns nil if the link has no label. For now, I am assuming 
; that there is only one link from the FROM-NODE to the TO-NODE.
  (if* (eq from-node to-node)
   then plato-identity
   else (loop for link in (send from-node :outgoing-links)
              when (eq (send link :to-node) to-node)
              return (send link :label))))

;---------------------------------------------

(defmethod (slipnode :get-related-node) (relation)
; Returns the node related to the given node by the given relation
; (e.g., if given "left" and "opposite", returns "right").
  (if* (eq relation plato-identity)
   then self
   else (loop for link in (send self :outgoing-links)
	      when (eq (send link :label) relation)
              return (send link :to-node))))

;---------------------------------------------

(defmethod (slipnode :apply-slippages) (slippage-list)
; Returns the node that is the translation of the given node
; according to the given slippage list.
  (loop for s in slippage-list 
	when (eq (send s :descriptor1) self)
	return (send s :descriptor2)
	finally (return self)))

;---------------------------------------------

(defmethod (slipnode :get-possible-descriptors) (object &aux instance)
; Returns a list of the instances of the given node that could be used
; as descriptors for the given object.
  (loop for link in instance-links do    
	(setq instance (send link :to-node))
	when (funcall (send instance :description-tester) object)
	collect instance))

;---------------------------------------------

(defun clear-slipnet ()
; Sets activation to 0 for each node in the slipnet.
  (loop for node in *slipnet* do
	(send node :set-activation-buffer 0)
	(send node :set-activation 0)))
