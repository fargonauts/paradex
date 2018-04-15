(ns paradex.kernel.slipnet.node
  (:require [paradex.kernel.slipnet.link :refer :all]))

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

(defn init-node [id activation length depth codelets iterate-group]
  (Node. id activation length length depth (init-links) codelets iterate-group))

(defn reset-node [node]
  (assoc node :activation 0))

(defn active? [node] (= (:activation node) 100))

