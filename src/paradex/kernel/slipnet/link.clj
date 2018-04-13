(ns paradex.kernel.slipnet.link
  (:require [paradex.kernel.base :refer [100-inverse]]))

; label and length are exclusively optional 
; (if a link has no label, then it is assigned a fixed length).

; Link types: (defrecord Links [category instance property lateral-slip lateral-non-slip incoming])

(defrecord Link [from to kind label fixed-length])

;(defn link-intrinsic-association 
;  [link]
;  (if-let [fixed-length (:fixed-length link)]
;    (100-inverse fixed-length)
;    ("Get the intrinstic association in a node")))
;
;(defn )

;;---------------------------------------------
;
;(defmethod (slipnet-link :intrinsic-degree-of-association) ()
;  (if* fixed-length
;   then (fake-reciprocal fixed-length)
;   else (send label :intrinsic-degree-of-association)))
;
;;---------------------------------------------
;
;(defmethod (slipnet-link :degree-of-association) ()
;  (if* fixed-length
;   then (fake-reciprocal fixed-length)
;   else (send label :degree-of-association)))
;
;;---------------------------------------------
