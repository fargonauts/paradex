(ns paradex.kernel.slipnet.link
  (:require [paradex.kernel.base :refer [inv-100]]))

; label and length are exclusively optional 
; (if a link has no label, then it is assigned a fixed length).

; Link types: (defrecord Links [category instance property lateral-slip lateral-non-slip incoming])

(def link-types [:category :instance :property :slip :non-slip :incoming])
(defrecord Link [from to kind label fixed-length])

;(defn link-intrinsic-association 
;  [central link]
;  (if-let [fixed-length (:fixed-length link)]
;    (inv-100 fixed-length)
;    ("Get node association")))
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
;(defmethod (slipnet-link :degree-of-association) ()
;  (if* fixed-length
;   then (fake-reciprocal fixed-length)
;   else (send label :degree-of-association)))
;
;;---------------------------------------------

(defn init-links [] (into {} (for [kind link-types] [kind {}])))
