(ns paradex.kernel.slipnet.link
  (:require [paradex.kernel.base :refer [inv-100]]))

; label and length are exclusively optional 
; (if a link has no label, then it is assigned a fixed length).

; Link types: (defrecord Links [category instance property lateral-slip lateral-non-slip incoming])

(def link-types [:category :instance :property :slip :non-slip :incoming])
(defrecord Link [from to kind label fixed-length])


(defn init-link [from to kind extra]
  (if (number? extra)
    (Link. from to kind nil extra)
    (Link. from to kind extra nil)))

(defn init-links [] (into {} (for [kind link-types] [kind {}])))
