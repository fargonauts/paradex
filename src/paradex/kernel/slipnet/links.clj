(ns paradex.kernel.slipnet.links
  (:require [paradex.kernel.coderack.coderack :refer :all]
            [paradex.kernel.base              :refer :all]
            [paradex.kernel.slipnet.link      :refer :all]
            [paradex.kernel.slipnet.node      :refer :all]))

(defrecord Links [from-links to-links])

(defn- modify-linkdict [central f & args]
  (let [kc [:slipnet :links]]
    (swap! central
      (fn [central]
        (assoc-in central kc (apply f (get-in central kc) args))))))

(defn- assoc-link [linkdict link]
  (let [kc1 [:from-links (:from link)]
        kc2 [:to-links   (:to link)]]
    (add-in 
      (add-in linkdict kc2 link) kc1 link)))

(defn add-link [central link]
  (modify-linkdict central assoc-link link))

(defn get-links [central]
  (get-in @central [:central :slipnet :links]))

(defn get-to [central k]
  (get-in (get-links central) [:to-links k]))

(defn get-from [central k]
  (get-in (get-links central) [:from-links k]))

(defn get-link-list [central]
  (let [links (get-links central)]
    (concat (vals (:from-links links)) (vals (:to-links links)))))

(defn get-links-for [central node]
  (let [links (get-links central)]
    [(node (:from-links links)) (node (:to-links links))]))
