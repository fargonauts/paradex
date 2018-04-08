(ns paradex.kernel.coderack
  (:require [paradex.kernel.formulas :refer [wrand drop-nth]]))

(defn pick-codelet [central]
  (let [codelets   (:codelets (:coderack @central))
        index      (wrand (map first codelets))
        [_ picked] (nth codelets index)
        remaining  (drop-nth index codelets)]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] remaining)))
    picked))

(defn add-codelet [central codelet urgency]
  (let [codelets  (:codelets (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] (concat [[urgency codelet]] codelets))))))

(defn add-updater [central updater]
  (let [updaters  (:updaters (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :updaters] (concat [updater] updaters))))))

(defmacro def-codelet [library id args body-list]
  `(swap! ~library
     (fn [state#]
       (assoc state# (keyword ~id)
         (fn ~args ~body-list)))))

(defn run-codelet [library id central]
  (let [codelet ((keyword id) @library)]
    (apply codelet [central])))

(defn run-updates [library central]
  (let [updaters (:updaters (:coderack @central))]
    (doall (map #(run-codelet library % central) updaters))))
