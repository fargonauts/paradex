(ns paradex.kernel.coderack.coderack
  (:require [paradex.kernel.coderack.formulas :refer [weighted-pick]]
            [paradex.kernel.coderack.codelet  :refer :all]))

; This simplified version of the coderack uses no bins.
; Other small simplifications have been made

(defrecord Coderack [codelets updaters size])
(defn init-coderack [n] (Coderack. [] [] n))

(defn pick-codelet-by-formula 
  "Probabilistically pick a codelet using the urgency formula and remove it from the coderack to be run"
  [central formula]
  (let [codelets   (:codelets (:coderack @central))
       [picked remaining] (weighted-pick codelets formula)]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] remaining)))
    picked))

(defn pick-codelet
  "Probabilistically pick a high-urgency codelet"
  [central]
  (pick-codelet-by-formula central #(:urgency %)))

(defn remove-codelet
  "Probabilistically pick a low-urgency codelet to be discarded"
  [central]
  (pick-codelet-by-formula central #(/ 1 (:urgency %))))

(defn remove-codelets 
  "Remove n codelets"
  [central n]
  (dotimes [_ n] (remove-codelet central)))

(defn add-codelet 
  "Add a new codelet to the coderack without checking for overflow"
  [central codelet]
  (let [codelets  (:codelets (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] 
                       (concat [codelet] codelets))))))

(defn add-codelets
  "Add several codelets to the coderack without checking for overflow"
  [central codelets]
  (doseq [codelet codelets] (add-codelet central codelet)))

(defn post-codelet
  "Posts a codelet to the coderack, removing extraneous codelets if necessary"
  [central codelet]
  (add-codelet central codelet)
  (let [size              (-> @central :coderack :size)
        n-codelets (count (-> @central :coderack :codelets))]
    (when (> n-codelets size)
      (remove-codelets central (- n-codelets size)))))

(defn post-codelets 
  "Post several codelets to the coderack"
  [central codelets]
  (doseq [codelet codelets] (post-codelet central codelet)))

(defn post [central codelet-type category urgency & args]
  "Create and post a codelet to the coderack"
  (post-codelet central (init-codelet central codelet-type category urgency args)))

(defn add-updater [central updater]
  "Adds an updater to the coderack"
  (let [updaters  (:updaters (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :updaters] (concat [updater] updaters))))))

(defn create-updater [central codelet-type & args]
  (add-updater central (init-codelet central codelet-type nil nil args)))

(defn run-next [library central]
  "Runs a codelet by id"
  (let [picked (pick-codelet central)]
    (run-codelet library picked central)))

(defn run-updates [library central]
  "Runs updaters in the coderack"
  (let [updaters (:updaters (:coderack @central))]
    (doall (map #(run-codelet library % central) updaters))))

(defn coderack-step [library central]
  "Run one step of the coderack"
  (run-next library central)
  (run-updates library central))

(defn empty-coderack [library central]
  "Delete the list of active codelets"
  (swap! central
    (fn [central]
      (assoc-in central [:coderack :codelets] []))))

(defmacro def-codelet [library id args body-list]
  "Defines a codelet in a codelet library"
  `(swap! ~library
     (fn [state#]
       (assoc state# (keyword ~id)
         (fn ~args ~body-list)))))

