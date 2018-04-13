(ns paradex.kernel.drivers
  (:require 
    [paradex.kernel.workspace.workspace :refer :all]
    [paradex.kernel.coderack.coderack  :refer :all]
    [paradex.kernel.slipnet.slipnet   :refer :all]))

(defn create-central []
  (atom {:slipnet   {:nodes {} :links {}}
         :workspace {}
         :coderack (init-coderack 100)
         :iterations 0}))

(def codelet-library (atom {}))

(defmacro def-clet [id args body]
  `(def-codelet codelet-library ~id ~args ~body))

(def-clet "x" [central n] (do (println (str "ran x" n))))
(def-clet "y" [central n] (do (println (str "ran y" n))))
(def-clet "z" [central n] (do (println (str "ran z" n))))

(def-clet "status" [central] (do (println "status!") (println central)))
(def-clet "slipnet-update" [central] (slipnet-update central))

(def central (create-central))

;(defn post [central codelet-type category urgency & args]

(defn initialize [central]
  (post central :x nil 1 200)
  (post central :x nil 1 300)
  (create-updater central :status)
  (create-updater central :slipnet-update)
  (create-node central "a" 100 90 [:x])
  (create-node central "b" 100 90 [:x])
  (create-link central "a" "b" nil nil 100 true))

(def-clet "even-odd-split" [central]
  (let [ws    (:workspace @central)
        left  (:left ws)
        right (:right ws)]
    (if (and (every? odd? left)
             (every? even? right))
      (println "Even-odd split detected")
      (println "Unknown sequence"))))


(defn initialize-nbongard [central]
  (add-codelet central :even-odd-split 1)
  (add-object  central [:left] '(1 3 5))
  (add-object  central [:left] '(2 4 6)))

(defn main-loop []
  (println central) 
  (initialize central)
  ;(initialize-nbongard central)
  (println "here")
  (println central) 
  (loop []
    (coderack-step codelet-library central)
    (if (empty? (:codelets (:coderack @central)))
      (println "Empty coderack, finishing..")
      (recur))))
