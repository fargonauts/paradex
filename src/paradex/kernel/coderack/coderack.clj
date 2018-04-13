(ns paradex.kernel.coderack.coderack
  (:require [paradex.kernel.coderack.formulas :refer [weighted-pick]]))

; This simplified version of the coderack uses no bins.
; Other small simplifications have been made

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
  (pick-codelet-by-formula central identity))

(defn remove-codelet
  "Probabilistically pick a low-urgency codelet to be discarded"
  [central]
  (pick-codelet-by-formula central #(/ 1 %)))

(defn remove-codelets 
  "Remove n codelets"
  [central n]
  (dotimes [_ n] (remove-codelet central)))

(defn add-codelet 
  "Add a new codelet to the coderack"
  [central codelet urgency]
  (let [codelets  (:codelets (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :codelets] 
                       (concat [[urgency codelet]] codelets))))))

(defn add-codelets 
  "Adds a list of codelets to the coderack"
  [central codelets]
  (doseq [[codelet urgency] codelets] (add-codelet central codelet urgency)))

(defn add-updater [central updater]
  "Adds an updater to the coderack"
  (let [updaters  (:updaters (:coderack @central))]
    (swap! central
           (fn [central]
             (assoc-in central [:coderack :updaters] (concat [updater] updaters))))))

(defmacro def-codelet [library id args body-list]
  "Defines a codelet in a codelet library"
  `(swap! ~library
     (fn [state#]
       (assoc state# (keyword ~id)
         (fn ~args ~body-list)))))

(defn run-codelet [library id central]
  "Runs a codelet by id"
  (let [codelet ((keyword id) @library)]
    (apply codelet [central])))

(defn run-updates [library central]
  "Runs updaters in the coderack"
  (let [updaters (:updaters (:coderack @central))]
    (doall (map #(run-codelet library % central) updaters))))

;(defflavor coderack-bin
;  (vector urgency-code relative-urgency-sum 
;   codelet-in-bin-probability pname)	  
;  ()
;  :gettable-instance-variables
;  :settable-instance-variables
;  :initable-instance-variables)
;
;(defmethod (coderack :empty) ()
;; Empty out the coderack.
;  (loop for codelet in (send *coderack* :codelet-list) do
;	(send self :delete-codelet-from-graphics codelet))
;  (loop for bin in *coderack-bins* do
;        (send bin :set-fill-pointer 0))
;  (setq *codelet-list* nil))
;
;;---------------------------------------------
;
;(defmethod (coderack :post) (codelet &aux bin)
;; Posts a codelet to the coderack.  If the coderack has 
;; %max-coderack-size% codelets, remove a codelet to make room for the new
;; one.
;
;  (if* (= (send self :total-num-of-codelets) %max-coderack-size%)
;   then (send self :remove-codelets 1)) ; Remove a codelet from the 
;                                        ; coderack.
;  (setq bin (send codelet :urgency-bin))
;  (vset (send bin :vector) (send bin :fill-pointer) codelet)
;  (send codelet :set-index-in-bin (send bin :fill-pointer))
;  (send codelet :set-time-stamp *codelet-count*)
;  (send bin :set-fill-pointer (1+ (send bin :fill-pointer)))
;  (push codelet *codelet-list*)
;  (send self :add-codelet-to-graphics codelet))
;
;;---------------------------------------------
;
;(defmethod (coderack :post-codelet-list) (codelet-list &aux num-to-remove)
;; See how many codelets have to be removed.  Remove them, and then
;; post the codelets on this list.
;  (setq num-to-remove 
;	(- (+ (send *coderack* :total-num-of-codelets)
;	      (length codelet-list))
;	   %max-coderack-size%))
;  (if* (> num-to-remove 0)
;   then (send *coderack* :remove-codelets num-to-remove))
;  (loop for codelet in codelet-list do
;	(send *coderack* :post-without-removing codelet)))
;
