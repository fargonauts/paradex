(ns paradex.kernel.base)

(defn wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices. (Implemented by Rich Hickey! https://stackoverflow.com/questions/14464011/idiomatic-clojure-for-picking-between-random-weighted-choices?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa)"
  [slices]
  (let [total (reduce + slices)
        r (rand-int total)]
    (loop [i 0 sum 0]
      (if (< r (+ (nth slices i) sum))
        i
        (recur (inc i) (+ (nth slices i) sum))))))

(defn drop-nth [n coll]
   (let [final (keep-indexed #(if (not= %1 n) %2) coll)]
     (println final)
     final))

(defn inv-100 [n]
  (- 100 n))

(defn add-in [dict kc v]
  (let [current (get-in dict kc)]
    (if (nil? current)
      (assoc-in dict kc [v])
      (assoc-in dict kc (concat [v] current)))))

; Macro for record with default values:

; Something like...
;(create @NAME {@FIELDS-DICT})

;(defrecord @NAME [@FIELDS])
;(defn init-@NAME [@UNGIVEN-FIELDS]
;  @NAME. @UNGIVEN-FIELDS and DEFAULTS)

; i.e.
; (defrecord Node [id
;                  activation 
;                  activation-buffer
;                  intrinsic-length 
;                  shrunk-length 
;                  depth 
;                  links ; Shorted from several link-specific members
;                  codelets 
;                  iterate-group])
; 
; (defn init-node [id activation length depth codelets iterate-group]
;   (Node. id activation 0 length length depth (init-links) codelets iterate-group))

; Recall macros:
; (defmacro def-codelet [central id args body-list]
;   "Defines a codelet in a codelet library"
;   `(swap! ~central
;      (fn [state#]
;        (assoc-in state# [:library (keyword ~id)]
;          (fn ~args ~body-list)))))

; Map destructuring
; (defn configure [val & {:keys [debug verbose]

(defmacro create-record [id & {:keys [& dict]}]
  (let [ks (keys dict)]
    `(do (defrecord ~(symbol id) ~(into [] (map symbol ks)))
         (defn ~(symbol (str "init-" id)) ~(into [] (for [[k# v#] dict :when (not= v# ::nil)] k#))
           (~(symbol (str "->" id )) (for [[k# v#] ~dict] (if (= v# ::nil) k# v#)))))))
