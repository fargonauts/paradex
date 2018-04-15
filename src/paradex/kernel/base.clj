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
