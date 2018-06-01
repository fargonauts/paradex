(ns paradex.core
  (:require [clojure.tools.cli :refer [cli]]
            [paradex.kernel.central :refer [init-central run]]
            [paradex.lib.nbongard   :refer [nbongard]])
  (:gen-class))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (run (init-central nbongard))))
