(ns paradex.core
  (:require [clojure.tools.cli :refer [cli]]
            [paradex.kernel.drivers :refer :all])
  (:gen-class))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (println args)
    (main-loop)))
