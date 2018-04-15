(ns paradex.kernel.slipnet.links
  (:require [paradex.kernel.slipnet.link :refer [link-types]]))

(defn init-links [] (into {} (for [kind link-types] [kind {}])))
