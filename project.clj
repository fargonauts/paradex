(defproject paradex "0.1.0"
  :description ""
  :url ""
  :license {:name ""
            :url ""}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.2.4"]]
  :plugins [[lein-bin "0.3.4"]
            [lein-marginalia "0.9.1"]]
  :bin { :name "paradex" }
  :main paradex.core)
