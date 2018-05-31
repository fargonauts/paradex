(ns paradex.slipnet
  (:require [clojure.test :refer :all]
            [paradex.kernel.base            :refer :all]
            [paradex.kernel.slipnet.slipnet :refer :all]
            [paradex.kernel.slipnet.node    :refer :all]
            [paradex.kernel.slipnet.link    :refer :all]
            [paradex.kernel.slipnet.links   :refer :all]
            [paradex.kernel.central         :refer :all]))
; Slipnet

;  (setq plato-a 
;	(make-instance 'slipnode  
;          :conceptual-depth 10
;          :pname "A"
;          :symbol-name 'a
;          :short-name '("A")
;	  :cm-name "A"
;          :incoming-links '(b-a-link letter-category-a-link)
;	  :category-links '(a-letter-category-link)
;          :has-property-links '(a-first-link)
;	  :lateral-nonslip-links '(a-b-link)))
;
;  (setq plato-b 
;	(make-instance 'slipnode 
;	  :conceptual-depth 10
;    	  :pname "B"
;          :symbol-name 'b
;          :short-name '("B")
;	  :cm-name "B"
;          :incoming-links '(a-b-link c-b-link letter-category-b-link)
;	  :category-links '(b-letter-category-link)
;          :lateral-nonslip-links '(b-a-link b-c-link)))

; Links

;   	     (make-instance 'slipnet-link 
;		 :from-node letter1 :to-node letter2 
;		 :label plato-successor))

;(def link-types [:category :instance :property :slip :non-slip :incoming])

(deftest test-slipnet
  (testing "Tests independent slipnet functions"
    (let [central (init-central)]
      ;(clojure.pprint/pprint central)
      (create-node central :predecessor 100 20 30 [] nil)
      (create-node central :successor   100 20 30 [] nil)
      ;(clojure.pprint/pprint central)

      (create-node central :a 100 20 10 [] nil)
      (create-node central :b 100 20 10 [] nil)
      ;(clojure.pprint/pprint central)

      (create-link central :a :b :lateral :successor  )
      ;(clojure.pprint/pprint central)
      (create-link central :b :a :lateral :predecessor)
      ;(defn node-get-related [central node relation]
      (println "Central structure:")
      (clojure.pprint/pprint central)

      (println "Links for A:")
      (println (get-links-for central :a))
      (println "successor of A:")
      (println (node-get-related central :a :successor))

      (println "A:")
      (println (get-in @central [:slipnet :nodes :a]))

      (println "In-assoc of A")
      (println (intrinsic-association (get-in @central [:slipnet :nodes :a]) central))
      ;(println (intrinsic-association central (get-in @central [:slipnet :nodes :a])))
      ;(println (macroexpand-1 (create-record TEST :id ::test)))
      ;(println (macroexpand '(create-record TEST :id ::test)))
      (create-record Test :first ::nil ::second 2)
      ;(->Test 1 2)
      ;(init-Test 1)
      (update-slipnet central)
      (run central nil))
    (is (= 1 1))))

;(defrecord Link [from to kind label fixed-length])

;(defrecord Node [id 
;                 activation
;                 intrinsic-length 
;                 shrunk-length 
;                 depth 
;                 links 
;                 codelets 
;                 iterate-group])

;(defn create-node [central id & args]
;(defn create-link [central from & args]
