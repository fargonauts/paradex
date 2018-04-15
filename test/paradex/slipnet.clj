(ns paradex.slipnet
  (:require [clojure.test :refer :all]
            [paradex.kernel.slipnet.slipnet :refer :all]
            [paradex.kernel.slipnet.node :refer :all]
            [paradex.kernel.slipnet.link :refer :all]
            [paradex.kernel.central :refer :all]))
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
      (create-node central :predecessor 100 20 30 [] nil)
      (create-node central :successor   100 20 30 [] nil)

      (create-node central :a 100 20 10 [] nil)
      (create-node central :b 100 20 10 [] nil)

      (create-link central :a :b :lateral :successor  )
      (create-link central :b :a :lateral :predecessor)
      )
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
