(ns paradex.lib.domain)

(defn init-domain [] nil)

"
Reference:

(create-node central :predecessor 100 20 30 [] nil)
(create-node central :successor   100 20 30 [] nil)

(create-node central :a 100 20 10 [] nil)
(create-node central :b 100 20 10 [] nil)

(create-link central :a :b :lateral :successor  )
(create-link central :b :a :lateral :predecessor)

Copycat's Nodes:

a..z
1..5
  
first
last
alphabetic-position

leftmost
rightmost
middle
whole
single
string-position

left
right
direction

predecessor
successor
predecessor-group
successor-group

sameness
sameness-group

letter
group
object-category
bond-category
group-category

identity
opposite
"
