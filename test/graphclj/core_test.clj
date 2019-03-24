(ns graphclj.core-test
  (:require [clojure.test :refer :all]
            [graphclj.core :refer :all]))
(use 'midje.repl)
(require '[graphclj.graph :as g])
(require '[graphclj.centrality :as c])
(require '[graphclj.tools :as t])


;;graph:
(deftest genGraph
  (fact "intVect converti bien un string en vecteur."
    (map g/intVect ["0 1" "2 3" "0 2"]) => (list [0 1] [2 3] [0 2]))
  (fact "Realise un graphe sans les liaisons entre sommets."
    (g/makeGraph [1 2 0 1]) => {1 {:neigh #{}}, 2 {:neigh #{}}, 0 {:neigh #{}}})
  (fact "Realise un graphe avec les liaisons entre sommets."
    (g/gen-graph ["0 1" "2 3" "0 2"]) => {0 {:neigh #{1 2}}, 1 {:neigh #{0}}, 2 {:neigh #{0 3}}, 3 {:neigh #{2}}}))

(deftest aleaGrap
  (fact "Realise un graphe aléatoire"
    (> (count (filter true? (map not=(repeatedly 20 #(g/erdos-renyi-rnd 3 0.5)) (repeatedly 20 #(g/erdos-renyi-rnd 3 0.5))))) 0) => true))



;;tools:
(deftest rank
  (fact "Ranks the nodes of the graph in relation to label l in accending order"
    (t/rank-nodes {0 {:neigh #{1}, :close 1.0}, 1 {:neigh #{0}, :close 1.0}} :close) => (quote {0 {:neigh #{1}, :close 1.0, :rank 0}, 1 {:neigh #{0}, :close 1.0, :rank 0}})))

(deftest reverseGraph
  (fact "Reverse the graph into string of link."
    (t/reverseGraph {0 {:neigh #{1}, :close 1.0, :rank 0}, 1 {:neigh #{0}, :close 1.0, :rank 0}}) => (quote ["0 -- 1"])))



;;centrality
;;(deftest degree
;;  (fact "Return degree of all nodes"
;;    (c/degrees {0 {:neigh #{1}}, 1 {:neigh #{0}}}) => (quote {0 {:neigh #{1}, :degree 1}, 1 {:neigh #{0}, :degree 1}})))

;;(defn close
;;  (fact "Returns the closeness for all nodes in graph g"
;;    (c/closeness-all {0 {:neigh #{1}}, 1 {:neigh #{0}}}) => (quote {0 {:neigh #{1}, :close 1.0}, 1 {:neigh #{0}, :close 1.0}})))

;;(defn dist
;;  (fact "Calculate the distances of one node to all the others"
;;    (c/distance {1 {:neigh #{0 4 3}}, 0 {:neigh #{1 3}}, 3 {:neigh #{0 1 2}}, 4 {:neigh #{1}},2 {:neigh #{3}}} 1) => (quote {1 0.0, 0 1.0, 3 1.0, 4 1.0, 2 2.0})))
