(ns graphclj.core-test
  (:require [clojure.test :refer :all]
            [graphclj.core :refer :all]))

(require '[graphclj.graph :as g])
(require '[graphclj.centrality :as c])
(require '[graphclj.tools :as t])


;;graph:
(deftest intVect
  (testing "intVect converti bien un string en vecteur."
    (is (= (map g/intVect ["0 1" "2 3" "0 2"]) [[0 1] [2 3] [0 2]]))))

(deftest mekeGraph
  (testing "Realise un graphe sans les liaisons entre sommets."
    (is (= (g/makeGraph [1 2 0 1]) (quote {1 {:neigh #{}}, 2 {:neigh #{}}, 0 {:neigh #{}}})))))

(deftest genGraph
  (testing "Realise un graphe avec les liaisons entre sommets."
    (is (= (g/gen-graph ["0 1" "2 3" "0 2"]) (quote {0 {:neigh #{1 2}}, 1 {:neigh #{0}}, 2 {:neigh #{0 3}}, 3 {:neigh #{2}}})))))

(deftest aleaGrap
  (testing "Realise un graphe aléatoire"
    (is (= (> (count (filter true? (map not=(repeatedly 20 #(g/erdos-renyi-rnd 3 0.5)) (repeatedly 20 #(g/erdos-renyi-rnd 3 0.5))))) 0) true))))


;;tools:
(deftest rank
  (testing "Ranks the nodes of the graph in relation to label l in accending order"
    (is (= (t/rank-nodes {0 {:neigh #{1}, :close 1.0}, 1 {:neigh #{0}, :close 1.0}} :close) (quote {0 {:neigh #{1}, :close 1.0, :rank 0}, 1 {:neigh #{0}, :close 1.0, :rank 0}})))))

(deftest reverseGraph
  (testing "Reverse the graph into string of link."
    (is (= (t/reverseGraph {0 {:neigh #{1}, :close 1.0, :rank 0}, 1 {:neigh #{0}, :close 1.0, :rank 0}}) (quote ["0 -- 1"])))))
;;centrality
(deftest degree
  (testing "Return degree of all nodes"
    (is (= (c/degrees {0 {:neigh #{1}}, 1 {:neigh #{0}}}) (quote {0 {:neigh #{1}, :degree 1}, 1 {:neigh #{0}, :degree 1}})))))

(deftest close
  (testing "Returns the closeness for all nodes in graph g"
    (is (= (c/closeness-all {0 {:neigh #{1}}, 1 {:neigh #{0}}}) (quote {0 {:neigh #{1}, :close 1.0}, 1 {:neigh #{0}, :close 1.0}})))))

(deftest dist
  (testing "Calculate the distances of one node to all the others"
    (is (= (c/distance {1 {:neigh #{0 4 3}}, 0 {:neigh #{1 3}}, 3 {:neigh #{0 1 2}}, 4 {:neigh #{1}},2 {:neigh #{3}}} 1) (quote {1 0.0, 0 1.0, 3 1.0, 4 1.0, 2 2.0})))))
