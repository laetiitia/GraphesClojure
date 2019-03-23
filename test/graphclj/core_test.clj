(ns graphclj.core-test
  (:require [clojure.test :refer :all]
            [graphclj.core :refer :all]))
(use 'midje.repl)
(require '[graphclj.graph :as g])
(require '[graphclj.centrality :as c])
(require '[graphclj.tools :as t])

;;graph:
(deftest function_genGraph
  (fact "intVect converti bien un string en vecteur."
    (map g/intVect ["0 1" "2 3" "0 2"]) => (list [0 1] [2 3] [0 2])))
