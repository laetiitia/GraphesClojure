(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set]))

(declare key-val)
(declare initDist)

(defn degrees [g]
  "Calculates the degree centrality for each node"
   (loop [key (key-val g), res g]
     (if (seq key)
       (let [sg (get g (first key)), deg (count (get sg :neigh)), add (assoc sg :degree deg)]
         (recur (rest key) (assoc res (first key) add)))
       res)))

(defn key-val[g]
  "Returns of the key of the map"
  (loop [m g, res []]
    (if (seq m)
      (let [ [k v] (first m)]
        (recur (rest m) (conj res k)))
      res)))


(defn initDist [g n]
  "Initialise distance"
  (loop [res {}, s (key-val g)]
    (if (seq s)
      (recur (assoc res (first s) 100) (rest s))
      (assoc res n 0))))

(defn examiner [g n r]
  "return [sommet_ouvert, res]"
  (loop [s (into [] (get (get g n) :neigh)), res r, ouvert []]
    (if (seq s)
      (let [dy (get res (first s)), dx (inc (get res n))]
        (if (> dy dx)
          (recur (rest s) (assoc res (first s) dx) (conj ouvert (first s))) ;;d(y)=d(x)+1
          (recur (rest s) res ouvert)))
      (vector ouvert res))))

;;Algorithme de Dijkstra: chemin de couts minimun (cf: 3I003)
(defn distance [g n]
  "Calculate the distances of one node to all the others"
  (loop [ouvert (vector n), ferme #{}, res (initDist g n)]
    (if (seq ouvert)
      (let [e (examiner g (first ouvert) res),
            o (into [] (distinct (concat ouvert (first e)))),
            f (conj ferme (first ouvert))]
        (recur (into [] (remove (fn [x] (contains? f x)) o)) f (second e)))
      res)))


(defn closeness [g n]
  "Returns the closeness for node n in graph g")


(defn closeness-all [g]
  "Returns the closeness for all nodes in graph g")

;;{1 {:neigh #{0 4 3}}, 0 {:neigh #{1 3}}, 3 {:neigh #{0 1 2}}, 4 {:neigh #{1}},2 {:neigh #{3}}
