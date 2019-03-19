(ns graphclj.graph
  (:require [clojure.string :as str]))


(declare intVect)
(declare makeGraph)
(declare link)


;; Generate a graph from the lines
(defn gen-graph [lines]
    "Returns a hashmap contating the graph"
    (loop [v (map intVect lines), hmap (makeGraph lines)]
      (if (seq v)
          (recur (rest v) (link (first v) hmap))
          hmap)))

;;Add link between the couple in v
(defn link [v hmap]
  (if (= (first v) (second v))
    hmap 
    (loop [cpt 0, n1 (first v), n2 (second v), m hmap]
      (if (< cpt 2)
        (let [ens (get (get m n1) :neigh), res (conj ens n2)]
            (recur (inc cpt) n2 n1 (assoc m n1 {:neigh res})))
        m))))

;; Create a graph without the neigh
(defn makeGraph [lines]
  "Returns a hashmap contating the graph without t"
  (loop [v (reduce concat (map intVect lines)), hmap {}]
    (if (seq v)
      (if (contains? hmap (first v))
        (recur (rest v) hmap)
        (recur (rest v) (assoc hmap (first v) {:neigh #{}})))
      hmap)))

;; String into Vecteur of number
(defn intVect[s]
    (loop [v (str/split s #" "), res []]
      (if (seq v)
        (recur (rest v) (conj res (Integer/parseInt (first v))))
        res)))

(defn erdos-renyi-rnd [n,p]
  "Returns a G_{n,p} random graph, also known as an Erdős-Rényi graph")
