(ns graphclj.tools
  (:require [clojure.string :as str]))

(declare getLabel)
(declare rank)
(declare dotStyle)
(declare reverseGraph)

;;(def graph {1 {:neigh #{0 4 3}, :close 3.5, :degree 3}, 0 {:neigh #{1 3}, :close 3.0, :degree 2}, 3 {:neigh #{0 1 2}, :close 3.5, :degree 3}, 4 {:neigh #{1}, :close 2.3333333333333335, :degree 1}, 2 {:neigh #{3}, :close 2.3333333333333335, :degree 1}})
(def g2 {1 {:neigh #{0 4 3}, :close 3.5, :degree 3, :rank 3}, 0 {:neigh #{1 3}, :close 3.0, :degree 2, :rank 2}, 3 {:neigh #{0 1 2}, :close 3.5, :degree 3, :rank 3}, 4 {:neigh #{1}, :close 2.3333333333333335, :degree 1, :rank 0}, 2 {:neigh #{3}, :close 2.3333333333333335, :degree 1, :rank 0}})


(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))


(defn rank-nodes [g l]
  "Ranks the nodes of the graph in relation to label l in accending order"
  (let [r (rank (getLabel g l))]
    (loop [k (keys g), res {}]
      (if (seq k)
        (let [f (first k)
              s (get g f)
              s (assoc s :rank (get r f))]
            (recur (rest k) (assoc res f s)))
        res))))

(defn getLabel [g l]
  (let [v1 (into [] (map l (vals g))), v2 (into [] (keys g))]
    (if (and (first v1) (first v2))
      (into [] (map vector v1 v2))
      nil)))


(defn rank [hmap]
  "Rank the result of getLabel: [indice rank]"
  (loop [m (sort hmap), last -1, val -1, res {}, count 0]
    (if (seq m)
      (let [[k v] (first m)]
        (if (= k last)
          (recur (rest m) last val (assoc res v val) (inc count))
          (recur (rest m) k count (assoc res v count) (inc count))))
      res)))


(defn generate-colors [n]
    (let [step 10]
     (loop [colors {}, current [255.0 160.0 122.0], c 0]
       (if (= c (inc n))
         colors
         (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
                (map #(mod (+ step %) 255) current) (inc c))))))


(defn to-dot [g]
  "Returns a string in dot format for graph g, each node is colored in relation to its ranking"
  (str "graph g{" (dotStyle g) (apply str (interpose "\n" (reverseGraph g) )) "\n}"))


(defn dotStyle [g]
  "Return the first part of the to-dot"
  (let [color (generate-colors (apply max (into [] (map first (getLabel g :rank)))))]
    (loop [k (sort (keys g)), res "\n"]
      (if (seq k)
        (recur (rest k) (str res (first k) " [style=filled color=" (seq (get color (get (get g (first k)) :rank))) "]\n"))
        (str/replace res #"[\(|\)]" "\"")))))


(defn reverseGraph [g]
  "Reverse the graph in sequence of link: [ (str 0 -- 1) ... ]"
    (loop [link (getLabel g :neigh), ouvert (into [] (first (first link))), ferme #{}, res []]
      (if (seq link)
        (let [s (first link)]
          (if (seq ouvert)
            (if (and (not (= (second s) (first ouvert))) (not (contains? ferme (first ouvert))))
              (recur link (rest ouvert) ferme (conj res (str (second s) " -- " (first ouvert))))
              (recur link (rest ouvert) ferme res))
            (recur (rest link) (into [] (first (second link))) (conj ferme (second s)) res)))
        res)))
