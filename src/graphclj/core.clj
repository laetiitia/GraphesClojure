(ns graphclj.core
  (:require [graphclj.graph :as graph]
            [graphclj.tools :as tools]
            [graphclj.centrality :as central])
  (:gen-class))

(declare option)
(declare graph)
(declare getEdge)
(declare verif)

(defn -main
  [& args]
  (println "\n--------- GraphMaker ----------\n 1: Generate a random graph. \n 2: Generate your own graph. \n 3: Quit.\n ")
  (let [reponse (Integer. (read-line))]
    (case reponse
      1 (do (println "\nEnter the number of nodes: ")(let [n (Integer. (read-line))] (println "\nEnter the probability of the edges: ") (let [p (Float/parseFloat (read-line))] (option (graph/erdos-renyi-rnd n p)))))
      2 (graph)
      (println "\n --------------------------------\n"))))

(defn option [g]
  (println "\nHere your graph: \n" g "\n\nWhat do you want to do?\n 1:Degree\n 2:Closeness\n 3:Distance\n 4:Rank\n 5:To-Dot\n 6.Back.\n")
  (let [reponse (Integer. (read-line))]
    (case reponse
      1 (option (central/degrees g))
      2 (option (central/closeness-all g))
      3 (do (println "\nChoose a node: " (into [] (keys g))) (let [r (Integer. (read-line))] (if (contains? g r)
                                                                                              (do (println "Distance: " (central/distance g r) "\n") (option g))
                                                                                              (do (println "\nIt's not a node of your graph.\n") (option g)))))
      4 (do (println "\nRank with what label?\n 1:degree\n 2:closeness\n 3.Your change your mind.\n") (let [r (Integer. (read-line))]
                                                                                                        (case r
                                                                                                          1 (if (nil? (tools/getLabel g :degree))
                                                                                                                (do (println "\nYou don't have the label :degree in your graph...\n") (option g))
                                                                                                                (option (tools/rank-nodes g :degree)))
                                                                                                          2 (if (nil? (tools/getLabel g :close))
                                                                                                                (do (println "\nYou don't have the label :close in your graph...\n") (option g))
                                                                                                                (option (tools/rank-nodes g :close)))
                                                                                                          (option g))))
      5 (if (nil? (tools/getLabel g :rank))
            (do (println "\nYou need a rank for the colors.\n") (option g))
            (do (println "\n--   --   --   -To-Dot-   --   --   --\n")(println (tools/to-dot g)) (option g)))
      (-main))))


(defn graph []
  (println "\nChoose how you want to create your graph:\n 1: Generate with a file. \n 2: Generate on your own. \n 3:You change your mind.\n ")
  (let [reponse (Integer. (read-line))]
    (case reponse
      1 (do (println "\nEnter the PATH of your file (with the .txt): \n") (let [r (read-line), lines (into [] (tools/readfile r)), g (graph/gen-graph lines)] (option g)))
      2 (getEdge)
      (-main))))

(defn getEdge[]
  (println "\nEnter the edge like that: \"0 1\" with 0 and 1 the nodes. If you finish enter -1.\n")
  (loop [res [] reponse (read-line)]
    (let [verif (verif reponse)]
      (case verif
        :true (recur (conj res reponse) (read-line))
        :false (option (graph/gen-graph res))
        (recur res (read-line))))))

(defn verif [s]
  (let [v (graph/intVect s)]
    (if (= (first v) -1)
      :false
      (if (= (count v) 2)
         :true
         (do (println "\nYou have to enter 2 nodes.\n") :failed)))))
