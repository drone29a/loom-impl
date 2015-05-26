(ns loom-impl.alg-test
  (:require [clojure.test :refer :all]
            [loom.graph :refer :all :exclude [weighted-graph graph Edge]]
            [loom.alg :refer :all]
            [loom-impl.core :refer :all]))

(defn weighted-graph
  [& edges]
  (-> (undirected-graph)
      (add-edges* edges)))

(defn graph
  [& edges]
  (let [edges* (if (map? edges)
                 (for [n1 (keys edges)
                       n2 (get edges n1)]
                   [n1 n2])
                 edges)]
    (-> (undirected-graph)
        (add-edges* edges*))))

;; http://en.wikipedia.org/wiki/Dijkstra's_algorithm
(def g1
  (weighted-graph
   [1 2 7] [1 3 9] [1 6 14] [2 3 10] [2 4 15]
   [3 4 11] [3 6 2] [4 5 6] [5 6 9]))

;; http://www.algolist.com/Dijkstra's_algorithm
(def g2
  (weighted-graph
   [:r :g 10] [:r :b 5] [:r :o 8] [:g :b 3] [:b :p 7] [:p :o 2]))

;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
(def g4
  (weighted-graph
   [:a :b 85]
   [:b :f 80]
   [:f :i 250]
   [:i :j 84]
   [:a :c 217]
   [:c :g 186]
   [:c :h 103]
   [:d :h 183]
   [:h :j 167]
   [:a :e 173]
   [:e :j 502]))

(def g6 (graph [0 1] [1 2] [1 3] [2 4] [3 4] [0 5]))

(def g8 (graph {1 [2 3 4] 5 [6 7 8]}))

;; graphs for mst
;; http://en.wikipedia.org/wiki/Kruskal's_algorithm
(def mst_wt_g1 (weighted-graph '(:a, :e , 1)
                        '(:c, :d ,2)
                        '(:a,:b, 3),
                        '(:b,:e,4),
                        '(:b,:c,5)
                        '(:e,:c,6)
                        '(:e,:d,7)))

;;graph with 2 components
(def mst_wt_g2 (weighted-graph [:a :b 2]
                              [:a :d 1]
                              [:b :d 2]
                              [:c :d 3]
                              [:b :c 1]
                              [:e :f 1]
                             ))

(def mst_unweighted_g3 (graph [:a :b] [:a :c] [:a :d] [:b :d] [:c :d]))

(def mst_wt_g4 (weighted-graph [:a :b 1]))

(def mst_wt_g5 (weighted-graph [:a :b 5] [:a :c 2] [:b :c 2]))

;;graph from Cormen et all
(def mst_wt_g6 (weighted-graph [:a :b 4] [:a :h 8]
                               [:b :c 8] [:b :h 11]
                               [:c :d 7] [:c :f 4] [:c :i 2]
                               [:d :f 14] [:d :e 9]
                               [:e :f 10]
                               [:f :g 2]
                               [:i :h 7] [:i :g 6]
                               [:h :g 1] ))


;;graph with 2 components and 2 isolated nodes
(def mst_wt_g7 (weighted-graph [:a :b 2]
                               [:b :d 2]
                               [:e :f 1]
                               :g :h
                             ))

(deftest depth-first-test
  (are [expected got] (= expected got)
       #{1 2 3 4 5 6 7 8} (set (pre-traverse g8))
       #{1 2 3 4 5 6 7 8} (set (post-traverse g8))))

(deftest breadth-first-test
  (are [expected got] (= expected got)
       #{1 2 3 4 5 6 7 8} (set (bf-traverse g8))
       false (not (some #{(bf-traverse (remove-nodes g6 5))}
                        [[0 1 2 3 4] [0 1 3 2 4]]))
       #{:r} (set (bf-traverse g2 :r :when #(< %3 1)))
       #{:r :o :b :g} (set (bf-traverse g2 :r :when #(< %3 2)))
       #{:r :o :b :g :p} (set (bf-traverse g2 :r :when #(< %3 3)))
       [:a :e :j] (bf-path g4 :a :j)
       [:a :c :h :j] (bf-path g4 :a :j :when (fn [n p d] (not= :e n)))
       [:a :e :j] (bf-path-bi g4 :a :j)))

(deftest dijkstra-test
  (are [expected got] (= expected got)
       [:a :c :h :j] (dijkstra-path g4 :a :j)
       [[:a :c :h :j] 487] (dijkstra-path-dist g4 :a :j)
       [[:r :o :p] 10] (dijkstra-path-dist g2 :r :p)
       #{:r :g :b :o :p} (set (map first (dijkstra-traverse g2)))
       {:r {:o 8 :b 5} :b {:g 8} :o {:p 10}} (dijkstra-span g2 :r)))

(deftest johnson-test
  (are [expected got] (= expected got)
       {:p {:p {:o 2, :b 7}
            :o {:r 10}
            :b {:g 10}}
        :o {:o {:p 2, :r 8}
            :p {:b 9}
            :b {:g 12}}
        :g {:g {:b 3}
            :b {:r 8, :p 10}
            :p {:o 12}}
        :b {:b {:p 7, :g 3, :r 5}
            :p {:o 9}}
        :r {:r {:o 8, :b 5}
            :b {:g 8}
            :o {:p 10}}} (johnson g2)

        {1 {1 {5 1}, 5 {3 2}, 3 {2 3, 6 3}, 2 {4 4}, 6 {10 4}}
         2 {2 {4 1}, 4 {10 2}}
         3 {3 {1 1, 2 1, 6 1}, 1 {5 2}, 2 {4 2}, 6 {10 2}}
         4 {4 {10 1}, 10 {2 2}}
         5 {5 {3 1}, 3 {1 2, 2 2, 6 2}, 2 {4 3}, 6 {10 3}}
         6 {6 {1 1, 10 1}, 1 {5 2}, 10 {2 2}, 2 {4 3}, 5 {3 3}}
         7 {4 {10 4}, 8 {11 2, 9 2}, 7 {8 1}, 9 {5 3, 3 3}, 11 {4 3, 2 3}, 3 {6 4, 1 4}}
         8 {4 {10 3}, 8 {11 1, 9 1}, 9 {7 2, 5 2, 3 2}, 11 {4 2, 2 2}, 3 {6 3, 1 3}}
         9 {8 {11 3}, 6 {10 3}, 7 {8 2}, 2 {4 3}, 9 {7 1, 5 1, 3 1}, 3 {6 2, 2 2, 1 2}}
         10 {10 {2 1}, 2 {4 2}}
         11 {11 {2 1, 4 1}, 4 {10 2}}} (johnson g13)

         false (johnson g11)

         {:e {:e {:b 0}
              :b {:d 0, :c 0}}
          :d {:d {:e 0}
              :e {:b 0}
              :b {:c 0}}
          :b {:b {:d 0, :c 0}
              :d {:e 0}}
          :c {}
          :a {:a {:b 10}
              :b {:d 10, :c 10}
              :d {:e 10}}} (johnson g12)))

(deftest all-pairs-shortest-paths-test
  (are [expected got] (= expected got)
        {:p {:p {:o 2, :b 7}
             :o {:r 10}
             :b {:g 10}}
         :o {:o {:p 2, :r 8}
             :p {:b 9}
             :b {:g 12}}
         :g {:g {:b 3}
             :b {:r 8, :p 10}
             :p {:o 12}}
         :b {:b {:p 7, :g 3, :r 5}
             :p {:o 9}}
         :r {:r {:o 8, :b 5}
             :b {:g 8}
             :o {:p 10}}} (all-pairs-shortest-paths g2)

        {1 {1 [5], 5 [3], 3 [6 2], 2 [4], 6 [10]}
         2 {2 [4], 4 [10]}
         3 {3 [6 2 1], 1 [5], 2 [4], 6 [10]}
         4 {4 [10], 10 [2]}
         5 {5 [3], 3 [6 2 1], 2 [4], 6 [10]}
         6 {6 [10 1], 1 [5], 10 [2], 5 [3], 2 [4]}
         7 {4 [10], 8 [9 11], 7 [8], 9 [3 5], 11 [2 4], 3 [1 6]}
         8 {4 [10], 8 [9 11], 9 [3 5 7], 11 [2 4], 3 [1 6]}
         9 {8 [11], 6 [10], 7 [8], 2 [4], 9 [3 5 7], 3 [1 2 6]}
         10 {10 [2], 2 [4]}
         11 {11 [4 2], 4 [10]}} (all-pairs-shortest-paths g13)))

(deftest connectivity-test
  (are [expected got] (= expected got)
       [#{1 2 3 4} #{5 6 7 8} #{9}] (map set (connected-components
                                              (add-nodes g8 9)))
       [#{:r :g :b :o :p}] (map set (connected-components g2))
       [#{1 2 3 4 5 6 8 7}] (map set (connected-components g9))
       true (connected? g6)
       true (connected? g9)
       #{#{2 3 4 1} #{8} #{7 5 6}} (set (map set (scc g9)))
       #{#{:b :e :a} #{:h :d :c} #{:f :g}} (set (map set (scc g10)))
       false (strongly-connected? g9)
       #{1 2 3 4 5 6 7 8} (set (nodes (connect g8)))
       #{:r :g :b :o :p} (set (nodes (connect g2)))))

(deftest other-stuff-test
  (are [expected got] (= expected got)
       false (dag? g2)
       [:a :c :h :j] (shortest-path g4 :a :j)
       [:a :e :j] (shortest-path (graph g4) :a :j)
       #{9 10} (set (loners (add-nodes g8 9 10)))
       ;; TODO: the rest
       ))

(deftest bellman-ford-test
  (are [expected graph start]
       (= expected (bellman-ford graph start))

       false g11 :a
       false g11 :b
       [{:e Double/POSITIVE_INFINITY,
         :d Double/POSITIVE_INFINITY,
         :b Double/POSITIVE_INFINITY,
         :a Double/POSITIVE_INFINITY,
         :c 0}{:c [:c]}] g11 :c
         false g11 :d
         false g11 :e
         [{:e 10,
           :d 8,
           :b 3,
           :c 7,
           :a 0}
          {:a [:a],
           :c [:a :b :c],
           :b [:a :b],
           :d [:a :b :d],
           :e [:a :b :d :e]}] g12 :a
           [{:e 7,
             :d 5,
             :c 4,
             :a Double/POSITIVE_INFINITY,
             :b 0}
            {:b [:b],
             :c [:b :c],
             :d [:b :d],
             :e [:b :d :e]}] g12 :b
             [{:e Double/POSITIVE_INFINITY,
               :d Double/POSITIVE_INFINITY,
               :b Double/POSITIVE_INFINITY,
               :a Double/POSITIVE_INFINITY,
               :c 0}
              {:c [:c]}] g12 :c
              [{:e 2,
                :b -5,
                :c -1,
                :a Double/POSITIVE_INFINITY,
                :d 0}
               {:d [:d],
                :c [:d :e :b :c],
                :b [:d :e :b],
                :e [:d :e]}] g12 :d
                [{:d -2,
                  :b -7,
                  :c -3,
                  :a Double/POSITIVE_INFINITY,
                  :e 0}
                 {:e [:e],
                  :c [:e :b :c],
                  :b [:e :b],
                  :d [:e :b :d]}] g12 :e))

(deftest bipartite-test
  (are [expected got] (= expected got)
       {0 1, 1 0, 5 0, 2 1, 3 1, 4 0} (bipartite-color g6)
       {5 1, 1 1, 2 0, 3 0, 4 0, 6 0, 7 0, 8 0} (bipartite-color g8)
       nil (bipartite-color g1)
       true (bipartite? g6)
       true (bipartite? g8)
       false (bipartite? g1)
       #{#{2 3 4 6 7 8} #{1 5}} (set (bipartite-sets g8))))

(deftest coloring?-test
  (are [expected got] (= expected got)
       true (coloring? g1 {1 0, 2 1, 3 2, 4 0, 5 2, 6 1})
       false (coloring? g1 {1 0, 2 1, 3 2, 4 0, 5 1, 6 1})
       true (coloring? g2 {:r 0, :g 1, :b 2, :p 0, :o 1})))

(deftest greedy-coloring-test
  (are [expected got] (= expected got)
       true (coloring? g1 (greedy-coloring g1))
       true (coloring? g2 (greedy-coloring g2))
       true (coloring? g4 (greedy-coloring g4))
       true (coloring? g6 (greedy-coloring g6))
       true (coloring? g13 (greedy-coloring g13))
       ; expected colors are 0, 1, and 2
       2 (apply max (vals (greedy-coloring triangle)))))

(deftest scc-test
  (are [expected got] (= expected got)
       #{#{2 4 10} #{1 3 5 6} #{11} #{7 8 9}} (set (map set (scc g13)))))

(deftest prim-mst-edges-test
  (are [expected got] (= expected got)
       [[:a :e 1] [:a :b 3] [:b :c 5] [:c :d 2]] (prim-mst-edges mst_wt_g1)
       [[:a :d 1] [:a :b 2] [:b :c 1] [:f :e 1]] (prim-mst-edges mst_wt_g2)
       [[:a :c] [:a :b] [:a :d]] (prim-mst-edges mst_unweighted_g3)
       [[:a :b 1]] (prim-mst-edges mst_wt_g4)
       [[:a :c 2] [:c :b 2]] (prim-mst-edges mst_wt_g5)
       [[:a :b 4] [:b :c 8] [:c :i 2] [:c :f 4] [:f :g 2]
        [:g :h 1] [:c :d 7] [:d :e 9]]  (prim-mst-edges mst_wt_g6)))

(deftest prim-mst-test
  (are [expected got] (= expected got)
       [#{:a :b :d :e :f :g :h} [[:a :b][:b :d][:b :a][:f :e][:d :b][:e :f]]]
       (let [mst (prim-mst mst_wt_g7)]
         [(nodes mst) (edges mst)])
       [#{:a :b :c} [[:a :c] [:c :b] [:c :a] [:b :c]]] (let [mst (prim-mst mst_wt_g5)]
              [(nodes mst) (edges mst)])))


;;;;graphs for A* path
(def astar-simple-path-g1 (graph [:a :b]
                            [:b :c]
                            [:c :d]
                            [:d :e]))

;;graph, with unreachable node
(def astar-with-unreachable-target-g2 (graph [:a :b]
                                              [:b :c]
                                              [:d :e]))

(deftest astar-path-test
  (are [expected got](= expected got)
       {:e :d :d :c :c :b :b :a :a nil}
       (astar-path astar-simple-path-g1 :a :e (fn [x y] 0))
       {:a nil :b :a :c :b}
       (astar-path astar-with-cycle-g3 :a :c (fn [x y] 0))
       {:a nil :b :a :c :b :d :c}
       (astar-path astar-with-cycle-g3 :a :d (fn [x y] 0))
       {:a nil :b :a :c :b :d :c}
       (astar-path astar-weighted-graph-g4 :a :d (fn [x y] 0))
       ;;all test graphs used for Dijkstra should work for A* as well
       {:a nil, :c :a, :h :c, :j :h} (astar-path g4 :a :j nil)
       {:r nil, :o :r, :p :o} (astar-path g2 :r :p nil))
  (is (thrown? Exception (astar-path astar-with-unreachable-target-g2 :a :e nil))
      ))

(deftest astar-dist-test
  (are [expected got](= expected got)
       4
       (astar-dist astar-simple-path-g1 :a :e (fn [x y] 0))
       2
       (astar-dist astar-with-cycle-g3 :a :c (fn [x y] 0))
       3
       (astar-dist astar-with-cycle-g3 :a :d (fn [x y] 0))
       35
       (astar-dist astar-weighted-graph-g4 :a :d (fn [x y] 0))
       )
  )

(def degeneracy-g1 (graph {:a [:b]
                           :b [:c :d]}))

(def degeneracy-g2 (graph {:a [:b]
                           :b [:c :d :e :f]
                           :d [:e :f]
                           :e [:f]}))

(deftest degeneracy-ordering-test
  (let [ns (degeneracy-ordering degeneracy-g1)]
    (is (= #{:a :c :b :d} (set ns)))
    (is (contains? (set (drop 2 ns)) :b)))

  (let [ns (degeneracy-ordering degeneracy-g2)]
    (is (= #{:a :c :b :d :e :f} (set ns)))
    (is (= #{:a :c} (set (take 2 ns))))
    (is (contains? (set (drop 2 ns)) :b))))

;; Graph with 4 maximal cliques: [:a :b :c], [:c :d], [:d :e :f :g], [:d :h].
(def maximal-cliques-g1 (graph {:a [:b :c]
                                :b [:c]
                                :c [:d]
                                :d [:e :f :g :h]
                                :e [:f :g]
                                :f [:g]}))

;; Graph with 3 maximal cliques: #{:a :b :c} #{:b :d :e} #{:e :f}
(def maximal-cliques-g2 (weighted-graph [:a :b 1]
                                        [:a :c 1]
                                        [:b :c 1]
                                        [:b :d 1]
                                        [:d :e 1]
                                        [:b :e 1]
                                        [:e :f 1]))

(deftest maximal-cliques-test
  (are [expected got](= expected got)
       #{#{:a :b :c} #{:c :d} #{:d :e :f :g} #{:d :h}}
       (set (maximal-cliques maximal-cliques-g1))
       #{#{:a :b :c} #{:b :d :e} #{:e :f}}
       (set (maximal-cliques maximal-cliques-g2))))
