(ns loom-impl.test.graph-test
  (:require [clojure.test :refer :all]
            [loom-impl.core :refer :all]
            [loom.graph :as lg]))

(deftest simple-weighted-graph-test
  (let [g1 (weighted-graph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (weighted-graph {1 {2 77 3 88} 2 {3 99} 4 []})
        g3 (weighted-graph g1)
        g4 (weighted-graph g3 (weighted-digraph [5 6 88]) [7 8] 9)
        g5 (weighted-graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]} (set (edges g1))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [2 1] [1 3] [3 1] [2 3]
             [3 2] [5 6] [6 5] [7 8] [8 7]} (set (edges g4))
             #{} (set (nodes g5))
             #{} (set (edges g5))
             true (has-node? g1 4)
             true (has-edge? g1 1 2)
             false (has-node? g1 5)
             false (has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (successors g1 1))
           #{1 2} (set (successors g1 3))
           #{} (set (successors g1 4))
           2 (out-degree g1 1)
           2 (out-degree g1 3)
           0 (out-degree g1 4)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
           #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
           #{[1 2] [2 1]} (set (edges (add-edges g5 [1 2])))
           #{1 2} (set (nodes (remove-nodes g1 3 4)))
           #{[1 2] [2 1]} (set (edges (remove-nodes g1 3 4)))
           #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
           #{[2 3] [3 2]} (set (edges (remove-edges
                                       g1 [1 2] [2 1] [1 3] [3 1])))))
    (testing "Weight"
      (are [expected got] (= expected got)
           77 (weight g1 1 2)
           77 (weight g2 1 2)
           77 (weight g3 1 2)
           88 (weight g4 6 5)
           1 (weight g4 7 8)))))


