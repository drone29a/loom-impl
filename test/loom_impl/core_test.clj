(ns loom-impl.core-test
  (:require [clojure.test :refer :all]
            [loom-impl.core :refer :all]
            [loom.graph :as lg]))

(comment
  (deftest attr-graph-test
    (let [g (digraph [1 2] [2 3] [2 4] [3 5] [4 5])
          lg1 (-> g
                  (add-attr 1 :label "node label")
                  (add-attr 2 3 :label "edge label"))
          lg2 (-> g
                  (add-attr-to-nodes
                   :label "node odd" [1 3 5])
                  (add-attr-to-nodes
                   :label "node even" [2 4])
                  (add-attr-to-edges
                   :label "edge from node 2" [[2 3] [2 4]])
                  (add-attr-to-edges
                   :label "edge to node 5" [[3 5] [4 5]]))]
      (is (= "node label" (attr lg1 1 :label)))
      (is (= "edge label" (attr lg1 2 3 :label)))
      (is (= "node odd" (attr lg2 1 :label)))
      (is (= "node odd" (attr lg2 3 :label)))
      (is (= "node odd" (attr lg2 5 :label)))
      (is (= "node even" (attr lg2 2 :label)))
      (is (= "node even" (attr lg2 4 :label)))
      (is (= "edge from node 2" (attr lg2 2 3 :label)))
      (is (= "edge from node 2" (attr lg2 2 4 :label)))
      (is (= "edge to node 5" (attr lg2 3 5 :label)))
      (is (= "edge to node 5" (attr lg2 4 5 :label)))))

  (deftest labeled-graph-test
    (let [g (digraph [1 2] [2 3] [2 4] [3 5] [4 5])
          lg1 (-> g
                  (add-label 1 "node label")
                  (add-label 2 3 "edge label"))
          lg2 (-> (digraph)
                  (add-labeled-nodes
                   1 "node label 1"
                   2 "node label 2")
                  (add-labeled-edges
                   [1 2] "edge label 1"
                   [2 3] "edge label 2"))]
      (is (= "node label" (label lg1 1)))
      (is (= "edge label" (label lg1 2 3)))
      (is (= #{1 2 3} (set (nodes lg2))))
      (is (= #{[1 2] [2 3]} (set (edges lg2))))
      (is (= "node label 1" (label lg2 1)))
      (is (= "node label 2" (label lg2 2)))
      (is (= "edge label 1" (label lg2 1 2)))
      (is (= "edge label 2" (label lg2 2 3))))))

