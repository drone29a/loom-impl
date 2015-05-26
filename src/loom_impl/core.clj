(ns loom-impl.core
  "Extra graph implementations for Loom."
  (:require [clojure.core.typed :as t]
            [loom.graph :as lg]
            [loom.attr]))

(def +default-weight+ 1)

(t/defalias Node t/Any)
(t/defalias Edge (t/U DirectedEdge UndirectedEdge))
(t/defalias AnyMap (t/Map t/Any t/Any))

(t/defn nodes-from-edge
  [e :- Edge] :- (t/Seq Node)
  [(lg/src e) (lg/dest e)])

(t/defn non-sorted-map?
  [x :- t/Any] :- t/Bool
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(t/defn sorted-pair
  [x :- t/Any
   y :- t/Any] :- '[t/Any t/Any]
   (if (<= (compare x y) 0)
     [x y]
     [y x]))

(t/ann-record UndirectedEdge
              [x :- Node
               y :- Node])
(deftype UndirectedEdge
    [x y]
  loom.graph/Edge
  (src [this] (x this))
  (dest [this] (y this))
  
  Object
  (equals [this other] (or (and (= (.x this) (.x other))
                                (= (.y this) (.y other)))
                           (and (= (.x this) (.y other))
                                (= (.y this) (.x other)))))
  (hashCode [this] (.hashCode (sorted-pair x y)))
  (toString [this] (str (sorted-pair x y))))

;; TODO: can probably remove this now that hashCode and equals are overwritten.
;; TODO: BUT, consider that test for equality now always includes more condition checks
;;       than if we could assume nodes are already in order, i.e., x <= y.
(comment
  (t/defn create-undirected-edge
    "Create an UndirectedEdge where the 'src' node <= 'dst' node.
   If x or y are maps, they must be sorted maps."
    [x :- Node
     y :- Node] :- UndirectedEdge
     (try
       (if (<= (compare x y) 0)
         (->UndirectedEdge x y)
         (->UndirectedEdge y x))
       (catch ClassCastException e
         (if (or (non-sorted-map? x) (non-sorted-map? y))
           (throw (Exception. "At least one node is a non-sorted map and thus not a Comparable. Consider using a record or sorted-map instead."))
           (throw e))))))

(t/ann-record UndirectedGraph
              [nodes :- (t/Set Node)
               edges :- (t/Set UndirectedEdge)
               node-edge-map :- (t/Map Node (t/Set UndirectedEdge))
               weights :- (t/Map UndirectedEdge t/Num)
               node-attrs :- (t/Map Node AnyMap)
               edge-attrs :- (t/Map UndirectedEdge AnyMap)])

;; A simple undirected graph that is weighted, editable, and attributable.
(defrecord UndirectedGraph
    
    [nodes edges node-edge-map weights node-attrs edge-attrs]
  loom.graph/Graph
  (nodes [this] (seq nodes))
  (edges [this] (seq edges))
  (has-node? [this node] (contains? nodes node))
  (has-edge? [this n1 n2] (contains? edges (->UndirectedEdge n1 n2)))
  (successors [this] (partial lg/successors this))
  (successors [this node]
    (->> (node-edge-map node)
         (mapcat nodes-from-edge)
         (filter (partial not= node))))
  (out-degree [this node] (->> (node-edge-map node)
                               count))
  (out-edges [this node] (->> (node-edge-map node)
                              seq))
  
  loom.graph/WeightedGraph
  (weight [this] (partial lg/weight this))
  (weight [this e] (:weight e))
  (weight [this n1 n2] (lg/weight this (->UndirectedEdge n1 n2)))
  
  loom.graph/EditableGraph
  (add-nodes* [this nodes*]
    (->UndirectedGraph (clojure.set/union nodes nodes*)
                       edges
                       node-edge-map
                       weights
                       node-attrs
                       edge-attrs))
  (add-edges* [this edges*]
    "Existing edges will be replaced when a duplicate edge is added. Any associated weights or attributes
    will be lost."
    (let [edge-weights nil
          nodes* (->> edges*
                      (mapcat nodes-from-edge)
                      set)
          node-edge-map* (->> edges*
                              (mapcat (fn [#^UndirectedEdge e] [[(.x e) e]
                                                                [(.y e) e]]))
                              (into {}))
          weights* (->> edges*
                        (map (fn [e] [e +default-weight+]))
                        (into {}))
          edge-attrs* (->> edges*
                           (map (fn [e] [e {}]))
                           (into {}))]
      (->UndirectedGraph (clojure.set/union nodes nodes*)
                         (clojure.set/union edges edges*)
                         (merge-with clojure.set/union node-edge-map node-edge-map*)
                         (merge weights weights*)
                         node-attrs
                         (merge edge-attrs edge-attrs*))))
  (remove-nodes* [this nodes*]
    (let [incident-edges (map (fn [n] [n (node-edge-map n)]) nodes*)
          g* (.remove-edges* this incident-edges)]
      (->UndirectedGraph (clojure.set/difference (:nodes g*) nodes*)
                         (:edges g*)
                         (:node-edge-map g*)
                         (:weights g*)
                         (apply dissoc (:node-attrs g*) nodes*)
                         (:edge-attrs g*))))
  (remove-edges* [this edges*]
    (let [removed-node-edge-maps (->> edges*
                                      (map (fn [#^UndirectedEdge e] {(.src e) [e] (.dest e) [e]}))
                                      (merge-with concat)
                                      (into []))]
      (->UndirectedGraph nodes
                         (clojure.set/difference edges edges*)
                         (loop [node-edge-map* node-edge-map
                                removed-node-edge-maps* removed-node-edge-maps]
                           (if (empty? removed-node-edge-maps*)
                             node-edge-map*
                             (let [[n removed-es] (first removed-node-edge-maps*)]
                               (recur (update node-edge-map* n (fn [es] (clojure.set/difference es (set removed-es))))
                                      (rest removed-node-edge-maps*)))))
                         ;; add weight stuff here
                         (apply dissoc weights edges*)
                         node-attrs
                         (apply dissoc edge-attrs edges*))))
  (remove-all [this]
    (->UndirectedGraph #{}
                       #{}
                       {}
                       {}
                       {}
                       {})))

(extend UndirectedGraph
  loom.attr/AttrGraph
  loom.attr/default-attr-graph-impl)

(t/defn undirected-graph
  [] :- UndirectedGraph
  (->UndirectedGraph #{} #{} {} {} {} {}))


;;;; TODO: A directed graph impl
(t/ann-record DirectedEdge
              [src :- Node
               dst :- Node
               weight :- t/Num])
(defrecord DirectedEdge
    [src dst weight]

  loom.graph/Edge
  (src [this] (src this))
  (dest [this] (dst this)))
