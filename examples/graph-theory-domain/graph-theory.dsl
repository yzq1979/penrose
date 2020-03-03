-- Main types
-- TODO: support list?
-- type List ('X : type) : type
-- function Cons ['X : type] (head : 'X, tail : List('X)) : List('X)
-- function Nil ['X : type] : List('X)

type Node
type Edge
type Graph

-- Subtypes
type UndirectedEdge
type DirectedEdge
type UndirectedGraph
type DirectedGraph
type Tree
type BinaryTree
type Face
type Path
type Cycle

-- function GenerateGraph() : Graph
-- function MkGraph : List(Node) * List(Edge) -> Graph
-- function MkEdge : Node * Node -> Edge
-- function MkUndirectedEdge : Node * Node -> UndirectedEdge
-- function MkDirectedEdge : Node * Node -> DirectedEdge 
-- Would be nice to name these fields "from" and "to"

UndirectedEdge <: Edge
DirectedEdge <: Edge
UndirectedGraph <: Graph
DirectedGraph <: Graph
Tree <: Graph
BinaryTree <: Tree
Face <: Graph
Path <: Graph
Cycle <: Graph

-- operator Union(G1 : Graph, G2 : Graph) : Graph
-- operator Product(G1 : Graph, G2 : Graph) : Graph
-- operator Neighbors(v : Node) : List(Node)
-- operator FindFace(G : Graph) : Face
-- operator FindVertex(G : Graph) : Node

predicate to: Node parent * Node child
predicate ParentOf2: Node parent * Node child1 * Node child2
predicate ParentOf3: Node parent * Node child1 * Node child2 * Node child3

predicate SelectedV : Node
predicate SelectedE : Edge
predicate Colored : Graph
predicate FullyConnected : Graph
predicate Bipartite : Graph
predicate SmallerDegree: Node * Node
predicate InV: Node * Graph
predicate InE: Edge * Graph
predicate InF: Face * Graph

notation "p -> c" ~ "to(p, c)"