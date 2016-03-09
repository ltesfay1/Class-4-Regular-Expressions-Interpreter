(*-------------------------------------------------------------------------*)
(* functions from lecture you may use *)

let rec map f l = match l with
    [] -> []
  | (h::t) -> (f h)::(map f t)
;;

let rec fold f a l = match l with
    [] -> a
  | (h::t) -> fold f (f a h) t
;;

let length x = fold (fun a y -> a+1) 0 x
;;

let rev x = fold (fun a y -> y::a) [] x
;;

let rec fold_right f l a =
  match l with
      [] -> a
    | h::t ->
      let a' = fold_right f t a in
      f h a'
;;

let append x y =
  fold_right (fun z a -> z::a) x y;;
 
(* ===== *)
(* Part 1: Higher order functions *)
(* ===== *)

(* 
count_x x y 
'a -> 'a list -> int
returns how many times x occurs in y 
Example: count_x 3 [1;3;1;1;3] = 2, count_x "hello" ["there";"ralph"] = 0
*)
let counter acc _ = acc + 1

let rec count x y l = match y with
[] -> l
    | (h::t) -> if h == x then count x t (h::l) else count x t l;;

let count_x x y =
   let l = count x y [] in
   fold counter 0 l
;;

(*
div_by_x x y
int -> int list -> bool list
return a list of booleans, each one indicating whether the correponding element in y is divisible by x
Example: div_by_x 2 [1;3;4;8;0] = [false;false;true;true;true]
*)
let div_by_x x y =
   let func h = if x != 0 && h mod x = 0 then true else false in
   map func y;;
(*
div_by_first y
int list -> bool list
return a list of booleans, each one indicating whether the correponding element in y is divisible by the first element of y
Example: div_by_first [2;3;4;8;0] = [true;false;true;true;true] and div_by_first [] = []
*)
let div_by_first y = match y with
[] -> []
    | h::t -> div_by_x h y;;
  
(* 
grow_lists x y 	 
'a list -> 'a list -> 'a list list 	 
return a list of lists, where each element of x is prepended to y
resulting lists must be in same order as in x
Example: grow_lists [1;2] [3;4] => [[1;3;4]; [2;3;4]]
*)

let grow_lists x y = 
   map (fun h -> [h]@y) x;;




(* 
concat_lists x 	
'a list list -> 'a list 	
return a list consisting of the lists in x concatenated together
(note just top level of lists is concatenated, unlike List.flatten)
Examples: concat_lists [[1;2];[7];[5;4;3]] = [1;2;7;5;4;3]
concat_lists [[[1;2;3];[2]];[[7]]] = [[1;2;3];[2];[7]]
*)

let concat_lists x =
   fold (fun z a -> z@a) [] x;;

    
(* ===== *)
(* Part 2a: Programming with datatypes -- binary tree of integers *)
(* ===== *)
    
(* The following is an implementation of binary search trees whose nodes contain integeres *)

(* The type of a tree is a datatype: 
   a tree is either empty (just a leaf), or
   it is a node containing an integer, a left subtree, and a right subtree *)
type int_tree =
    IntLeaf 
  | IntNode of int * int_tree * int_tree

(* An empty tree is a leaf *)
let empty_int_tree = IntLeaf

(* Inserting x into tree t: 
   if the tree is empty, then adding x produces a single-node tree
   if x is greater than the value at the current node, 
     return a tree whose right subtree is replaced by the tree produced by
       inserting x into the current right subtree
   if x is already in the tree, then return the tree unchanged
   if x is less than the value at the current node, 
     do the opposite of when x was greater (i.e., insert in the left subtree)
*)
let rec int_insert x t =
  match t with
      IntLeaf -> IntNode(x,IntLeaf,IntLeaf)
    | IntNode (y,l,r) when x > y -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when x = y -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

(* Checking whether x occurs in tree t: 
   Follow the same sort of procedure as insertion, but return true if x is in the tree
   and false otherwise
*)
let rec int_mem x t =
  match t with
      IntLeaf -> false
    | IntNode (y,l,r) when x > y -> int_mem x r
    | IntNode (y,l,r) when x = y -> true
    | IntNode (y,l,r) -> int_mem x l

(* Implement the following functions, operating on trees *)

(* 
int_size t 
int_tree -> int
returns how many nodes are in the tree
Example: int_size empty_int_tree = 0, int_size (int_insert 1 (int_insert 2 empty_int_tree)) = 2 
*)
let rec int_size t = 
   match t with
   IntLeaf -> 0
    | IntNode (y,l,r) -> 1 + int_size l + int_size r
;;
(* 
int_to_list t 
int_tree -> int list
returns a list of all values in the tree, resulting from an in-order traversal
Examples: int_to_list (int_insert 2 (int_insert 1 empty_int_tree)) = [1;2]
int_to_list (int_insert 2 (int_insert 2 (int_insert 3 empty_int_tree))) = [2;3]
*)    
let rec int_to_list t =
   match t with
   IntLeaf -> []
    | IntNode (y,l,r) -> (int_to_list l)@y::(int_to_list r)
;;  

(* 
int_insert_all xs t 
int list -> int_tree -> int_tree
returns a tree t' that is the same as t but has all integers in xs added to it
Examples: int_to_list (int_insert_all [1;2;3] empty_int_tree) = [1;2;3]
Note: Try to use fold to implement this function on one line
*)    
let rec int_insert_all xs t =
   fold (fun acc element -> int_insert element acc) t xs
;;

(* 
max_elem t 
int_tree -> int
returns the maximum element in the tree
Example: max_elem (int_insert_all [1;2;3] empty_int_tree) = 3
Note: This should take time O(height of the tree)
*)    
let rec max_elem t =
match t with
IntLeaf -> failwith "max_elem"
    | IntNode (y,l,r) -> if r = IntLeaf then y else max_elem r
;;

(* ===== *)
(* Part 2b: Programming with datatypes -- polymorphic binary tree *)
(* ===== *)

(* The previous part defined a binary tree over only integers. But we
   should be able to define a binary tree over any kind of data, as
   long as it is totally ordered. We capture this idea with the
   following data type definitions.
*)

(* This says, as before, that a tree is either a leaf or a node, but
   now the node may contain a value of any type 'a, not just ints. *)
type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree

(* Since a tree may contain values of any type, we need a way to
   compare those values. For this purpose, we define the type of
   comparison functions: they take two values of type 'a and return an
   int. If the returned value is negative, then the first value is less
   than the second; if positive, then the first is greater; if 0, then
   the two values are equal. *)
type 'a compfn = 'a -> 'a -> int

(* Finally: a polymorphic binary tree: This definition bundles the
   tree with its comparison function so that the latter can be used when needed
   by the tree's functions, pinsert and pmem, below. *)
type 'a ptree = 'a compfn * 'a atree

(* An empty tree is a leaf and a comparison function *)
let empty_ptree f : 'a ptree = (f,Leaf)

(*
pinsert x t 
'a -> 'a ptree -> 'a ptree
returns a tree t' that is the same as t but has x added to it
*)   
let rec annon_insert x t f=
  match t with
      Leaf -> Node(x,Leaf,Leaf)
    | Node (y,l,r)  when ((f x y) > 0) -> Node (y,l,(annon_insert x r f))
    | Node (y,l,r)  when ((f x y) = 0) -> t
    | Node (y,l,r)  -> Node (y,(annon_insert x l f),r)


let pinsert x ((f,t):'a ptree) = 
   (f, (annon_insert x t f))
;;
(*
pmem x t 
'a -> 'a ptree -> bool
returns whether x appears in tree t
*)
let rec annon_mem x t f=
  match t with
      Leaf -> false
    | Node (y,l,r) when ((f x y) > 0) -> annon_mem x r f
    | Node (y,l,r) when ((f x y) = 0) -> true
    | Node (y,l,r) -> annon_mem x l f

;;

let pmem x ((f,t):'a ptree) =
   annon_mem x t f
;;
(* Examples:

let t0 = empty_ptree (fun x y -> if x < y then -1 else if x > y then 1 else 0);;
let t1 = pinsert 1 (pinsert 8 (pinsert 5 t0));;
pmem 5 t0 = false;;
pmem 5 t1 = true;;
pmem 1 t1 = true;;
pmem 2 t1 = false;;

*)
     
(* ===== *)
(* Part 3: Programming with records -- graphs *)
(* ===== *)

(* A graph is a set of nodes, represented as an int_tree, and a list
   of edges.  A node is represented as an integer, and an edge is a
   record identifying its source and destination nodes. *)
type node = int;;
type edge = { src: node; dst: node; };;
type graph = { nodes: int_tree; edges: edge list };;

(* an empty graph (has type graph) *)
let empty_graph = {nodes = empty_int_tree; edges = [] }

(* 
add_edge e g 
edge -> graph -> graph
returns a new graph that is the same as g, but with e added
Note: does not worry about duplicate edges
*)
let add_edge ({ src = s; dst = d } as e) { nodes = ns; edges = es } =
  let ns' = int_insert s ns in
  let ns'' = int_insert d ns' in
  let es' = e::es in
  { nodes = ns''; edges = es' }

(* 
add_edges es g 
edge list -> graph -> graph
returns a new graph that is the same as g, but with all edges in es added
Note: does not worry about duplicate edges
*)
let add_edges es g =
  fold (fun g e -> add_edge e g) g es

(* IMPLEMENT THE FOLLOWING *)
    
(* 
is_empty g 
graph -> bool
returns whether the graph is empty
Example: is_empty empty_graph = true
is_empty (add_edge {src=1; dst=2} empty_graph) = false
*)      
let is_empty g = 
   g = empty_graph
;;
(* 
num_nodes g 
graph -> int
returns the number of nodes that appear in g
Example: num_nodes (add_edge {src=1; dst=2} empty_graph) = 2
Example: num_nodes (add_edge {src=1; dst=1} empty_graph) = 1
*)
let num_nodes g = match g with
{nodes;edges} -> (int_size nodes);;
(* 
is_dst x e 
node -> edge -> bool
returns true if x is the destination of the given edge
Example: is_dst 1 { src=1; dst = 2 } = false
is_dst 2 {src = 1; dst = 2 } = true
*)    

let is_dst x e = match e with
{src; dst} -> if dst = x then true else false;;
    
(* 
src_edges x g
node -> graph -> edge list
returns those edges in g whose source node is x
Example: 
src_edges 1 
  (add_edges [{src=1;dst=2}; {src=1;dst=3}; {src=2;dst=2}] empty_graph) =
[{src=1;dst=2}; {src=1;dst=3}]
*)    

let rec check_edge x edges = match edges with
 [] -> []
| h::t -> match h with {src; dst} -> if src = x then h::(check_edge x t) else (check_edge x t);;

let src_edges x g = match g with
{nodes; edges} -> rev (check_edge x edges);;

(* 
reachable n g
node -> graph -> int_tree
returns a set of nodes reachable from n, in g, where the set is represented as an int_tree
Example: 
int_to_list 
  (reachable 1 
    (add_edges [{src=1;dst=2}; {src=1;dst=3}; {src=2;dst=2}] empty_graph)) =
[1;2;3];;
int_to_list 
  (reachable 3 
    (add_edges [{src=1;dst=2}; {src=1;dst=3}; {src=2;dst=2}] empty_graph)) =
[3]
*)    
let reachable n g =
  failwith "unimplemented"
