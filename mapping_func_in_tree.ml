(* map_tree applies a function f, which is taken as input, to each node in the tree *)
let map_tree_tests: (((int -> int) * int tree) * int tree) list = [
  ((identity, t), t);
  ((identity, Empty), Empty);
  ((add1, Node(1, Empty, Empty, Empty)), Node(2, Empty, Empty, Empty));
  (((fun x -> add1(add1 x)), t), Node(3,Node(4, Empty, Empty, Empty), Node(5, Empty, Empty, Empty), Node(6, Empty, Empty, Empty)));
];;

let rec map_tree f t =
  match t with
  | Empty -> Empty
  | Node(x, l, m, r) ->
      Node((f x) , (map_tree f l) , (map_tree f m) , (map_tree f r))
;;

(* delete_data makes a (k, v) tree a key-only tree *)
let delete_data t =
  map_tree (fun (x, y) -> x) t
;;  

(* fold_tree changes every 'Empty' tree to some value e, and then applies a function f to the resulting tuple to get a value *)

let fold_tree_tests:
  (((int * int * int * int -> int) * int * int tree) * int) list =
  [
    (((fun _ -> 0), 3, t), 0);
    (((fun (a, b, c, d) -> a + b + c + d), 1, Empty), 1);
    (((fun (a, b, c, d) -> a + b + c + d), 0, Node(1, Empty, Empty, Empty)), 1);
    (((fun (a, b, c, d) -> a + b + c + d), 0, t), 10)
  ];;

let rec fold_tree f e t =
  match t with
  | Empty -> e
  | Node(x, l, m, r) ->
      f(x, (fold_tree f e l), (fold_tree f e m), (fold_tree f e r))
;;


let size_tests: (int tree * int) list = [
  (Empty, 0);
  (Node(1, Empty, Empty, Empty) , 1);
];;

let size t =
  fold_tree (fun (x, l, m, r) -> 1 + l + m + r) 0 t
;;

(* reflect: left and right subtrees are exchanged *)
let reflect_tests: (int tree * int tree) list = [
  (Empty, Empty);
  (Node(1, Empty, Empty, Empty), Node(1, Empty, Empty, Empty));
  (Node(1, Empty, Node(1, Empty, Empty, Empty), Empty), Node(1, Empty, Node(1, Empty, Empty, Empty), Empty));
  (Node(1, Node(2, Empty, Empty, Empty), Empty, Node(3, Empty, Empty, Empty)), Node(1, Node(3, Empty, Empty, Empty), Empty, Node(2, Empty, Empty, Empty)));
];;

let reflect t =
  fold_tree (fun (x, l, m, r) -> Node(x, r, m, l)) (Empty) t
;;

(* postorder gives a a list containing nodes in the post order traversal of the tree *)
let postorder_tests: (int tree * int list) list = [
  (Empty, []);
  (Node(1, Empty, Empty, Empty),[1]);
  (Node(1, Node(2, Empty, Empty, Empty), Empty, Node(3, Empty, Empty, Empty)), [2;3;1])
];;

let postorder t = 
  fold_tree (fun (x, l, m, r) -> (l @ m @ r @ [x])) [] t
;;
