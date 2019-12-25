(* The type of graphs. *)
type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a) list
};;

(* neighbours: finds neighbours of a vertex in the graph *)
let neighbours_tests: ((string graph * string) * string list) list = [
  (({nodes = []; edges = []}, "a"), []);
  (({nodes = ["a"]; edges = []}, "a"), []);
  (({nodes = ["a"; "b"]; edges = [("a", "b")]}, "a"), ["b"]);
  (({nodes = ["a"; "b"]; edges = [("b", "a")]}, "a"), []);
  (({nodes = ["a"; "b"; "c"]; edges = [("a", "b"); ("a", "c"); ("a", "d")]}, "a"), ["b"; "c"; "d"]);
];;

let neighbours g vertex =
  let f acc (v, nbr) =
    if v = vertex then acc @ [nbr]
    else acc
  in
  try List.fold_left f [] g.edges
  with
  |_ -> []
;;

(* find_path: recusive function giving you the path from a to b in a graph *)
let find_path g a b = 
  let rec aux_node node visited = 
    if List.mem node visited then raise Fail
    else if node = b then List.rev (node :: visited)
    else aux_list (neighbours g node) (node :: visited) 
        
  and aux_list nodes visited =
    match nodes with
    | [] -> raise Fail
    | h :: t ->
        try aux_node h visited
        with Fail -> aux_list t visited 
  in
  match g.edges with
  | [] -> raise Fail
  | _ -> aux_node a []      
;;

(* tail recursive version of find_path *)
let find_path' g a b =
  let rec aux_node node visited fc sc =
    if List.mem node visited then fc ()
    else if node = b then sc visited node
    else aux_list (neighbours g node) (node :: visited) fc sc

  and aux_list nodes visited fc sc =
    match nodes with
    | [] -> raise Fail
    | h :: t ->
        aux_node h visited (fun () -> aux_list t visited fc sc) sc
          
  in
  match g.edges with
  | [] -> raise Fail
  | _ -> aux_node a [] (fun () -> raise Fail) (fun l el -> l @ [el])
;;
