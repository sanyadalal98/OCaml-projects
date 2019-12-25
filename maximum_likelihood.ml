let fact n =
  let rec factorial n =
    if n = 0 then 1
    else  n * factorial (n - 1)
  in
  if n <= 0 then 1 else factorial n
;;

let binom (n, k) =
  if n < k then 0.0
  else float (fact n) /. (float (fact k) *. float (fact (n - k)))
;;

let dist_black n x (marblesTotal, marblesDrawn) =
  (binom (n, x) *. binom (marblesTotal - n, marblesDrawn - x))
  /. (binom (marblesTotal, marblesDrawn))
;;

let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []
;;

let max_in_list l =
  let rec max_in_list' pos l =
    match l with
    | [] -> assert false
    | [h]  -> (pos, h)
    | h::t ->
      let (q, mx) = max_in_list' (pos + 1) t in
      if h < mx then (q, mx)
      else (pos, h)
  in
  let (pos, _) = max_in_list' 0 l in
  pos
;;

let double x = 2 * x ;;

let tabulate_tests: (((int -> int) * int) * int list) list = [
  (((fun x -> x), -1), []);
  (((fun x -> x), 0), [0]);
  (((fun x -> double x), 4), [0;2;4;6;8]); 
  (((fun x -> double(double x)), 3), [0;4;8;12]);
  (((fun x -> (x * x) + 7), 3), [7;8;11;16])
];;

let dist_table ((marblesTotal: int), (marblesDrawn: int)) (x : int) : float list =
  let y n = dist_black n x (marblesTotal, marblesDrawn)
  in
  tabulate y marblesTotal
;;

let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([[];[]], true);
  ([[0.]], false); 
  ([[0. ; 0.2 ; 0.4] ; [0.3 ; 0.4 ; 0.5 ] ; [0.01 ; 0.2 ; 0.22]], false);
];;

let is_empty (matrix : 'a list list) : bool =
  List.for_all ( fun xs -> xs = [] ) matrix
;;
    
let dist_matrix ((total : int), (drawn : int)) (resultList : int list) : float list list =
  let y x = dist_table (total , drawn) x
  in
  List.map y resultList
;;

let rec combined_dist_table (matrix : float list list) : float list = 
  let my_func x y = 
    if x = [] && y = [] then []
    else if x = [] && y != [] then y 
    else if y = [] && x != [] then x
    else List.map2 ( *. ) x y 
  in
  List.fold_left my_func [] matrix
;;
      
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))
;;
