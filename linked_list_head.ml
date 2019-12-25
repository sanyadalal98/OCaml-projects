type 'a llist =
  | Nil
  | Cons of (float * 'a) * 'a lcell * 'a lcell
and 'a lcell = ('a llist) ref
;;

(* Constructing singleton linked lists, i.e. a linked list with just one element in it *)
let singleton (init: float * 'a): 'a lcell ref =
  let l = ref (Cons (init, ref Nil, ref Nil)) in
  let front = ref l in
  front
;;

(* Some sample linked lists. *)
let empty_list = ref (ref Nil)
let one_element_list = singleton (3.0, "a")
let two_element_list =
  let second = ref Nil in
  let first = ref (Cons ((2.3, "b"), ref Nil, second)) in
  second := Cons ((3.3, "a"), first, ref Nil);
  ref first
;;

(* Converting linked lists to regular OCaml lists. *)
let rec list_of_lcell lcell =
  match !lcell with
  | Nil -> []
  | Cons (d, _, next) -> d :: list_of_lcell next
;;

(* add head to doubly linked list *)
let add_head x head =
  match !(!head) with
  | Nil -> head := ref(Cons(x, ref Nil, ref Nil))
  | Cons(d, prev, next) ->
      let new_cell = ref(Cons(x, ref Nil, !head)) in
      (
        !head := Cons(d, new_cell, next);
        head := new_cell
      )
;;

(* remove head of doubly linked list *)
let remove p head =
  let rec remove' ll =
    match !ll with
    | Nil -> ()
    | Cons((x,y), prev, next) ->
        if p x then
          match (!prev, !next) with
          |(Nil, Nil) -> head := ref Nil
          |(Nil, Cons(d, prev_n, next_n)) ->
              (
                prev_n := Nil;
                head := next
              )
          |(Cons(d, prev_p, next_p), Nil) ->
              next_p := Nil
          |(Cons(d, prev_p, next_p), Cons(d', prev_n, next_n)) ->
              (
                prev := Cons(d, prev_p, next);
                next := Cons(d', prev, next_n)
              )
        else remove' next
  in
  remove' !head ;;
