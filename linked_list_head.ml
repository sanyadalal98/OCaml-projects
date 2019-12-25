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
