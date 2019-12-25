(* intToBin: converts an integer to a binary number, which is written in reverse. Example: 6 = 110 = Zero(One(One E)) *)

let intToBin_tests = [
  (0, E);
  (1, One E);
  (2, Zero ( One E ));
  (5, One ( Zero ( One E)));
  (6, Zero ( One ( One E ))); 
  (8, Zero ( Zero ( Zero ( One E ))));
  (15, One ( One ( One ( One E))))
];;

let rec intToBin (n : int) : bnum =
  match n with
  | 0 -> E 
  | n ->
      if n mod 2 = 0
      then 
        Zero ( intToBin (n / 2 ))
      else
        One ( intToBin ( n / 2 )) 
;;
 
(* binToInt: Converts any binary number, even if it has leading zeros, to its integer conversion *)
let binToInt_tests = [
  (E , 0);
  (One E , 1);
  (Zero ( One E ) , 2);
  (Zero ( One (Zero (Zero E))) , 2);
  (One ( One ( One ( One E))) , 15)
];;

let rec binToInt (b : bnum) : int = 
  match b with
  | E -> 0
  | Zero E -> 0
  | One E -> 1 
  | Zero (something) -> 2 * (binToInt something)
  | One ( something ) -> 1 + 2 * (binToInt something)
;;
