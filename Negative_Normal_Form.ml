type prop =
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Impl of prop * prop
;;

let nnf_tests = [
  ( Atom "q" , Atom "q" );
  ( Neg(Neg(Atom "p")) , Atom "p" );
  ( Neg(Neg(Neg(Atom "q"))) , Neg(Atom "q") );
  ( Neg(Conj(Atom "p", Disj(Atom "q", Atom "r"))), Disj(Neg(Atom "p"), Conj(Neg(Atom "q"), Neg (Atom "r"))) );
  ( Neg(Disj(Atom "p", Conj(Atom "q", Atom "r"))), Conj(Neg(Atom "p"), Disj(Neg(Atom "q"), Neg(Atom "r"))) ); 
  ( Neg(Impl(Atom "p", Atom "q")), Conj(Atom "p", Neg(Atom "q")) );
  ( Conj(Neg(Conj(Atom "a", Neg(Atom "b"))), Atom "c"), Conj(Disj(Neg(Atom "a"), Atom "b"), Atom "c") );
  ( Neg(Impl(Impl(Atom "r", Atom "q"), Neg(Atom "p"))), Conj(Disj(Neg(Atom "r"), Atom "q"), Atom "p") );
  ( Impl(Impl(Atom "q", Atom "r"), Atom "p"), Disj(Conj(Atom "q", Neg(Atom "r")), Atom "p") );
];;

let rec nnf (p : prop) : prop =
  match p with
  |Atom x -> Atom x
  |Neg ( Neg ( x ) ) -> nnf x
  |Neg ( Impl ( x , y ) ) -> Conj ( ( nnf x ) , nnf ( Neg ( y ) ) )
  |Impl ( x , y ) -> Disj ( nnf ( Neg ( x ) ) , y )
  |Neg ( Conj ( x , y ) ) -> Disj ( ( nnf ( Neg ( x ) ) ) , ( nnf ( Neg ( y ) ) ) )
  |Neg ( Disj ( x , y ) ) -> Conj ( ( nnf ( Neg ( x ) ) ) , ( nnf ( Neg ( y ) ) ) )
  |Conj ( x , y ) -> Conj ( ( nnf x ) , ( nnf y ) )
  |Disj ( x , y ) -> Disj ( ( nnf x ) , ( nnf y ) )
  |Neg ( x ) -> Neg ( nnf x )
;;
