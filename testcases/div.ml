let div y x = 
   (* if y = 0 then assert false else  *)
      (x / y)
(* Requires    { y != 0 }
   Ensures  [r]{ r * y = x }
*)

(*
(* Not working for apply to star : arguments don't match  *)
let div_by_one = (div 1)
(* Requires    { true }
   Ensures  [f]{ f(x) |= { true } *->:m { m * 1 = x } }
*)
(* div_one_pure(x,r): x = r  *)

*)
let div_by_one' x = div 1 x
(* Requires    { true } 
   Ensures  [m]{{ m * 1 = x } }
*)

let twice f x = f (f x)
(* Given Q
   Requires    { f(x) |= {true} *->r:{Q(x,r)} }
   Ensures  [r]{ Q(x,m) & Q(m,r) }
*)


let div_by_one_star x = twice div_by_one' x
(* Requires    { true /\ twice[Q=div_one_pure] }
   Ensures  [r]{ r=x }
*)