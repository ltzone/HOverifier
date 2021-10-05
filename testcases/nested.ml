let div y x = if y = 0 then assert false else (x / y)
(* Requires    { y != 0 }
   Ensures  [r]{ r * y = x }
*)

let div_by_one = (div 1)
(* Requires    { true }
   Ensures  [f]{ f(x) |= { true } *->:m { m * 1 = x } }
*)


let twice f x = f (f x)
(* Requires    { f(x) |= {true} *->r:{r=fpure(x)} }
   Ensures  [r]{ r=fpure(fpure(x)) }
*)


let div_by_one_star x = twice div_by_one x
(* Requires    { true }
   Ensures  [r]{ r=x }
*)