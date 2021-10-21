
let foo x y = x - y
(* true *-> res = x - y *)


let bar f a b = f (f a b) b
(* Requires 
      f(m,n) |= { n >= 0 } *->:res { res <= m}
      b >= 0
      
   Ensures[r]
      r <= a

*)

let baz c d = (bar foo c d)
(* Requires 
      d >= 0
      
   Ensures[r]
      r <= c
*)
