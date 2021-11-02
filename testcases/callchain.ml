let foo x = x + 2
(*@ declare foo(x:int)
given 
requires { true }
ensures[r:int] { r = x + 2 }
@*)


let goo w = w + 2
(*@ declare goo(w:int)
given 
requires { true }
ensures[q:int] { w + 2 = q }
@*)

let bar z = goo (foo z) + goo (foo z) 
(* + foo (goo z) *)
(*@ declare bar(z:int)
given 
requires { true }
ensures[p:int] { p = 2 * z + 8 }
@*)