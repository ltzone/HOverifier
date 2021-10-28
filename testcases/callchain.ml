let foo x = x + 2
(*@ declare foo(x)
given 
requires { true }
ensures[r] { r = x + 2 }
@*)


let goo w = w + 2
(*@ declare goo(w)
given 
requires { true }
ensures[q] { w + 2 = q }
@*)

let bar z = goo (foo z) + goo (foo z) 
(* + foo (goo z) *)
(*@ declare bar(z)
given 
requires { true }
ensures[p] { p = 2 * z + 8 }

@*)