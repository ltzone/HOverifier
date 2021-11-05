(* 
McCarthy function, example from
https://arxiv.org/pdf/1306.5264.pdf 
*)

let rec mc x = if 100 < x then x - 10 else mc (mc (x + 11))
(*@
declare mc(x)
given 
requires { x <= 101 }
ensures[res] { res = 91 }
@*)