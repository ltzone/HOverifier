let f x = x + 1
(*@
declare f(x)
given 
requires { true }
ensures[r] { r = x + 1 }
@*)

let g x = x + 2
(*@ 
declare g(x)
given 
requires { true }
ensures[res] { res = x + 2 }
@*)

let test x = if 0 <= x then f else g  
(*@ 
declare test(x)
given 
requires { 0 <= x }
ensures[h] { true with 
  h(y) |= { true } *->:r { r = y + 1 }
}
@*)