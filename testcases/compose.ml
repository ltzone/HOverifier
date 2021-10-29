let compose f g x = f (g x)
(*@
declare compose(f,g,x)
given fpure(int,int),gpure(int,int)
requires { true with
    f(a) |= {true} *->:r1 { fpure(a,r1) } &
    g(b) |= {true} *->:r2 { gpure(b,r2) }
 }
ensures[r] { gpure(x,z) & fpure(z,r) }
@*)

let incr x = x + 1
(*@
declare incr(x)
given 
requires { true }
ensures[res] { res = x + 1 }
@*)

(*@ 
pred incr_pure(x:int, r:int) |= r = x + 1
@*)

let incr2 x = compose incr incr x
(*@
declare incr2(x)
given 
requires { true }
ensures[p] { p = x + 2 }
@*)