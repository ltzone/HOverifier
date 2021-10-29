(* A higher order function for a pure function *)
let incr x = x + 1
(*@
declare incr(x)
requires    { true }
ensures[r]  { r = x + 1 }
@*)



let double x = x + x

(*@
declare double(x)
 requires      { true }
 ensures[res]  { res = x + x }
@*)



let three_add x y z = x + y + z

(*@
declare three_add(x, y, z)
 requires      { true }
 ensures[res]  { res = x + y + z }
@*)



let quad_fo x = (((+) x) (x) + ((+) x x))

(*@
declare quad_fo(x)
 requires      { true }
 ensures[res]  { res = x + x + x + x }
@*)  



let once f x = f x 

(*@
declare once(f, x)
 given fpure(int, int)
 requires { true with
    f(a) |= { true } *->:r { fpure(a,r) } }
 ensures[p] { fpure(x,p) }
@*)


let twice f x = f (f x)

(*@
declare twice (f, x)
  given fpure2(int, int)
 requires { true with
   f(a) |= { true } *->:res0 { fpure2(a,res0) } }
 ensures[res] { fpure2(x,t) & fpure2(t,res) }
@*)



let incr_once x1 = once incr x1

(*@
declare incr_once (x1)
 requires    { true }
 ensures[r]  { r = x1 + 1 }
@*)


let incr_twice x2 = twice incr x2

(*@
declare incr_twice (x2)
 requires    { true }
 ensures[r]  { r = x2 + 2 }
@*)


let quad x = (twice double) x
(*@
declare quad (x)
 requires      { true }
 ensures[res]  { res = x + x + x + x }
@*)

(*@
pred incr_pure(q1:int,r1:int) |=
  r1 = q1 + 1

pred double_pure(q2:int,r2:int) |=
  r2 = q2 * 2
@*)