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



(* fpure = \x. x+1 *)
let incr_once x1 = once incr x1

(*@
declare incr_once (x1)
 requires    { true }
 ensures[r]  { r = x1 + 1 }
@*)


(* true
 for once [incr ; x]: 
 
 arglist = [incr ; x]

 Requires { f(a) |= { true } *->:r { r=fpure(a) } }
 Ensures[p] { p = fpure(x) }

 derive: env |- true => incr <: { true } *->:r { r=fpure(a) }

 Goal: i.e. true -> incr.pre /\ incr.post -> r=fpure(a) [SOLVER]

 SOLVER returns: success with fpure=\x.x+1


 then FV-app gets: 
  ~~r: r=fpure(x)~~
  r: r=fpure(x) with \x.x+1


REMARK: No matter how we choose to instantiate the fpure, it seems that
  fpure should always be treated explicitly by the solver

*)



let incr_twice x2 = twice incr x2

(*@
declare incr_twice (x2)
 requires    { true }
 ensures[r]  { r = x2 + 2 }
@*)



(*

** assertion language should be able to include quantifying over functions ([fpure])

------------------------------------------------ HIP do forward function application, neeeds to treat polymorphism
{ f(a) |= { true } *->:r { r=fpure(a) }  }
   f x : r
{ r = fpure(x) & f(a) |= { true } *->:r { r=fpure(a) }  }
------------------------------------------------ HIP do forward function application, neeeds to treat polymorphism
{ r = fpure(x) & f(a) |= { true } *->:r { r=fpure(a) }  }
    f r : res
{ res = fpure(r) & r = fpure(x) & f(a) |= { true } *->:r { r=fpure(a) } }
------------------------------------------------ SLEEK solver, needs to encode uninterpreted function `fpure`
  { res = fpure(fpure(x)) }

  Verify the body of a higher order function by
    [FV-app] [FV-call]
*)





(***** ISSUE: can't mechanize the process of unifying [fpure] with the post condition of double *)

(* twice f =
   Requires      f(a) |= { true } *->:r { r=fpure(a) }
   Ensures[res]  res=fpure(fpure(a)) *)

(* unify r=fpure(a) <-> res=x+x [r/res,a/x] 
         r=?        <-> r=x+x
*)


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