(* A higher order function for a pure function *)
let incr x = x + 1
(* Requires    true
   Ensures[r]  r = x + 1
*)


let double x = x + x
(* Requires      { true }
   Ensures[res]  { res = x + x } *)



let three_add x y z = x + y + z
(* Requires      { true }
   Ensures[res]  { res = x + y + z } *)
   


let quad_fo x = (((+) x) (x) + ((+) x x))
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *)
      
(* 

let once f x = f x
(* Requires { f(a) |= { true } *->:r { r=fpure(a) } }
   Ensures[p] { p = fpure(x) }
*)


(* fpure = \x. x+1 *)

let incr_once x = once incr x
(* Requires    true
   Ensures[r]  r = x + 1
*)


let twice f x = f (f x)
(* Requires { f(a) |= { true } *->:res0 { res0=fpure(a) } }
   Ensures[res] { res = fpure(fpure(x)) }
*)

let incr_twice x = twice incr x
(* Requires    true
   Ensures[r]  r = x + 2
*)


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
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *) *)
