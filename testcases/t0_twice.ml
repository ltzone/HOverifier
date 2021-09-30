(* A higher order function for a pure function *)

let once f x = f x
(* forall fpure,
     Requires      f(a) |= { true } *->:r { r=fpure(a) }
     Ensures[res]  res=fpure(x)
*)

let incr x = x + 1
(* Requires    true
   Ensures[r]  r = x + 1
*)

(* fpure = \x. x+1 *)

let incr_once x = once incr x
(* Requires    true
   Ensures[r]  r = x + 1
*)

let twice f x = f (f x)
(*   Requires      f(a) |= { true } *->:r { r=fpure(a) }
     Ensures[res]  res=fpure(fpure(x))
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


let double x = x + x
(* Requires      { true }
   Ensures[res]  { res = x + x } *)


(***** ISSUE: can't mechanize the process of unifying [fpure] with the post condition of double *)

(* twice f =
   Requires      f(a) |= { true } *->:r { r=fpure(a) }
   Ensures[res]  res=fpure(fpure(a)) *)

(* unify r=fpure(a) <-> res=x+x [r/res,a/x] 
         r=?        <-> r=x+x
*)


let quad x = (twice double) x
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *)
