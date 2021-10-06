let once f x = f x
(* Requires { f(a) |= { true } *->:r { r=fpure(a) } }
   Ensures[p] { p = fpure(x) }
*)


let two_arg f x y = f x y
(* Requires { f(a,b) |= { true } *->:r { r=fpure(a,b) } }
   Ensures[p] { p = fpure(x) }
*)


let twice f x = f (f x)
(* Requires { f(a) |= { true } *->:res0 { res0=fpure(a) } }
   Ensures[res] { res = fpure(fpure(x)) }
*)