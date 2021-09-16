
let plus x y = x + y
(*@ Requires     true
    Ensures[res]   {res = x+y}  *)


let plus_one = plus 1
(*@ Requires       true
    Ensures[res]   { res(y) |= {true} *->:res {res = 1+y} } *)
(* can be verified by applying FV-app

  { true } plus { true } 
  { true } 1:v  { v = 1 }
  { v = 1 } |- plus.pre = true
  plus(x,y) = { true } *->:r {r = x + y}
  -----------FV-app
  { true } plus 1:g {v=1 /\ g(y) = {true} *->:r {r = v + y} }
  -----------FV-conseq
  { true } plus 1:res {res(y) = {true} *->:r {r = 1 + y} }

*)


let twice f x = f (f x)
(* Requires      f(a) |= { true } *->:r { r=fpure(a) }
   Ensures[res]  res=fpure(fpure(a))
*)


let plus_two = twice plus_one
(* Requires      true  
   Ensures[res]  res(x)={true} *->:r {r=x+2} )
*)

(*

\delta_2 |- twice.pre

   res(y) |= {true} *->:r {r = 1+y} } 
|- ex fpure, res(a) |= { true } *->:r { r=fpure(a) }
how to unify?

fpure: \a.1+a


\delta_3 = r=fpure(fpure(a)) /\ fpure=\a.1+a
g(x) |=  { f(a) |= { true } *->:r { r=fpure(a) } } 
            (*<---- needs to bre pruned *)
          *->:res  {res=fpure(fpure(a))}


Alternative solution: provide straightforward specification for pure function,

plus(x,y) |= x+y
plus_one(x) |= x+1
twice |=
    Requires      f(a)|=fpure(a)
    Ensures[res]  res=fpure(fpure(a))
*)



let mult_n x = fun y -> x * y
(*@ Requires true
    Ensures  res(y)= {true} *-> {res = x*y} *)

(* test case *)
let () = plus_two 10 |> print_int (* 12 *)

