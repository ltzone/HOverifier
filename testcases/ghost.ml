(* 
foo :: (Bool -> Int ) -> ()
foo f = assert (f True == f False)

foo :: n:Int -> (Bool -> SInt n) -> ()

foo :: [n:Int] -> (Bool -> SInt n) -> ()


verrify: 
[foo (\z -> 1)] meets the condition in [assert]
*)


let foo f= 
  if f 2 = f 4 then 1 else 0
(*@
declare foo(f)
// implicit global n
// given
requires { EX (n:int), true with
    f(b) |= { true } *->:r { n = r } }
ensures[res] { res = 1 }
@*)

(*@ 
pred foo_inst(p:int) |= p = 5  
@*)

let g b = 5
(*@
declare g(b)
requires { true }
ensures[r] { r = 5 }
@*)

let test = foo g
(*@
declare test()  
requires { true }
ensures[res] { res = 1 }
@*)







(* let foo' (n:int) (f: int -> int) = 
  if f 2 = f 4 then 1 else 0
(*@
declare foo'(f)
given fpure(int,int)
// implicit global n
requires { true with
    f(b) |= { true } *->:r { fpure(n,r) } }
ensures[res] { res = 1 }

pred foo_inst(p:int,q:int) |= p = q
@*) *)

