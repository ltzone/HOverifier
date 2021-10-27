let foo (f: int -> int) = 
  if f 2 = f 4 then 1 else 0
(*@
declare foo(f)
// given fpure(int)
// implicit global n
requires { true with
    f(b) |= { true } *->:r { n = r } }
ensures[res] { res = 1 }
@*)

(*@ pred foo_inst(p:int) |= p = 5  @*)

let g (b: int) = 5
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

