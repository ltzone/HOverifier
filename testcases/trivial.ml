let once f x = f x

(*@
declare once(f,x)
given fpure2(int,int)
requires { true with
   f(a) |= { true } *->:r { fpure2(a,r)}  }
ensures[p] { fpure2(x,p) }
@*)

let two_arg f x y = f x y

(*@
declare two_arg(x,y)
given fpure3(int,int,int)
requires { true with
  f(a,b) |= {true} *->:r {fpure3(a,b,r)} }
ensures[res] { fpure3(x,y,res) }
@*)



let twice f x = f (f x)
(*@
declare twice(f,x)
given fpure(int,int)
requires { true with
    f(a) |= { true } *->:res0 {fpure(a,res0)} }
ensures[res] {EX (n:int), fpure(x,n) & fpure(n,res) }
@*)


