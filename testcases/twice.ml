
let twice f x = f (f x)
(*@
declare twice(f,x)
given fpure(int,int)
requires { true with
    f(a) |= { true } *->:res0 {fpure(a,res0)} }
ensures[res] {EX (n:int), fpure(x,n) & fpure(n,res) }
@*)