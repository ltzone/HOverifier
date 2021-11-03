
let twice f x = (f x)
(*@
declare twice(f,x)
given fpure(int,int)
requires { true with
    f(a) |= { true } *->:res0 {fpure(a,res0)} }
ensures[res] {EX (n:int), fpure(x,res) }
@*)