
let foo x y = x - y
(*@
declare foo(x,y)
requires { true }
ensures[res] { res = x - y }
@*)


let bar f a b = f (f a b) b


(*@
declare bar(f,a,b)
given 
requires { 0 <= b with
    f(m,n) |= { 0 <= n } *->:res0 { res0 <= m }
 }
ensures[p] { p <= a }
@*)


let baz c d = (bar foo c d)


(*@
declare baz(c,d)
given 
requires { 0 <= d }
ensures[r] { r <= c }
@*)