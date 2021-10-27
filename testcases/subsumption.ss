declare foo(x,y)
requires { true }
ensures[res] { res = x - y }


declare bar(f,a,b)
given 
requires { 0 <= b with
    f(m,n) |= { 0 <= n } *->:res0 { res0 <= m }
 }
ensures[p] { p <= a }


declare baz(c,d)
given 
requires { 0 <= d }
ensures[r] { r <= c }