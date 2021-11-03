## Support for more types of variables


## "Auxiliary variables" or ghost variables

```OCaml
let foo f= 
  if f 2 = f 4 then 1 else 0
(*@
declare foo(f)
requires { EX (n:int), 0 <= n with
    f(b) |= { 0 <= n } *->:r { n = r } }
ensures[res] { res = 1 }
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

```

When we check `test`, we need to prove `g <: foo.f`
```
g(b) |= {true} *->:r { r=5 }
Exists n, f(b) |= {true} *->:r { n=r }
```