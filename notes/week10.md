## Check function subsumption

We have a higher order function

```
let apply (f:F1) x = f x
(*  Requires true & f |= F1
    Ensures  ....
*)
```

and a caller

```
let g a b = b / a
(* g |= F2 *)

let foo x = apply g x
```

We need to check `F_g <: F_f` or `F2 <: F1`

Example:
```
client: f(a,b) |= { a != 0 } *->r: {r * a = b}
                  P1               Q1

server: f(a,b) |= { a > 0  } *->r: {(r * a = b) & (b > 0)}
                    P2                          Q2



-------------------------------


 { a > 0 } *->r: {(r * a = b)}
            <:
{ a != 0 } *->r: {r * a = b}
----------------------
F2 <: F1

```




Verification Condition for `F2 <: F1` under assumption `P`
`P /\ P2 -> (P1 /\ ( Q1 -> Q2 ))`

With quantifiers:
`forall a b, forall e f, P /\ P2 -> exists c d, (P1 /\ (forall r, Q1 -> Q2))`
- `a, b` are function's arguments, shared by `F1` and `F2`
- `e, f` are universal quantifiers in `F1`
- `c, d` are universal quantifiers in `F2`, for this reason, `fpure` for `F2` can't appear in the VC and should be instantiated in advance

## General predicates and automating solution

Allow user to provide some predicates in the comment.

```
let double x = x + x
(* Requires      { true }
   Ensures[res]  { res = x + x } *)
(* doubleP(x,r):= r = 2*x  *)

let twice f x = f (f x)
(* Requires { f(a) |= { true } *->:res0 { fpure(a,res0) } }
   Ensures[res] { fpure(x,r) & fpure(r,res) }
*)


let quad x = (twice double x)
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *)
```

When finding an instantiation of `fpure` in proving subsumption for `double <: f`,

First try to verify at an abstract level, to see if `fpure` can be preserved in post condition without instantiating

If fails, try to find user-provided predicates that have the same type signature as `fpure`, instantiate `fpure` and try to solve again

Otherwise, report failure.



Plan:
- extend predicates to support recursive structures
```
factP(x,r) = x = 0 & r = 1
          or factP(x-1,m) & r = m * x

LL_foldl[fpure: Int -> Int -> Int -> Bool]<x,r> == 
    self::Nil<> & x=r
or  self::Cons<y,ys'> * ys'::LL_foldl[fpure]<z,r> & fpure(x,y,z)
```





## TODO

1. write a example of subsumption


```OCaml
let foo x y = x - y
(* true *-> res = x - y *)


let bar f a b = f (f a b) b
(* Requires 
      f(m,n) |= { m > n } *->:res { res > 0}
      a > 2 * b
      
   Ensures[r]
      r > 0

*)
```