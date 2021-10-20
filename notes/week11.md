## Subsumption testcase

```OCaml
let foo x y = x - y
(* true *-> res = x - y *)


let bar f a b = f (f a b) b
(* Requires 
      f(m,n) |= { n >= 0 } *->:res { res <= m}
      b >= 0
      
   Ensures[r]
      r <= a

*)

let baz c d = bar foo c d
(* Requires 
      d >= 0
      
   Ensures[r]
      r <= c
*)
```

When verifying `baz`

```
[context]  0<=d 
[client] foo(x,y)[]|= { true } *->:res { res=(x-y) }
[server] f(m,n)[]|= { 0<=n } *->:res0 { res0<=m }

[server'] f(x,y)[]|= { 0<=y } *->:res { res<=x }

Need to prove `foo <: f`
SMT encoding:
0<=d /\ 0 <=y  ===>
  true /\
  (forall x, res,  res <= x ==> res = x - y )
```


## If branching



## Issues

- Will we use the same abstract predicate to characterize two function parameters?
  `let foo f g x = ... { Requires f |= fpure,... g |= fpure }`
  [has implemented, to be tested]