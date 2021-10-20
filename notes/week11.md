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

- Is it sound that we treat the multiple possible instantiations as disjunction


- We need to remove f from below, which requires that the function specification constraints should be independent from other arguments
```
 _r27 :  true  with 
        _r27(x)[fpure_twice]|= { true  with 
        f(a)[]|= { true } *->:res0 {fpure_twice(a,res0)}} *->:res {(fpure_twice(x,n) /\ fpure_twice(n,res))}
```


## Resolve partial application

```OCaml
let quad x = twice double x
(* verified, check `double <: twice.f`
*)



let quad x = (twice double) x
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *)
```


- Step 1
  - eval `(twice double):r`, we get the new assertion by adding an extra function specification available for the return value `r`
  - In the original specification of twice, the first argument will be replaced with the [double]
   ```
   true  & 
   r(x)|= { true  & double(a)[]|= { true } *->:res0 {fpure_twice(a,res0)}} 
       *->:res {(fpure_twice(x,n) /\ fpure_twice(n,res))}
   ```

- Step 2
  - The forward verifier will take out the new specification of [r] and put into the context

- Step 3 
  - Next we evaluate [r x] (which is a full application)
  - To validate the precondition of specification [r(x)]
  - We need to validate `double(a)[]|= { true } *->:res0 {fpure_twice(a,res0)}}`

  - Recall that previously in a full application, (e.g. `let baz = bar foo c d`)
    - `foo` will correspond to the first argument in `bar`, therefore we only need to verify `foo <: bar.f`

  - but here, when we try to validate `double(a)[]|= { true } *->:res0 {fpure_twice(a,res0)}}`,
  - we can't search for a corresponding function in the argument list
  - in fact, what we really want to verify is that
    `double (defined in an other place in the program) <: {true} *->:res0 {fpure_twice(a,res0)}`

- The two styles are essentially generating the same VC, the only difference is that
  - for full application, we check the subsumption when do the application
  - for partial application, we first do the substitution and postpone the proof obligation for subsumption later until it is fully applied (as this will be noted by absence of function name in the argument list and the verifier will turn to the global specification environment to look for its actual specification)
