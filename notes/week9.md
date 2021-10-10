

## Data Structure

Expressions include constants, variables and logical function applications.

```OCaml
type logical_var = string

type program_var = string

type logical_fun = string

type constant = Int of int

type bin_operator = Plus | Minus | Mult

type logical_exp = Pvar of program_var 
                 | Lvar of logical_var 
                 | Fun of logical_fun * (logical_exp list)
                 | Const of constant
                 | Op of bin_operator * logical_exp * logical_exp
```

Pure predicates include binary operators on expressions and logical connectives

```OCaml
type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
               | And of pure_pred * pure_pred
               | Or of pure_pred * pure_pred
               | Neg of pure_pred
               | True | False
```

Each assertion have two parts, the pure assertion in disjunctive normal form and some specifications that describe the function values at the current program point.


```OCaml
type pred_normal_form = {
  pure: (logical_var list * pure_pred) list; (* list of disjunctive clauses *)
  spec: fun_signature list;
} 
and fun_spec = {
  fpre: pred_normal_form;
  fpost: program_var * pred_normal_form;
} 
and fun_signature = {
  fname: program_var;
  fvar: program_var list;
  fspec: fun_spec;  
}
```

Example:
```OCaml
let twice f x = f (f x)
(* Requires    { f(a) |= {true} *->res0:{res0=fpure(a)} }
   Ensures  [r]{ r=fpure(fpure(x)) }
*)
```

```
let twice_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="res0", {pure=[([], Arith (Eq, Pvar "res0", Fun ("fpure", [Pvar "a"])))]; spec=[]}
      };  
    }
  ]};
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Fun ("fpure", [ Fun ("fpure", [Pvar "x"]) ] ))))]; spec=[]};
}

let twice_sig = {
  fname="twice";
  fvar=["f";"x"];
  fspec=twice_spec
}
```

## Forward Verification


### Partial application
```OCaml
(* (+) arg1 arg2
   Requires      { true }
   Ensures[res]  { res = arg1 + arg2 } *)

let quad_fo x = (((+) x) (x) + ((+) x x))
(* Requires      { true }
   Ensures[res]  { res = x + x + x + x } *)
```

- `(+) x x` produces
    ```
    _r11: _r11=(x+x)) ))
    ```
- `(+) x` produces
    ```
    _r12: _r11 = x + x  with 
            _r12(arg2) |= { true } *->:plus_res { plus_res=(x+arg2) }
    ```
- `_r12 x` produces
    ```
    _r14: _r11 = x + x  /\ _r14 = x + x
    ```
- `_r14 + _r11` produces
  `_r16: ( _r16=(_r14+_r11)  /\ (( _r14=(x+x)  /\  _r11=(x+x) ))))`
  => ` _r16=((x+x)+(x+x)) `


### Call function with pre-condition, and as return values
```OCaml
let div y x = 
      (x / y)
(* Requires    { y != 0 }
   Ensures  [r]{ r * y = x }
*)


let div_by_one = (div 1)
(* Requires    { true }
   Ensures  [f]{ f(x) |= { true } *->:m { m * 1 = x } }
*)
```

- `1` produces `_r3 : ( _r3=1 )`
- `div _r3` produces:
  ```
  _r4 : ( _r3=1  ) with 
        _r4(x) |= {~ _r3=0 } *->:r { x=(r*_r3) }
  ```
- checking post condition:
  1. pure part: `_r3 = 1 => true`
  2. returned function  `_r4(x)` should be a subsumption of specified `f(x)`
     - `(_r3 = 1) /\ true => ~ _r3 = 0`
     - `(_r3 = 1) /\ x = (r*_r3) => r * 1 = x`


### Issue: when the verification needs to instantiate a logical function constant

```OCaml
let incr x = x + 1
(* Requires    true
   Ensures[r]  r = x + 1
*)

let once f x = f x
(* Requires { f(a) |= { true } *->:r { r=fpure(a) } }
   Ensures[p] { p = fpure(x) }
*)

let incr_once x = once incr x
(* Requires    true
   Ensures[r]  r = x + 1
*)
```


```
Verifying incr_once
hip: once :  true 
hip: x :  true 
hip: incr :  true 
sleek: checking for pre and post
[pre]  true 
[post]  true 
sleek: checking for pre and post
[pre]  _r24=fpure(x) 
[post]  _r24=(x+1) 
Entailment fail
Verify incr_once fail
```


## Proposed Solution

Let users declare the instantiation explicitly

```OCaml
let once f x = f x
(* Given (fpure: Int -> Int)
   Requires { f(a) |= { true } *->:r { r=fpure(a) } }
   Ensures[p] { p = fpure(x) }
*)

let twice f x = f (f x)
(* Given (fpure: Int -> Int)
   Requires { f(a) |= { true } *->:res0 { res0=fpure(a) } }
   Ensures[res] { res = fpure(fpure(x)) }
*)

let incr_once x = once incr x
(* Requires    true /\ once_spec[fpure=\x.x+1]
   Ensures[r]  r = x + 1
*)


let incr_twice x = twice incr x
(* Requires    true /\ twice_spec[fpure=\x.x+1]
   Ensures[r]  r = x + 2
*)

let quad x = (twice double) x
(* Requires      { true /\ twice_spec[fpure=\x.x+x] }
   Ensures[res]  { res = x + x + x + x } *)

```