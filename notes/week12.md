## Parser
- Parser for specification
- explicit type annotation
- user needs to provide instantiation candidates
```Mermaid
graph TD;
source.ml --> ocaml(OCaml Parser)
source.ml --> spec(Specification Parser)
ocaml --> prog(Program AST)
spec --> env(Specification Environment);


prog -- traverse --- HIP
env -- look up --- HIP
SLEEK -- proof obligation --- HIP 
```



## Non-deterministic forward verifier

- Restructure the forward verifier, return a list of possible predicates
```OCaml
let rec infer_expression : 
    (env:env) 
    (acc:pred_normal_form) 
    (expr:Parsetree.expression) 
    : (env * logical_var * pred_normal_form) list
```
  - non-determinism comes from
    - instantiation of predicates
      1. try not instantiate
      2. try instantiate from the candidates
      3. report failure

## Recursive predicates

- sleek can't work well

- entail recursive predicates:
  - try unfold the right


## ECOOP 2021 paper

- **implicit** ghost variables