
## Prototype System

- OCaml frontend
- Forward Verifier `hip.ml`
  
![](./img/09-30-11-18-17.png)

- AST for the assertion language `spectree.ml`

![](./img/09-30-10-42-28.png)

- Entailment checker `sleek.ml`



## Implementing the entailment checker with Z3

The trick of checking `forall [a....] [f], pre |- exists [b...] post`

The build-in quantifier of z3 only supports quantifying over basic sorts (`[a...]`, `[b...]`), therefore pure functions in assertions can only be encoded as top-level uninterpreted functions.

encode as `exists f, pre |-\- post`, or equivalently `exists f, pre /\ ~ post`, 
- if SAT returns true, then deriviation fails at the found x as a counterexample
- if SAT returns false, then entailment is checked
