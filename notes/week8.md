## Implementing with Z3

The trick of checking `forall f, pre |- post`

The build-in quantifier of z3 only supports quantifying over basic sorts, therefore pure functions in assertions can only be encoded as top-level uninterpreted functions.

encode as `exists f, pre |-\- post`, or equivalently `exists f, pre /\ ~ post`, 
- if SAT returns true, then deriviation fails at the found x as a counterexample
- if SAT returns false, then entailment is checked
