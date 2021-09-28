## Implementing with Z3

The trick of checking `forall x, pre |- post`

encode as `exists x, pre /\ ~ post`, 
- if SAT returns true, then deriviation fails at the found x as a counterexample
- if SAT returns false, then entailment is checked
