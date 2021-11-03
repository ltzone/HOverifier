- GIVEN/ NOT GIVEN predicates
  - If not given, then fix the assignment!
- add types to predicates, Q(bool, int->int)...
- sanity check for type matching in variable substitution




## TODO list

- [x] what about a nested function specification?
> issue, function as return value, how should the specification be layed out? { true } *-> {true with f(x)=... } ? or just f(x) = ...
  - [x] example of div_by_one
  > addressed! we only need to further evaluate identifiers where possible
- [x] subsumption check
- [x] general specification
  - first implemented as user declared, then find ways to automate it
  > prefer with explicit instantiation, because it fits the design of user-defined predicate
  - implemented by adding an extra constraint to the solver
- [x] Assertion parser
- [x] if structure
  > currently implemented as a coercion to integer type
  - add bool type to the assertion language
- ADT, match structure
- refine the disjunctive normal form reasoning
- predicate system to support recursive reasoning
- introduce separation logic

- discriminate between pvar and lvar in substitution
- alpha renaming everywhere (e.g. div.ml)

## Discussion

- declare fpure explicitly, and add fpure to a list, but only allows fpure to be collected on toplevel of every specification
  > is there any possibility for a local fpure?




- example for multiple instantiations

- known issue: clashing of return anchor names