# An automated verifier for Programs that use Higher-order Functions

## TODO list

- subsumption check
  - what about a nested function specification?
    - example of div_by_one
- general specification
  - first implemented as user declared, then find ways to automate it
  > prefer with explicit instantiation, because it fits the design of user-defined predicate
- if structure
- ADT, match structure
- refine the disjunctive normal form reasoning
- predicate system to support recursive reasoning
- introduce separation logic
- Assertion parser


## Discussion

- declare fpure explicitly, and add fpure to a list, but only allows fpure to be collected on toplevel of every specification
  > is there any possibility for a local fpure?


## Setup

The verifier is built upon the frontend of OCaml 4.12.0. all the codes in `parsing` and `utils` except `parser.mly` are almost the same as the official codebase.

For this project, you need to first provide a `utils/config.ml` file. A recommended way to generate it is to download a clone of OCaml 4.12.0 and run `./configure ; cd utils ; make config.ml` and copy the generated `config.ml` to this repository.

To build up, you need to have `opam`

```
opam switch create 4.12.0
eval $(opam env)
opam install dune z3 menhir 
dune exec ./main.exe testcases/t1_fold.ml
```

To use z3 on MacOS, copy the dynamic library to the build directory
```
cp `opam config var z3:lib`/libz3.dylib .
cp libz3.dylib _build/default
```

## Acknowledgements

This repository is the implementation of a one-semester personal research project at National University of Singapore supervised by Prof. Chin Wei Ngan, with lots of help from Yahui Song.

The project is inspired by the HIP/SLEEK verifier.