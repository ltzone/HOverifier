# An automated verifier for Programs that use Higher-order Functions

## TODO list


- what about a nested function specification?
> issue, function as return value, how should the specification be layed out? { true } *-> {true with f(x)=... } ? or just f(x) = ...
  - example of div_by_one
- [x] subsumption check
- [x] general specification
  - first implemented as user declared, then find ways to automate it
  > prefer with explicit instantiation, because it fits the design of user-defined predicate
  - implemented by adding an extra constraint to the solver
- if structure
  - add bool type to the assertion language
- ADT, match structure
- refine the disjunctive normal form reasoning
- predicate system to support recursive reasoning
- introduce separation logic
- Assertion parser

- discriminate between pvar and lvar in substitution
- alpha renaming everywhere (e.g. div.ml)

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


### Install HIP/SLEEK

- advanced use cases in this project requires the SLEEK solver
- you can build sleek as follows:
  ```
  opam install cppo Num OCamlgraph FileUtils Batteries Camlp4 extlib
  ```
- for mac user
  ```
  cd omega_modified
  make depend
  cd omega_calc/obj
  make
  sudo cp oc /usr/local/bin
  ```
- `cd xml; make clean; make`
- `make`
  

## Acknowledgements

This repository is the implementation of a one-semester personal research project at National University of Singapore supervised by Prof. Chin Wei Ngan, with lots of help from Yahui Song.

The project is inspired by the HIP/SLEEK verifier.