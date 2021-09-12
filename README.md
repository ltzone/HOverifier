# An automated verifier for Programs that use Higher-order Functions


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


## Acknowledgements

This repository is the implementation of a one-semester personal research project at National University of Singapore supervised by Prof. Chin Wei Ngan, with lots of help from Yahui Song.

The project is inspired by the HIP/SLEEK verifier.