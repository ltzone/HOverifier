# An automated verifier for (Pure) Programs with Higher-order Functions




## Setup

The verifier is built upon the frontend of OCaml 4.12.0. all the codes in `parsing` and `utils` are almost the same as the official codebase.

For this project, you need to first provide a `utils/config.ml` file. A recommended way to generate it is to download a clone of OCaml 4.12.0 and run `./configure ; cd utils ; make config.ml` and copy the generated `config.ml` to this repository.

To build up, you need to have `opam`

```
opam switch create 4.12.0
eval $(opam env)
opam install dune z3 menhir core
```

To use z3 on MacOS, copy the dynamic library to the build directory
```
cp `opam config var z3:lib`/libz3.dylib .
cp libz3.dylib _build/default
```

A few testcases have been provided in `testcases/`, you can `sh regression_test.sh` to run them.

Alternatively, you may write your own assertion-annotated `xxx.ml` file and put it in the `testcases/` and run `dune exec main.exe xxx`

### Install HIP/SLEEK

> In fact, no advanced use cases have been implemented, the project currently does now depend on HIP/SLEEK, so you may skip this part
- advanced use cases in this project requires the SLEEK solver
- `git clone https://github.com/hipsleek/hipsleek.git`
- `git checkout extsess`
- `opam switch create ocaml-base-compiler.4.10.2`
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
- `./sleek examples/working/sleek/sleek2.slk`
  

## Acknowledgements

This repository is the implementation of CP3106, a one-semester personal project at SoC NUS supervised by Prof. Chin Wei Ngan, with lots of help from Yahui Song.

The project is inspired by the HIP/SLEEK verifier.