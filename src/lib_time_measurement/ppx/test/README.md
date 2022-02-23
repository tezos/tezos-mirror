
# Testing PPX preprocessing

This folder is used to unit test code transformation resulting
from preproccessing Ocaml code with `Tezos_time_measurement_ppx`.

## Valid and invalid programs

Tests are separated into two different folders:
* `test/valid` that contains tests of the preprocessing of programs
   that are semantically valid in regard to the `@time` attribute.
* `test/invalid` that contains tests of the preprocessing of programs
   that are syntactically correct, but where the `@time` attribute
   is misused.

## Inputs and Outputs

Each test consists of two files sharing the same `prefix`:
* An *input file* (named `<prefix>input.ml`). This file will contain
  a semantically valid Ocaml program that will be given as input to
  preprocessing.
* An *output file* (named `<prefix>output.ml` in `test/valid` or 
  `<prefix>output` in `test/invalid`. This last one will contain
  the output that should be expected when processing the matching
  input file. For valid programs, this means the program resulting
  from preprocessing. For invalid one, output files will contain
  the standard error output of the preprocessing.
  
In the rest of this documentation, the term `i/o` will refer to a
couple of *input file* and *output file*.

## Running the tests

Tests can be easily runned using the following command:
```
dune build @src/lib_time_measurement_ppx/runtest
```
For each `i/o`, This will trigger a dune `rule` that will preprocess
the *input file* using `Tezos_event_ppx` and compare the resulting
program with the *output file*.
  
## Dune stanzas generation

Each `i/o` needs some dune stanzas to exist in the `dune` file to be
able to perform the test. Those stanzas can be generated inside
`dune.inc` file which is inclued inside the `dune` file.

If a new `i/o` is added into `test/valid` or `test/invalid` directories,
then running the test with `dune build @src/lib_event_ppx/runtest`
will fail because it detected that `dune.inc` file should be updated.
This can be done using the following command:

```
dune build @src/lib_time_measurement_ppx/runtest --auto-promote
```

