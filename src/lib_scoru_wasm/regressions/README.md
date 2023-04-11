# WASM PVM Regressions

This directory contains a set of regression tests which are intented to assert
our modifications of the WASM PVM are backward compatible.

The tests are exported in a library consumed by the Tezos Tezt-suite.

You can run the tests with the following command:

```bash
dune exec -- tezt/tests/main.exe --file tezos_scoru_wasm_regressions.ml
```

Tags of interests are:

- `proof`, to only run the tests asserting proofs sizes do not change
- `hash`, to only run the tests asserting the intermediary hashes do not change
- `vN`, to only run the tests targeting a given version of the WASM PVM
  (*e.g.*, `v0`).

Finally, the regression traces for the `proof` tests are valid CSV files which
can be visualized using tools like `gnuplot`.

```bash
gnuplot -e "plot '$out'" -p
```

where `$out` is the path to a regression trace.
