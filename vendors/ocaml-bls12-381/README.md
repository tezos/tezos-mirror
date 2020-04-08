# OCaml implementation of BLS12-381


## Install

1. Setup environment
```
opam switch create ./ 4.09.1 --deps-only
eval $(opam env)
```

2. Install Rust dependencies and install the library


```
./build_deps.sh
opam install . -y
```

3. Play with utop

```
opam install utop
dune utop
```

## Run tests

```
dune build @install
opam install alcotest
dune runtest
```

## Run the benchmarks

**USE THE RELEASE VERSION OF THE RUST LIBRARY TO GET PRODUCTION BENCHMARKS (cargo build --release)**

```
opam install core_bench
dune exec benchmark/bench_ec.exe
dune exec benchmark/bench_ff.exe
dune exec benchmark/bench_pairing.exe
```

## Documentation

- Use `dune build @doc` to generate the API documentation.
