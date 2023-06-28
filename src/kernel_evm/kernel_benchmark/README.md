# Benchmarking EVM Kernel

## Context

The `evm_execution` crate provides functions to execute ethereum transactions in
a SORU kernel.
During development, tests were done to assess the efficiency and safety wrt the
number of ticks necessary.

## Methodology

Provide a kernel and benchmark scenarios usable with the debugger.
Obtain benchmark data from debugger output.

The benchmark kernel is used to test the `evm_execution` crate but adding just
the minimum layer to create a functionnal kernel.

- a faucet
- each message from the inbox is expected to be an rlp encoded transaction
- output execution information in debug messages (eg gas)

The benchmark scenarios are provided as scripts that generate the input data.
See `./benchmarks/`.

## How to use

- compile the benchmark kernel

```
make -C ../src/kernel_evm/
cp ../src/kernel_evm/target/wasm32-unknown-unknown/release/kernel_benchmark.wasm .
wasm-strip kernel_benchmark.wasm
```

- compile the debugger

```
make
```

- generate inputs for a given scenario

```
node src/kernel_evm/kernel_benchmark/scripts/benchmarks/bench_transfers_1.js > inbox_bench_transfers_1.json
```

- execute the debugger on the kernel and the inputs

```
$ ./octez-smart-rollup-wasm-debugger --kernel kernel_benchmark.wasm --inputs inbox_bench_transfers_1.json
> load inputs
Loaded 11 inputs at level 0
> bench
Ran for 15 kernel_run calls:
46962960 ticks in 15.270356 seconds
...
```
