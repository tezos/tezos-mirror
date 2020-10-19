# tezos-benchmark
Summary line: Benchmark and inference library.

## Overview
- `tezos-benchmark` is a library to describe benchmarks, collect results and
  fit predictive models. It also allows to generate code from the fitted
  models.
- See the `.rst` documentation for a more detailed description.

## Implementation Details
- Testing: the `test` subdirectory contains unit tests. Most of these
  unit tests are currently disconnected, because the CI is not able to
  run them.
- The subdirectory `example` contains a full example of
  a benchmark for the blake2 hash function.
