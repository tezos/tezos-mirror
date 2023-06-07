# tezos-snoop
Summary line: Benchmark and inference tool.

## Overview
- The purpose of `tezos-snoop` is to provide a CLI to the library
  `tezos-benchmark` and to the various benchmarks defined througout
  the code. It also provides means to display benchmark and inference
  results and to generate reports.

## Implementation Details
- The entrypoint is in the file `main_snoop.ml`.
- The modules `Cmdline` and `Commands` contain respectively type
  definitions and `tezos-clic` command definitions.
- The module `Display` allows to construct plots.
- The module `Report` allows to generate reports in the `latex` language.
- The `latex` sub-library is a think abstraction over latex documents.
