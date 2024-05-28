#!/bin/sh
now=$(date +"%Y-%m-%d_%Hh%M")
OUT="benchmark_WASM-$now.out"
echo "output file: $OUT"
dune exec src/lib_scoru_wasm/bench/benchmark_scoru_wasm.exe --profile=release > "$OUT"
