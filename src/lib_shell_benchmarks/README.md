# `Tezos_shell_benchmarks`

This library contains protocol-independent benchmarks of various kinds.
These are built on top of the `tezos-benchmark` library and are made
available to `tezos-snoop`. The following broad classes of benchmarks
are defined here:

- Miscellanous benchmarks
- IO benchmarks
- Encoding benchmarks

## Miscellanous benchmarks

The module `Misc_benchmarks` contains a benchmark for the function
`Lwt_main.run`. This is used in the protocol for fine-grained estimation of the
execution time of Michelson instructions.

This benchmark depends on `Builtin_benchmarks.Timer_latency_bench`: we need to
subtract the latency induced by the timer in order to estimate the execution
time of `Lwt_main.run`.

## IO benchmarks

The module `Io_benchmarks` contains various benchmarks assessing the
performances of the storage layer. The storage layer is considered as a
key-value store with keys corresponding to lists of strings ("paths") and
values being `bytes` of various sizes.

The following benchmarks are defined:

- `Context_size_dependent_read_bench`
- `Context_size_dependent_write_bench`
- `Irmin_pack_read_bench`
- `Irmin_pack_write_bench`
- `Read_random_key_bench`
- `Write_random_keys_bench`

It is assumed throughout that read and write performance can possibly depend of
the following parameters:
- number of keys in the context
- length of the key being accessed
- size of the value associated to the key being accessed

More work is needed on these benchmarks to make them more realistic, preferably
by people having some experience of the storage layer.

### `Context_size_dependent_read_bench`

This benchmark prepares a synthetic context with a random number of random keys
and values of random sizes.
After preparing the context, the cache is cleared by commiting and
reloading.
It then measures the time taken to access a random key in the
domain of the key-value store (`Tezos_protocol_environment.Context.get`).

### `Context_size_dependent_write_bench`

This benchmark prepares a synthetic context as the previous one and
benchmarks the time to commit a write of a random, non-existing key
with random bytes of random length.

### `Irmin_pack_read_bench`

This benchmark prepares a random context containing a directory
with > 256 children, triggering the 'pack' behaviour of Irmin-pack.
The packed directory is placed at a random depth in the tree.
After preparing the context, the cache is cleared by commiting and
reloading.
This benchmark then measures the access time to a random key in this directory.

### `Irmin_pack_write_bench`

This benchmarks first prepares a random context, then prepares a
random irmin-pack directory as above.
After preparing the context, the cache is cleared by commiting and
reloading.
This benchmark then measures the commit time of a bunch of writes
to _fresh_ keys in the previously generated irmin-pack directory.
There's a hard-coded option to allow overwriting existing keys,
not exposed to the configuration parameters (parameter `~bench_init`).

### `Read_random_key_bench`

This benchmarks proceeds on an _existing_ context (eg a mainnet one).
The context is copied to a temporary directory.
The benchmark then measures the read access time to a random key in a
prescribed directory.

### `Write_random_key_bench`

This benchmarks proceeds on an _existing_ context (eg a mainnet one). The
context is copied to a temporary directory. The benchmark then measures the
commit time of a bunch of writes to a subset of keys of a specified
subdirectory of the context (eg '/delegates/important/stuff', etc).

## Encoding benchmarks

The module `Encoding_benchmarks` contains simple benchmarks for
encoding/decoding cryptographic values (keys, key hashes, etc) in both
optimized and readable format.

## Implementation details

### `Io_stats`

The `Io_stats` module provides functions to compute statistics on
a context, as well as ways to plot these statistics.

### `Io_helpers`

The `Io_helpers` module contains facilities for the following:
- loading and saving contexts
- committing to, saving, closing and reloading a context (useful to clear the
  Irmin cache)
- copying directories

### `Encoding_benchmarks_helpers`

The `Encoding_benchmarks_helpers` module contains models and benchmark
templates that are useful when defining encoding benchmarks.

## API documentation

See the [documentation on defining benchmarks with snoop](https://tezos.gitlab.io/developer/snoop_arch.html#defining-benchmarks-the-generator-module).
