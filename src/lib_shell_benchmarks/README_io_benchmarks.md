# New IO benchmarks

We have 2 new IO benchmarks `io/READ` and `io/WRITE` to get more realistic
gas cost parameters for the storage accesses than the older benchmarks.

## How to run

### System requirements

#### Linux

The new benchmarks are Linux dependent.  They use `/proc` filesystem to
clear the OS's disk cache and retrieve the memory usage information.

#### Root priviledge

You need root access of the machine to clear the OS cache.

### purge_disk_cache.exe

Source code: `devtools/benchmarks-tools/purge_disk_cache/`

You must compile `purge_disk_cache.exe` then copy it to the root of
Tezos source code with root setuid flag:

```
% dune build devtools/benchmarks-tools/purge_disk_cache/
% cp _build/default/devtools/benchmarks-tools/purge_disk_cache/purge_disk_cache.exe .
% sudo chown root ./purge_disk_cache.exe
% sudo chmod u+s ./purge_disk_cache.exe
```

### Tezos mainnet context

You need a Tezos data directory somewhere (in this section we will use `_snoop/tezos-node`):

```
% ls _snoop/tezos_node
context     identity.json  peers.json  version.json
daily_logs  lock           store
```

For precise benchmarking, the directory should be copied from a running
Tezos mainnet node. It is **not** recommended to use a data directory
freshly imported from a snapshot:

```
# OK: the data directory should experience several context GCs
% du -sh _snoop/tezos-node
55G	_snoop/tezos-node
```

```
# NG: do not simply import a snapshot
% ./octez-node snapshot import mainnet-xxxxxxx.rolling --data-dir _snoop/tezos-node
% du -sh _snoop/tezos-node
7G	_snoop/tezos-node
```

The directory `_snoop/tezos-node` should be on an SSD disk without RAID.  The disk should have enough free space.

### Config file

By default, the data and the cache folders are expected to be found in `_snoop`.
You can redefine these locations with a different configuration. Here is an example of
a minimal template for a config file for this purpose:

```
{ "namespace": ".", "config": null,
  "children":
  [ { "namespace": "io", "config":
      { "tezos_data_dir": "/your/data/here/",
        "cache_dir": "/your/cache/here/" },
      "children": [] } ] }
```

### Benchmark execution

```
% rm -rf _io_bench/
% ./octez-snoop generate code for benchmarks io/READ io/WRITE --out-dir _io_bench/ --config-file config.json
```
