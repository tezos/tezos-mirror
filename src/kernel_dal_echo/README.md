# DAL Echo Kernel

A simple kernel for testing the DAL. At each level, it will download all pages published at `current_level - attestation_lag` at slot `0`, then store the downloaded content to `/output/slot-0` of the durable storage.

## How to Build

``` sh
make build
```
