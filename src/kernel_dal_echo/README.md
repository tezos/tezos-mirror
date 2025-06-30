# DAL Echo Kernel

A simple kernel for testing the DAL. At each level, it will download all pages published at `current_level - attestation_lag` at slot `0`, then store the downloaded content to `/output/slot-0` of the durable storage.

## How to Build

``` sh
make build
```

## Bandwidth

`kernel_bandwidth` provides an alternative version of the DAL echo kernel that
reads slots but only writes the number of bytes read per slot instead of the
content. Slots to be read are set through a configuration file of the form:

```
instructions:
  - set:
      value: <bitvector>
      to: /slots
```

where the bit vector is an hexadecimal encoding of an integer in little-endian,
where each of its bits represent a DAL slot.
