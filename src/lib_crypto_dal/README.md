# DAL Cryptographic Primitives
<!-- Summary line: One sentence about this component. -->

Implements KZG polynomial commitments and Reed-Solomon erasure coding for the
Tezos Data Availability Layer (DAL).

## API Documentation
<!-- Link to the external API. -->

The main public interface is [`cryptobox.mli`](cryptobox.mli). Generated API
documentation is available via `odoc` (package `octez-libs.crypto-dal`).

## Overview
<!--
- Describe the purpose of this component.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
-->

### Purpose

The Data Availability Layer (DAL) reduces the storage strain on the L1
blockchain by storing only constant-size cryptographic **commitments** on-chain.
The actual data blobs (**slots**) are stored off-chain and made available by DAL
nodes. This library provides the cryptographic primitives that make this
possible.

### Core idea

The construction links Reed-Solomon codes with KZG polynomial commitments:

1. A **slot** (raw bytes) is serialized into a vector of `k` scalar field
   elements and interpolated into a **polynomial** of degree less than `k`
   (see [Key parameters](#key-parameters) for `k` and other derived
   quantities).
2. The polynomial is **committed** using the KZG scheme, producing a
   constant-size commitment (one BLS12-381 G1 element).
3. The polynomial is evaluated at `n = redundancy_factor * k` points
   (Reed-Solomon encoding) and the resulting codeword is split into
   `number_of_shards` **shards** — each shard is a contiguous block of
   evaluations at a coset of the evaluation domain.
4. Constant-size **KZG proofs** allow any party to verify that a shard (or a
   page) is consistent with the on-chain commitment, without downloading the
   full slot.

Reed-Solomon is a maximum distance separable (MDS) code, meaning it achieves
the best possible trade-off between redundancy and recoverability: any
`number_of_shards / redundancy_factor` shards suffice to reconstruct the
original slot.

A **page** is a fixed-size sub-segment of a slot (`page_size` bytes). Pages
exist because slots are too large to be included in L1 operations. In
particular, smart rollups import DAL data at page granularity: during a
refutation game, the page together with its constant-size KZG proof serves as
evidence that the imported data is consistent with the on-chain commitment.

### Data flow

Notation: `𝔽_r` is the BLS12-381 scalar field (`r` is the order of the
curve's main subgroup), `𝔾_1` is the first elliptic curve group. `k` is the
maximum polynomial length (derived from `slot_size` and `page_size`),
`n = redundancy_factor * k` is the erasure-coded length, `s = number_of_shards`,
and `l = n / s` is the shard length.

```
RAW BYTES (slot_size bytes)
  │
  │  serialize: 31 bytes → 1 scalar; pad to k scalars
  ▼
DATA  ∈  𝔽_r^k
  │
  │  interpolate (domain of size k)
  ▼
POLY  p ∈ 𝔽_r[x],  deg p < k
  │              │
  │  commit      │  evaluate at n points (Reed-Solomon)
  ▼              ▼
  C ∈ 𝔾_1     CODEWORD  ∈  𝔽_r^n
                 │
                 │  partition into s cosets of size l = n/s
                 ▼
               SHARDS  (shard_i, proof_i)  i = 0, …, s−1
```

### Key parameters

The four protocol-level parameters:

| Parameter            | Meaning                                          |
|----------------------|--------------------------------------------------|
| `slot_size`          | Byte length of a slot                            |
| `page_size`          | Byte length of a page (sub-segment of a slot)    |
| `number_of_shards`   | Total number of shards (`s`)                     |
| `redundancy_factor`  | MDS redundancy; recovery needs `s / redundancy_factor` shards |

These are bundled in `Dal_config.parameters` and used to initialize a
`Cryptobox.t` value that carries both the raw parameters and derived quantities
(domain sizes, polynomial lengths, etc.).

### Prover vs. Verifier

The library separates the cryptographic operations into two roles because they
have very different resource requirements:

- **Prover**: can commit, produce shard proofs, page proofs, and commitment
  proofs. Initialization (`init_prover_dal`) loads the full Structured Reference
  String (SRS) from disk — a large file (~800 MB for mainnet parameters) that
  must be downloaded once and read at startup.
- **Verifier**: can verify commitments, pages, and shards. Initialization
  (`init_verifier_dal`) uses only a small, fixed subset of SRS points that are
  embedded directly in the binary (`zcash_srs.ml`), making startup lightweight
  and requiring no external files.

The verifier mode is the default. Processes that only need to check proofs (e.g.
the protocol validation logic) pay no SRS loading cost, while processes that
produce proofs opt into the heavier prover initialization.

### Dependencies

This library depends on:

- `octez-libs.kzg` — KZG commitment scheme implementation
- `octez-libs.bls12-381-polynomial` — polynomial arithmetic over BLS12-381
- `octez-libs.crypto-dal.dal-config` — DAL parameter types (sub-library in
  `dal_config/`)
- `octez-libs.base`, `octez-libs.stdlib`, `octez-libs.error-monad` — Octez base
  libraries

## Implementation Details
<!--
- Describe the file structure and the location of the main components.
- Other relevant implementation details (e.g., global invariants,
  implementation design rationale, etc.).
- Testing specifics, build-system specifics, etc. as needed.
-->

### File structure

| File                        | Role                                                       |
|-----------------------------|------------------------------------------------------------|
| `cryptobox.ml(i)`           | Core API: commit, prove/verify for pages and shards, slot ↔ polynomial conversion, precomputation management |
| `cryptobox_intf.ml`         | Module types `VERIFIER` and `COMMITMENT` shared by the prover and verifier interfaces |
| `dal_config/dal_config.ml(i)` | Sub-library (`octez-libs.crypto-dal.dal-config`): DAL parameter record and node configuration |
| `parameters_check.ml(i)`   | Pure validation of parameter sets (domain sizes, polynomial lengths, divisibility constraints) |
| `srs.ml(i)`                | SRS I/O (compressed/uncompressed formats), verifier G1/G2 subset extraction, SRS validity checks |
| `zcash_srs.ml`             | Embedded Zcash SRS material for the verifier (generated via `Srs.Internal_for_tests.print_verifier_srs_from_file`) |
| `trusted_setup.ml(i)`      | Download of SRS files from remote URLs with SHA-256 integrity checks |
| `errors.ml(i)`             | Error types for trusted-setup download (`Download_status`, `Mismatched_SHA`) |
| `event.ml`                 | Internal event-logging declarations (section `["dal"; "cryptobox"]`) |
| `trap.ml(i)`               | Trap share detection: determines whether a share is a "trap" for a given delegate, used in DAL attestation |

### Design notes

- **Global mutable state for SRS.** The library uses a global reference to hold
  the initialization parameters (SRS + mode). This is set once at startup by
  `init_prover_dal` or `init_verifier_dal` and determines which operations are
  available.

- **Amortized shard proofs.** Generating all `s` shard proofs is done in
  `O(n log n)` time using a precomputation step (see
  [Fast amortized KZG proofs](https://eprint.iacr.org/2023/033.pdf)). The
  precomputation can be saved to and loaded from disk
  (`save_precompute_shards_proofs` / `load_precompute_shards_proofs`), with
  optional Blake2B integrity checking.

- **Page-interleaved serialization.** The byte-to-scalar serialization
  (`polynomial_from_slot`) uses a page-interleaved layout: scalar elements are
  arranged so that each page's data is contiguous when the polynomial is
  evaluated. This layout is injective (see `cryptobox.mli` documentation).

### Testing

Tests are in `test/`:

- `test_dal_cryptobox.ml` — Tezt/QCheck tests for the core `Cryptobox` API
  (commit/prove/verify round-trips, encoding, error cases).
- `test_trap.ml` — tests for `Trap.share_is_trap`, including statistical
  validation of the trap fraction.

Tests depend on small SRS fixtures (`srs_zcash_g1_5`, `srs_zcash_g2_5`).

Run with:

```
dune build @src/lib_crypto_dal/test/runtest
```
