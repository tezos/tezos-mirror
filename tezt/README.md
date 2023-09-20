# Test suite for the Tezos blockchain using the Tezt framework

This directory contains a test suite for Octez (an implementation of the Tezos blockchain) based on Tezt. Tezt is a
generic test framework that is used for writing tests in OCaml.

The directory includes the sources of the Tezt framework, its extension with specific support for Tezos (called Tezt-Tezos), and a whole test suite.

## API Documentation

The user manual of the testsuite is available online at <https://tezos.gitlab.io/developer/tezt.html>.

The APIs of the `tezt` and `tezt-tezos` libraries are documented in the
interface files (suffix `mli`). Both APIs are also available online at
<https://tezos.gitlab.io/api/api-inline.html>.

## Overview

This directory contains the following subdirectories:

- `lib` defines an OCaml library named `tezt` which contains the test
  framework. It is independent from Octez and from the Tezos protocols.

- `lib_tezos` defines another OCaml library named `tezt-tezos` which
  is specific to Octez, complementing the above with Octez-specific
  features.

- `lib_performance_regression` defines another OCaml library named
  `tezt-performance-regression` which implements the so-called
  Long Test Framework (LTF). It provides ways to register tests that are
  to be run on dedicated "executors" with stable performances.
  It features InfluxDB and Grafana integration for measures,
  and Slack integration for alerts.

- `tests` defines an OCaml executable which gathers automatic tests
  written for Tezos.

- `manual_tests` defines an OCaml executable which gathers manual
  tests written for Tezos. A manual test is a test which requires
  extra configuration and cannot be set up easily in the CI.

- `long_tests` defines an OCaml executable which gathers tests that
  are run on the LTF (see `lib_performance_regression` above).

- `remote_tests` defines an OCaml executable with some Octez tests that serve
  as examples for the ability of Tezt to run external executables through SSH.

- `vesting_contract_test` defines an OCaml executable which gathers
  tests for the vesting contract. Those tests are too long to run
  in the CI, the goal is to run them manually. (They could be moved to
  `manual_tests`.)

- `self_tests` defines an OCaml executable with some tests that test
  other libraries in this directory (in particular `tezt` itself).

- `snoop` defines an OCaml executable that performs benchmarks for gas.

- `_regressions` defines a set of output expected from a call to an
  RPC or from some OCaml value serialized to JSON. If a file
  is added into this repository, it should be also added in the
  regression test which can be either `tests/RPC_test.ml` or
  `tests/encoding.ml`.

- `records` contains records of the time taken by tests, and a script that updates them.
  Records are used by the CI to distribute tests in jobs of roughly equal duration.
  To update it, get a recent CI pipeline ID and run (from the root of the Tezos repository):

        dune exec scripts/ci/update_records/update.exe -- -i -a from=PIPELINE_ID
        git add tezt/records
        git commit -m 'Tezt: update records'

## Implementation Details

Tezt is implemented in the [OCaml
language](https://ocaml.org). Currently, the project ensures that
libraries `tezt` and `tezt-tezos` do not depend on any library of
`Tezos`. The motto of the library is to provide an **extensive** but
**simple** test framework for Tezos.

An important feature of `tezt-tezos` is the possibility to catch `events`
emitted by the node. This allows fine-grained tests. In particular, as
much as possible, a test in this testsuite should avoid the use of timeouts
and instead, should rely on `events` if possible.

The implementation of the `tezt` library is detailed in its own documentation in the `lib/README.md` file.
