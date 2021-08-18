# The Tezos blockchain

This directory contains the sources of the Tezt framework. Tezt is a
generic test framework that is used for writing tests in OCaml for
Tezos.

## Overview

Information about Tezt and its documentation can be found online at
https://tezos.gitlab.io/developer/tezt.html

The Tezt directory contains several directories:

- `lib` defines an OCaml library named `tezt` which contains the test
  framework. It is independent from Tezos.

- `lib_tezos` defines another OCaml library named `tezt-tezos` which
  is specific to Tezos, complementing the above with Tezos-specific
  features.

- `tests` defines an OCaml executable which gathers automatic tests
  written for Tezos.

- `manual_tests` defines an OCaml executable which gathers manual
  tests written for Tezos. A manual test is a test which requires
  extra configuration and cannot be set up easily in the CI.

- `_regressions` defines a set of output expected from a call to an
  RPC or from some OCaml value serialized to JSON. If a file
  is added into this repository, it should be also added in the
  regression test which can be either `tests/RPC_test.ml` or
  `tests/encoding.ml`.

- `test-results.json` is a record of the time taken by tests.
  It is used by the CI to distribute tests in jobs of roughly equal duration.
  To update it, run (from the root of the Tezos repository):

    make && dune exec tezt/tests/main.exe -- --record tezt/test-results.json

## Implementation Details

Tezt is implemented in the [OCaml
language](https://ocaml.org). Currently, the project ensures that both
libraries `tezt` and `tezt-tezos` do not depend on any library of
`Tezos`. The motto of the library is to provide an **extensive** but
**simple** test framework for Tezos.

An important feature of `tezt` is the possibility to catch `events`
emitted by the node. This allows fine-grained tests. In particular, as
much as possible, a test in `tezt` should avoid the use of `timeouts`
and instead, should rely on `events` if possible.

## API Documentation

The `tezt` and `tezt-tezos` APIs are documented in the
interface files (end by `mli`). Both APIs are available online:
https://tezos.gitlab.io/api/api-inline.html

The user manual of `tezt` is also available online on the [tezt
page](https://tezos.gitlab.io/developer/tezt.html).
