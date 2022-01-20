# Lwtreslib: an Lwt- and Result-friendly addition/replacement for the Stdlib

This package provides functions that complement OCaml Stdlib, specifically Lwt-
and Result-friendly additions.

## API Documentation

See the [online API documentation](https://tezos.gitlab.io/api/odoc/_html/tezos-lwt-result-stdlib/index.html).

If you are contributing to Lwtreslib, carefully read and be sure to adhere to the *Design Principles* mentioned in the documentation.

## Overview

Lwtreslib provides a replacement for OCaml's Stdlib intended to be used in
projects which rely on Lwt and Result.

Lwtreslib is used heavily in Octez. It is instantiated in the Error-monad and
made available through lib-base's `TzPervasives`.

## Implementation Details

The sources of Lwtreslib are organised as follow:

- `bare/` contains the sources for a bare-bones implementation of Lwtreslib that
provides monadic combinators and collection traversals.

    - `bare/sigs/` contains the sources for the signatures of all the modules
      exported by `bare/`
    - `bare/functor_outputs` contains the sources for the signatures of all the
      modules constructed by functors exported by `bare/`
    - `bare/structs` contains the sources for of all the modules exported by
      `bare/`
    - `examples/traces/` contains multiple example implementation of traces. A
      trace is a data-structure that holds multiple errors organised in a way
      that reflects the way the errors happened. Specifically, errors can be
      stringed together to represent the fact that control flow traversed multiple
      points, or they can be held side-by-side to indicate the fact that they
      happenned in simultaneously evaluating promises.

      The code in this directory is meant more as examples than fully-fledged
      traces, but they can also be used for prototyping or as a basis for a more
      complete trace implementation.

- `traced/` contains the sources for a trace-enabled implementation of Lwtreslib
  that provides monadic combinators and collection traversals. This
  implementation provides all the functionality of `bare/` with added support
  for traces (i.e., structured collections of errors).

    - `traced/sigs/` contains the sources for the signatures of all the modules
      exported by `traced/`
    - `traced/functor_outputs` contains the sources for the signatures of all
      the modules constructed by functors exported by `bare/`
    - `traced/structs` contains the sources for of all the modules exported by
      `bare/`. These modules are functorised over the implementation of a trace.
      The file `traced/structs/structs.ml` contains an all-in-one functor for
      instantiating all of the modules.
    - `test/` contains code to test the library.

