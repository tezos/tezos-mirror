# The Tezos blockchain
This directory contains the sources of Tezos, a distributed consensus platform with meta-consensus capability.

## Overview
<!--
- Describe the purpose of this component and how the code in this directory
  works. If needed, design rationale for its API.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
- Describe the security model and assumptions about the crates in this
  directory.
-->

Information about the Tezos project, and the documentation of the Tezos software repository, can be found in the parent directory or online at https://tezos.gitlab.io/

An overview of the software architecture of Tezos is presented in the [octopus drawing](https://tezos.gitlab.io/shell/the_big_picture.html#the-big-picture).
That diagram describes at a high level (and a coarse grain) how the code in this directory is structured and how it works.

Dependencies on external components are mainly managed through the OPAM package manager. The parent directory (qv) also contains a few vendored external OCaml libraries.

## Implementation Details
<!--
- Describe how the component is modeled.
- Describe the code structure and implementation design rationale.
- Other relevant implementation details (e.g. global invariants).
- Testing specifics, if needed.
-->

Tezos is mainly implemented in the [OCaml language](https://ocaml.org).

The software architecture of Tezos at a finer grain is presented as a [more detailed diagram](https://tezos.gitlab.io/shell/the_big_picture.html#packages).
That diagram describes the sources at the level of OPAM packages ([OPAM](https://opam.ocaml.org) is the OCaml package manager).

## API Documentation
<!--
- Link to the external API.
- For the top-level source directory, link to the most important APIs within.
-->

Tezos external and internal APIs are extensively documented at https://tezos.gitlab.io and more specifically therein:
- For installing and getting started with the Tezos software: consult the Introduction Tutorials
- For using the Tezos binaries: consult the User Documentation, and also the Command Line Interface (CLI) reference
- For developing Tezos applications: consult the Developer Tutorials, and also the RPC interface reference and OpenAPI specification
- For contributing to the platform or for integrating at a lower level with Tezos: consult the documentation of the Shell and of the various Protocols, and also the internal APIs in the Online OCaml Documentation
