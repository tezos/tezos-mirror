# The Tezos protocol
<!-- Summary line: One sentence about this component. -->
This directory contains a development version of the Tezos economic protocol and some protocol-related components and APIs.

## Overview
<!--
- Describe the purpose of this component and how the code in this directory
  works. If needed, design rationale for its API.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
- Describe the security model and assumptions about the crates in this
  directory.
-->

First of all, one must distinguish the Tezos economic protocol from the other protocol-related components:
- The economic protocol is implemented in subdirectory `lib_protocol/`. This
  code is subject to on-chain governance, and the protocol hash is computed on
  the contents of this subdirectory (excluding its own subdirectory ``test/``).
- Everything else in this directory depends on the economic protocol but is not
  subject to the on-chain governance, and hence is not involved in the protocol
  hash computation.

There are other protocol-related files outside of this directory. Integration tests are located in `tezt/`. Protocol documentation is located in `docs/alpha/` and the protocol changelog is in `docs/protocols/alpha.rst`.

## Implementation Details
<!--
- Describe how the component is modeled.
- Describe the code structure and implementation design rationale.
- Other relevant implementation details (e.g. global invariants).
- Testing specifics, if needed.
-->

The implementation contains the economic protocol itself and the additional
components described in the subsequent subsection.

### The economic protocol

For a high-level overview of the Tezos economic protocol implementation (in the
`lib_protocol/` subdirectory), see
<https://tezos.gitlab.io/developer/entering_alpha.html>

### Other components

The rest of the implementation is structured in the following subdirectories:
- `bin_accuser/`, `bin_baker/`, and `bin_endorser/`: these directories implement protocol-specific binaries for bakers and other nodes actively participating in the economic protocol.
- `lib_client/`, `lib_client_commands/`, `lib_client_sapling/`

  These directories contain the protocol-specific parts of the client.
  + `lib_client` contains many wrappers around RPC calls from the protocol and the RPC plugin, and also the client part of the Michelson runtime (mostly macro expansion, location translation, and pretty-printing of errors).
  + `lib_client_commands` contains the command definitions using CLIC.
  + `lib_client_sapling` is the Sapling part of the client.
- `lib_delegate/`: this directory contains code that is used by node "clients" (the client, the baker, the endorser, the accuser) to interact with the economic protocol and to implement consensus related tasks (like baking).
- `lib_parameters/`: this directory defines the global constants that the protocol is instantiated with.
- `lib_plugin/`

  This directory packages some code that the shell needs for interacting with the protocol. It's not strictly speaking protocol code (it doesn't define the context, it doesn't restrict the validity of operations, etc.), but it's adjacent enough that it needs to have access to the protocol's internal representations for things (e.g., it has some RPCs that look on the inside of some operations).

  It is currently used by the mempool to filter operations based on their content and to provide protocol-specific RPCs without modifying the protocol code (and hence the hash of the protocol).
- `parameters/`: this directory holds the generated JSON files containing the protocol parameters for possible "networks". Concretely there are three sets of generated parameters: for mainnet, for sandbox, and for (protocol unit) tests.

## API Documentation
<!--
- Link to the external API.
- For the top-level source directory, link to the most important APIs within.
-->

The main API of the protocol is:
https://tezos.gitlab.io/api/odoc/_html/tezos-protocol-alpha
