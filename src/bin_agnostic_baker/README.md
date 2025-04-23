# Agnostic Baker

## Overview

Agnostic Baker is a protocol-independent binary that dynamically selects the
appropriate baking process based on the active protocol. It monitors the state of
the blockchain and automatically switches to the correct process when a new
protocol is encountered (for example, during migrations, or at startup).

It is designed to simplify the baking process for users, such that they will no
longer need to run two baker binaries at migration time. This makes the need for
protocol specific baking binaries obsolete.

## Usage

To run the agnostic baker, the command line syntax is similar to the one
for the protocol-dependent baking binaries:

```bash
octez-baker [OCTEZ-BAKER-COMMANDS]
```

The `[OCTEZ-BAKER-COMMANDS]` list consists of all the arguments that can be used
for the specific protocol baking binary. To be more clear, if a user wants to use
the agnostic baker to replace a baking command which would be

```bash
octez-baker-<protocol> [OCTEZ-BAKER-COMMANDS]
```

they can do this by using the same `[OCTEZ-BAKER-COMMANDS]` and let the agnostic
baker run the baker process for `<protocol>`, information obtained from the node.

With the introduction of the agnostic baker, a unification of the CLI has also been
achieved, therefore there will not be incompatibilities between two consecutive protocol
baking commands.

## How it works

1. **Initialization**: The daemon starts and connects to the specified Tezos node.
2. **Protocol Detection**: It fetches the currently active protocol. This is done via an RPC request on the `head` metadata against the Tezos node.
3. **Baker Selection**: Based on the active protocol, it selects the corresponding baking process.
4. **Baker Execution**: The chosen baker process is executed with the specified arguments
(`[OCTEZ-BAKER-COMMANDS]`).
5. **Chain Monitoring**: The daemon continuously monitors the chain for new blocks and protocol changes (based on the voting period).
6. **Protocol Updates**: If a new protocol is encountered, the daemon stops the old baker process after a few levels have
passed since the migration (for safety purposes), and starts a new baker process for the new protocol. This is currently done via an Lwt cancelling mechanism.
