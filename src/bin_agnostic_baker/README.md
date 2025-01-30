# Agnostic Baker (experimental)

## Overview

Agnostic Baker is a protocol-independent binary that dynamically selects the
appropriate baking binary based on the active protocol. It monitors the state of
the blockchain and automatically switches to the correct binary when a new
protocol is encountered (for example, during migrations, or at startup).

It is designed to simplify the baking process for users, such that they will no
longer need to run two baker binaries at migration time.

## Experimental purpose

For now, the binary is continuously being developed and tested. This is the reason
why users are warned that the binary is experimental and that it should not be
used for real-life scenarios, for instance, baking on `mainnet`.

## Usage

To run the agnostic baker, the command line syntax is quite similar to the one
for the protocol-dependent baking binaries:

```bash
./octez-experimental-agnostic-baker [OCTEZ-EXPERIMENTAL-AGNOSTIC-BAKER-COMMANDS] \
-- [OCTEZ-BAKER-COMMANDS]
```

The `[OCTEZ-EXPERIMENTAL-AGNOSTIC-BAKER-COMMANDS]` list consists of arguments specific
to the agnostic baker binary and they include:

- `--binaries-directory` : where to find the baker binaries to use
- `--endpoint` : endpoint to communicate with the connected node
- `--base-dir` : directory dedicated to the agnostic baker (that can contain logs
and configuration files)
-- `--help` : displays help information

The `[OCTEZ-BAKER-COMMANDS]` list consists of all the arguments that can be used
for the specific protocol baking binary. To be more clear, if a user wants to use
the agnostic baker to replace a baking command which would be

```bash
./octez-baker-<protocol> [OCTEZ-BAKER-COMMANDS]
```

they can do this by using the same `[OCTEZ-BAKER-COMMANDS]` and let the agnostic
baker run the binary for `<protocol>`, information obtained from the node.

Notice that the two types of arguments are separated by a clear `--`.

## How it works

1. **Initialization**: The daemon starts and connects to the specified Tezos node.
2. **Protocol Detection**: It fetches the currently active protocol. This is done via an RPC request on the `head` metadata against the Tezos node.
3. **Baker Selection**: Based on the active protocol, it selects the corresponding `octez-baker-<protocol>` binary.
4. **Baker Execution**: The chosen binary is executed with the specified arguments
(`[OCTEZ-BAKER-COMMANDS]`).
5. **Chain Monitoring**: The daemon continuously monitors the chain for new blocks and protocol changes (based on the voting period).
6. **Protocol Updates**: If a new protocol is encountered, the daemon stops the current baker and starts a new one matching the new protocol.
