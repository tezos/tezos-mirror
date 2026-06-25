# Octez documentation

This directory contains the user-facing documentation of Octez, which is written in RST (reStructuredText) format and published online at <https://octez.tezos.com/docs/>.

Always start by reading file README.rst in the current directory, which presents an overview of the documentation and its tools.

For background information on the Octez repository, refer to the AGENTS.md file in the parent directory.

## Runbooks

This section describes some more complex procedures related to the documentation and its maintenance.

### Protocol activation

When a new protocol is activated after being successfully voted on-chain, the documentation must be updated to show it as the active protocol, and drop the documentation of the old protocol.
For that purpose, an MR must be prepared in advance, shifting the active protocol to it.

The preparation of the doc is described in detail in [meta-issue #2170](https://gitlab.com/tezos/tezos/-/issues/2170), section "Protocol activation".
Read the meta-issue and follow it closely.

### Protocol snapshot

When a new protocol proposal is reaady, it is released by snapshotting the development protocol (protocol Alpha).
This snapshot concerns both the code and the documentation, the latter being contained in directory "alpha/".

The snapshot of the documentation part is described in detail in [meta-issue #2170](https://gitlab.com/tezos/tezos/-/issues/2170), section "Protocol snapshot".
Read the meta-issue and follow it closely.

### Background on protocol changes

The Octez implementation consists of an essentially stable part called the shell and a plugin part implementing the Tezos protocol, which can be replaced via on-chain governance. The complete lifecycle of Tezos protocols is described in
the [protocol playbook](developer/protocol_playbook.rst), and in the helper [protocol release checklist](developer/protocol_release_checklist.rst).
Indeed, both pages mention meta-issue ``#2170``.

The previous procedures (protocol snapshot and activation) integrate in the protocol release checklist and reflect the main impact on documentation of protocol changes.
