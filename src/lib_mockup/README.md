# Mockup mode

Libraries for mockup mode, i.e., the implementation of
`tezos-client`'s `--mode mockup` flags.

## Overview

There are 3 libraries in this directory (see `dune` file):

* `tezos_mockup`
* `tezos_mockup_registration`
* `tezos_mockup_commands`


### Tezos mockup 

This library defines a mockup implementation of RPCs that are handled locally
in mockup mode while they are executed on a node by vanilla clients.

Mockup-specific services (e.g., baking RPCs) are implemented in `local_services.ml`
The mockup RPC layer is itself in `RPC_client.ml`.

Higher-level mockup interaction is located in `persistence.ml`, in particular to
handle mockup states (creation, modification, etc.).

### Tezos mockup registration

This library defines the interface that is expected to be implemented by
the protocol. This then allows to register a protocol and support it in
mockup mode. See for example `src/proto_alpha/lib_client/mockup.ml`.

`mockup_args.ml` handles mockup (dummy) chain ids.

### Tezos mockup commands

Mockup commands that are independent from the protocol (one of `list mockup
protocols`) are implemented here. Currently, there is only one such command
(`list mockup protocols` indeed). All other mockup client commands are the same as for
vanilla clients, except for the additional `--mode mockup` switch.

Protocol-specific mockup commands are implemented in the respective
`src/proto_*/lib_client_commands/client_proto_mockup_commands.ml`. For example,
for protocol `alpha`, this is done in
`src/proto_alpha/lib_client_commands/client_proto_mockup_commands.ml`.  `create
mockup` is an example of such a command.

All mockup commands are used in
`src/lib_client_base_unix/client_config.ml`. More precisely, there is a
dedicated mockup context object in
`src/lib_client_base_unix/client_context_unix.ml`, delegating its RPC calls to
the local mockup implementation


## Testing 

Tests for mockup mode use the `tezt` testing framework.

Run at toplevel

```
dune exec tezt/tests/main.exe -- mockup
```

## API Documentation

- https://tezos.gitlab.io/api/odoc/_html/tezos-mockup/index.html

### Other documentation

- [Manual](https://tezos.gitlab.io/user/mockup.html)
- [Blog post](https://blog.nomadic-labs.com/introducing-mockup-mode-for-tezos-client.html)
