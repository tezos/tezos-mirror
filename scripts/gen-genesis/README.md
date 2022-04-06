# Generate genesis information for a new network

Generate configuration code (in OCaml) to bootstrap a new network. The result
can be used in `src/bin_node/node_config.ml`.

## Compile

```bash
dune build gen_genesis.exe
```

## Usage

Peers are optional

```bash
gen_genesis.exe <network_name> <genesis_protocol_hash> [bootstrap_peer..]
```

Example:
```
gen_genesis.exe dalphanet Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P "paris.bootzero.tzalpha.net:19732"
```
