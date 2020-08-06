# Generate genesis information for a new network

Generate a configuration for a new network

## Compile

```bash
dune build gen_genesis.exe
```

## Usage

Peers are optional

```bash
gen_genesis.exe [network_name] [previous_protocol_hash] [bootstrap_peer1] [bootstrap_peer2]
```

Example:
```
gen_genesis.exe dalphanet PtYuensgYBb3G3x1hLLbCmcav8ue8Kyd2khADcL5LsT5R1hcXex "paris.bootzero.tzalpha.net:19732"
```
