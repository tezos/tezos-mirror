# `tezindex`

`tezindex` is an experimental Tezos blockchain indexer that monitors new blocks
from an Octez Layer 1 node, extracts balance updates from block metadata, and
stores them in a SQLite database. It also exposes a local RPC server for
querying the indexed data.

## Features

- **Real-time block monitoring** -- Subscribes to the node's head stream and
  processes each new block as it arrives.
- **Balance update extraction** -- Parses block metadata to extract
  protocol-level balance updates (baking rewards, attestation rewards, fees,
  etc.) and pairs debited sources with credited destinations.
- **SQLite storage** -- Stores indexed balance updates in a local SQLite
  database using Caqti (with dialect support for PostgreSQL).
- **RPC server** -- Optional local HTTP server for querying indexed data.
- **Cycle aggregation** -- Each balance update is tagged with its cycle,
  enabling per-cycle reward queries via the RPC server.
- **Protocol-aware architecture** -- Uses a plugin system where each protocol
  version registers its own balance update extractor. Currently supports
  protocol `024_PtTALLiN` and `alpha`.

## Building

From the repository root:

```
eval $(opam env) && make
```

The binary is built as `octez-tezindex`.

## Usage

```
octez-tezindex [global options] run [options]
```

### Global options

| Option | Description |
|---|---|
| `--base-dir <path>` / `-d` | Data directory for tezindex (default: `~/.octez-indexer`, or `$OCTEZ_INDEXER_DIR`) |
| `--endpoint <uri>` / `-E` | Octez L1 node RPC endpoint (e.g. `http://localhost:8732`) |

### Run options

| Option | Description |
|---|---|
| `--rpc-addr <ADDR:PORT>` | Start a local RPC server on the given address (default port: `8733`) |
| `--external-rpc-addr <ADDR:PORT>` | External RPC server address |
| `--db-name <name>` | Database filename, relative to `--base-dir` (default: `db.sqlite`) |
| `--watched-address <PKH>` | Public key hash of an account to index. Can be specified multiple times |

### Examples

Monitor two baker accounts, storing data in the default location:

```
octez-tezindex --base-dir ~/.tezindex --endpoint http://localhost:8732 run \
  --watched-address tz1abc... \
  --watched-address tz1def...
```

With the RPC server enabled:

```
octez-tezindex --base-dir ~/.tezindex --endpoint http://localhost:8732 run \
  --rpc-addr localhost:8733 \
  --watched-address tz1abc...
```

Custom database name:

```
octez-tezindex --endpoint http://localhost:8732 run \
  --db-name myindex.sqlite \
  --watched-address tz1abc...
```

## How it works

1. **Bootstrap** -- Connects to the L1 node and waits for it to be
   bootstrapped.
2. **Head stream** -- Subscribes to `Shell_services.Monitor.heads` to receive
   new block headers in real time.
3. **Protocol detection** -- For each block, detects the active protocol and
   looks up the matching protocol-specific balance update extractor.
4. **Metadata extraction** -- Fetches block metadata via `Block_services.metadata`
   and extracts balance update items from the protocol data.
5. **Pairing** -- Balance updates come as debit/credit pairs. The extractor
   matches debited sources (e.g. `Baking_rewards`) with credited destinations
   (e.g. `Baker`, `Contract`, `Staker`) using a sliding window.
6. **Storage** -- Each paired balance update is inserted into the
   `block_balance_updates` SQLite table with `ON CONFLICT DO NOTHING` to handle
   re-processing.

## Indexed data

### Balance update categories (debited sources)

| Category | Description |
|---|---|
| `Block_fees` | Transaction fees collected in the block |
| `Baking_rewards` | Rewards for producing the block |
| `Baking_bonuses` | Additional baking bonuses |
| `Attestation_rewards` | Rewards for attesting previous blocks |
| `Dal_attestation_rewards` | Rewards for DAL attestation |

### Balance update results (credited destinations)

| Result | Description |
|---|---|
| `Contract` | Credited to an implicit account (tz1/tz2/tz3/tz4) |
| `Baker_own_stake` | Credited to baker's own staked deposits |
| `Delegate` | Credited to shared stakers pool |
| `Baker_edge` | Credited to baker's edge |
| `Staker` | Credited to a specific staker |
| `Lost` | Lost attestation or DAL attestation rewards |

### Database schema

The database contains a `block_balance_updates` table:

```sql
CREATE TABLE IF NOT EXISTS block_balance_updates(
  id       INTEGER PRIMARY KEY,
  block    INTEGER NOT NULL,
  cycle    INTEGER NOT NULL,
  address  BLOB NOT NULL,
  category TEXT NOT NULL,
  result   TEXT NOT NULL,
  value    INTEGER NOT NULL,
  UNIQUE (block, address, category, result)
);

CREATE INDEX IF NOT EXISTS block_balance_updates_idx
  ON block_balance_updates(block);
CREATE INDEX IF NOT EXISTS block_balance_updates_cycle_address_idx
  ON block_balance_updates(cycle, address);
```

- `block` -- Block level (int32)
- `cycle` -- Cycle number (int32)
- `address` -- Public key hash stored as binary
- `category` -- One of the categories above (text)
- `result` -- One of the results above (text)
- `value` -- Amount in mutez (int64)

## RPC server

When started with `--rpc-addr`, tezindex exposes a local HTTP RPC server.

### Endpoints

| Method | Path | Description |
|---|---|---|
| GET | `/health` | Returns `"ok"` if the service is running |
| GET | `/v1/rewards/split/<baker>/<cycle>` | Returns baker rewards for a cycle in TzKT-compatible format |

#### `GET /v1/rewards/split/<baker>/<cycle>`

Returns a flat JSON object with baker rewards for the given cycle, using
camelCase field names compatible with the
[TzKT `/v1/rewards/split` API](https://api.tzkt.io/#operation/Rewards_GetRewardSplit).
The data is aggregated from per-block balance updates using
`SUM(value) GROUP BY category, result`.

Currently populated fields:

| Field | Source |
|---|---|
| `blockRewardsStakedOwn/Edge/Shared/Delegated` | `Baking_rewards` + `Baking_bonuses` |
| `missedBlockRewards` | `Baking_rewards` / `Baking_bonuses` with `Lost` result |
| `attestationRewardsStakedOwn/Edge/Shared/Delegated` | `Attestation_rewards` |
| `missedAttestationRewards` | `Attestation_rewards` with `Lost` result |
| `dalAttestationRewardsStakedOwn/Edge/Shared/Delegated` | `Dal_attestation_rewards` |
| `missedDalAttestationRewards` | `Dal_attestation_rewards` with `Lost` result |
| `blockFees` | `Block_fees` (non-Lost) |
| `missedBlockFees` | `Block_fees` with `Lost` result |

All other TzKT fields are present in the response but default to zero/empty.

Example:

```
curl http://localhost:8733/v1/rewards/split/tz1.../800
```

```json
{
  "cycle": 800,
  "blockRewardsStakedOwn": "123456",
  "blockRewardsStakedEdge": "0",
  "blockRewardsStakedShared": "0",
  "blockRewardsDelegated": "0",
  "attestationRewardsStakedOwn": "789012",
  "missedAttestationRewards": "0",
  ...
}
```

## Architecture

```
tezindex/
  lib_tezindex/           Library
    config.ml               CLI arguments and configuration
    data.ml                 Balance update types and encodings (DB-level)
    split_data.ml           TzKT-compatible reward split types and encodings
    db.ml                   Database pool initialization and table creation
    sql_requests.ml         SQL schema, dialect env, insert/select queries
    rpc_server.ml           Local RPC server (health, v1/rewards/split endpoints)
    log.ml                  Logging with verbosity levels
  bin_tezindex/           Executable
    tezindex_main.ml        Entry point, wires DB + RPC + archiver
    general_archiver.ml     Main loop: monitors head stream, dispatches to protocol machines
    protocol_machinery.ml   PROTOCOL_SERVICES module type interface
    alpha_machine.real.ml     Alpha protocol balance update extractor (canonical source)
    PtTALLiN_machine.real.ml  Protocol 024 balance update extractor
```

### Protocol machine pattern

Each protocol implements the `PROTOCOL_SERVICES` signature via the
`General_archiver.Define` functor, which registers the protocol's balance
update recorder in a hash table keyed by protocol hash. At startup, a
side-effecting module reference (e.g. `module M024 = PtTALLiN_machine.M`)
triggers registration. This makes adding support for new protocols
straightforward: implement the signature and add a module reference.

## License

MIT
