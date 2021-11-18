# Tezos TPS evaluation tool

## Estimating the average block

The CLI tool can be used to extract statistics about transactions and originations using [Tezos
indexer](https://gitlab.com/nomadic-labs/tezos-indexer) snapshots.

Please refer to tezos-indexer's [SQL schema](https://gitlab.com/nomadic-labs/tezos-indexer/-/tree/master/src/db-schema) for more
information about the stored data.

### Prerequisites

In order to run `tps-evaluation` you'll need a running PostgreSQL instance with a snapshot (either
full or partial) from the Tezos indexer.

[Here](https://z.lamini.ca/v970.mainnet.1729000.tar.bz2) is a snapshot containing blocks 0 to 1.729M. And
[here](https://z.lamini.ca/recent_blocks_mainnet.2021-Oct-5.sql.bz2) is a partial snapshot between
2021-09-03 and 2021-10-05.

Once you have snapshot, you can dump it to the PostgreSQL instance:

```bash
bunzip2 -c v970.mainnet.1729000.tar.bz2 | psql -h <pg-instance> -U <pg-user> -a <your-db-name>
```

This might take a while depending on the hardware you use.

Once the above command is complete you'll be able to use `tps-evaluation` to query the database.

### Example usage

```bash
./tezos-tps-evaluation estimate-average-block -c <connection-string> --start 2021-09-01 --end 2021-09-30 -p 0.04
{
  "regular": 2989498,
  "origination": 5498,
  "contract": {
    "most_used": {
      "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton": 1772827,
      "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn": 1165878,
      "KT1GRSvLoikDsXujKgZPsGLX8k8VvR2Tq95b": 333703
    }
  },
  "total_operations": 8214832
}
```
