# lib_sqlite -- Thin type-safe SQLite wrapper

Library name: `octez_sqlite` (package `octez-l2-libs`)

## Purpose

Wraps Caqti with type-safe query operators, connection pooling, transactions, and OpenTelemetry tracing. Used by the rollup node store (`sql_store.ml` in `lib_smart_rollup_node`).

## Type-safe query operators

```ocaml
val ( ->. ) : 'a Caqti_type.t -> unit Caqti_type.t -> ... -> ('a, unit, [`Zero]) Request.t
val ( ->! ) : ... -> ('a, 'b, [`One]) Request.t         (* exactly one row *)
val ( ->? ) : ... -> ('a, 'b, [`One | `Zero]) Request.t (* zero or one row *)
val ( ->* ) : ... -> ('a, 'b, [`Many | `One | `Zero]) Request.t (* many rows *)
```

Each request accepts optional `~name`, `~table`, `~attrs` for OpenTelemetry tracing.

## Connection pooling

- `Read_write`: pool size = 1 (SQLite exclusive write lock)
- `Read_only {pool_size}`: configurable pool for concurrent reads
- Connections validated and recycled per `max_conn_reuse_count`

## Transactions

- `with_transaction conn f` -- auto COMMIT/ROLLBACK
- Phantom types prevent nested transactions (raises error)
- WAL journal mode set automatically
- `assert_in_transaction` -- debug assertion to verify you're in a transaction

## WARNING: Deadlock in callbacks

`fold_s` and `iter_s` callbacks that call back into the same pool can deadlock, because `Read_write` has only 1 connection. If the callback needs to issue queries, use the same `conn` passed to the outer query, not `Sqlite.use`.

## Tracing

All queries emit OpenTelemetry spans with `db.system.name = "sqlite"`, operation name, table, and query text.

## Files

- `sqlite.ml/mli` -- entire implementation (`sqlite.mli` is the full API reference)

## Build

```
dune build src/lib_sqlite/
```
