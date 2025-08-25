(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let env driver_info s =
  match (Caqti_driver_info.dialect_tag driver_info, s) with
  (* PRIMARY KEY - 64 bits integers *)
  | `Pgsql, "PRIMARY_INCREMENTING_INT" -> Caqti_query.L "BIGSERIAL"
  | `Sqlite, "PRIMARY_INCREMENTING_INT" -> Caqti_query.L "INTEGER"
  (* PRIMARY KEY - 32 bits integers *)
  | `Pgsql, "SMALL_PRIMARY_INCREMENTING_INT" -> Caqti_query.L "SERIAL"
  | `Sqlite, "SMALL_PRIMARY_INCREMENTING_INT" -> Caqti_query.L "INTEGER"
  (* FOREIGN KEY - Refers to a 64 bits PRIMARY KEY *)
  | `Pgsql, "PRIMARY_INCREMENTING_INT_REF" -> Caqti_query.L "BIGINT"
  | `Sqlite, "PRIMARY_INCREMENTING_INT_REF" -> Caqti_query.L "INTEGER"
  (* FOREIGN KEY - Refers to a 32 bits PRIMARY KEY *)
  | `Pgsql, "SMALL_PRIMARY_INCREMENTING_INT_REF" -> Caqti_query.L "INTEGER"
  | `Sqlite, "SMALL_PRIMARY_INCREMENTING_INT_REF" -> Caqti_query.L "INTEGER"
  (* *)
  | `Pgsql, "BYTES" -> Caqti_query.L "BYTEA"
  | `Sqlite, "BYTES" -> Caqti_query.L "BLOB"
  | _, _ -> raise Not_found

let create_delegates =
  "CREATE TABLE IF NOT EXISTS delegates(\n\
  \  id $(SMALL_PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  address $(BYTES) UNIQUE NOT NULL)"

let create_nodes =
  "CREATE TABLE IF NOT EXISTS nodes(\n\
  \  id $(SMALL_PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  name TEXT UNIQUE NOT NULL,\n\
  \  comment TEXT,\n\
  \  password $(BYTES) -- node can't feed if NULL\n\
  \  )"

let create_blocks =
  "CREATE TABLE IF NOT EXISTS blocks(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  predecessor $(PRIMARY_INCREMENTING_INT_REF),\n\
  \  timestamp INTEGER NOT NULL, -- Unix time\n\
  \  hash $(BYTES) UNIQUE NOT NULL,\n\
  \  level INTEGER NOT NULL,\n\
  \  round INTEGER NOT NULL,\n\
  \  baker $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  FOREIGN KEY (baker) REFERENCES delegates(id))"

let create_blocks_reception =
  "CREATE TABLE IF NOT EXISTS blocks_reception(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  application_timestamp TEXT, -- ISO8601 string\n\
  \  validation_timestamp TEXT, -- ISO8601 string\n\
  \  block $(PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  source $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \  FOREIGN KEY (source) REFERENCES nodes(id),\n\
  \  UNIQUE (block, source))"

let create_operations =
  "CREATE TABLE IF NOT EXISTS operations(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  hash $(BYTES) UNIQUE NOT NULL,\n\
  \  endorsement BOOLEAN NOT NULL,\n\
  \  endorser $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  level INTEGER NOT NULL,\n\
  \  round INTEGER,\n\
  \  FOREIGN KEY (endorser) REFERENCES delegates(id))"

let create_operations_reception =
  "CREATE TABLE IF NOT EXISTS operations_reception(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  timestamp TEXT NOT NULL, -- ISO8601 string\n\
  \  operation $(PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  source $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  errors $(BYTES),\n\
  \  FOREIGN KEY (operation) REFERENCES operations(id),\n\
  \  FOREIGN KEY (source) REFERENCES nodes(id),\n\
  \  UNIQUE (operation,source))"

let create_operations_inclusion =
  "CREATE TABLE IF NOT EXISTS operations_inclusion(\n\
  \   id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \   block $(PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \   operation $(PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \   FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \   FOREIGN KEY (operation) REFERENCES operations(id),\n\
  \   UNIQUE (block, operation))"

let create_endorsing_rights =
  "CREATE TABLE IF NOT EXISTS endorsing_rights(\n\
  \   id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \   level INTEGER NOT NULL,\n\
  \   delegate $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \   first_slot INTEGER NOT NULL,\n\
  \   endorsing_power INTEGER NOT NULL,\n\
  \   FOREIGN KEY (delegate) REFERENCES delegates(id),\n\
  \   UNIQUE (level, delegate))"

let create_cycles =
  "CREATE TABLE IF NOT EXISTS cycles(\n\
  \   id INTEGER PRIMARY KEY,\n\
  \   level INTEGER NOT NULL,\n\
  \   size INTEGER NOT NULL,\n\
  \   UNIQUE (level))"

let create_missing_blocks =
  "CREATE TABLE IF NOT EXISTS missing_blocks(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  source $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  level INTEGER NOT NULL,\n\
  \  round INTEGER NOT NULL,\n\
  \  baker $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  FOREIGN KEY (source) REFERENCES nodes(id),\n\
  \  FOREIGN KEY (baker) REFERENCES delegates(id),\n\
  \  UNIQUE (source, level, round))"

module Mutex = struct
  let delegates = Lwt_mutex.create ()

  let nodes = Lwt_mutex.create ()

  let blocks = Lwt_mutex.create ()

  let blocks_reception = Lwt_mutex.create ()

  let operations = Lwt_mutex.create ()

  let operations_reception = Lwt_mutex.create ()

  let operations_inclusion = Lwt_mutex.create ()

  let attesting_rights = Lwt_mutex.create ()

  let cycles = Lwt_mutex.create ()

  let missing_blocks = Lwt_mutex.create ()
end

let create_endorsing_rights_level_idx =
  "CREATE INDEX IF NOT EXISTS endorsing_rights_level_idx ON \
   endorsing_rights(level)"

let create_blocks_level_idx =
  "CREATE INDEX IF NOT EXISTS blocks_level_idx ON blocks(level)"

let create_operations_level_idx =
  "CREATE INDEX IF NOT EXISTS operations_level_idx ON operations(level)"

let create_blocks_reception_block_idx =
  "CREATE INDEX IF NOT EXISTS blocks_reception_block_idx ON \
   blocks_reception(block)"

let create_operations_reception_operation_idx =
  "CREATE INDEX IF NOT EXISTS operations_reception_operation_idx ON \
   operations_reception(operation)"

let create_operations_inclusion_operation_idx =
  "CREATE INDEX IF NOT EXISTS operations_inclusion_operation_idx ON \
   operations_inclusion(operation)"

let create_cycles_level_idx =
  "CREATE INDEX IF NOT EXISTS cycles_level_idx ON cycles(level)"

let create_missing_blocks_level_idx =
  "CREATE INDEX IF NOT EXISTS missing_blocks_level_idx ON missing_blocks(level)"

let create_tables =
  [
    create_delegates;
    create_nodes;
    create_blocks;
    create_blocks_reception;
    create_operations;
    create_operations_reception;
    create_operations_inclusion;
    create_endorsing_rights;
    create_cycles;
    create_missing_blocks;
    create_endorsing_rights_level_idx;
    create_blocks_level_idx;
    create_operations_level_idx;
    create_blocks_reception_block_idx;
    create_operations_reception_operation_idx;
    create_operations_inclusion_operation_idx;
    create_cycles_level_idx;
    create_missing_blocks_level_idx;
  ]

let alter_blocks =
  "ALTER TABLE blocks ADD COLUMN predecessor $(PRIMARY_INCREMENTING_INT_REF)"

let alter_blocks_reception_add_application_timestamp =
  "ALTER TABLE blocks_reception ADD COLUMN application_timestamp TEXT"

let update_blocks_reception_set_application_timestamp_to_timestamp =
  "UPDATE blocks_reception SET application_timestamp = timestamp"

let alter_blocks_reception_drop_timestamp =
  "ALTER TABLE blocks_reception DROP COLUMN timestamp"

let alter_blocks_reception_add_validation_timestamp =
  "ALTER TABLE blocks_reception ADD COLUMN validation_timestamp TEXT"

let alter_nodes = "ALTER TABLE nodes ADD COLUMN password $(BYTES)"

let alter_tables =
  [
    [alter_blocks];
    [
      alter_blocks_reception_add_application_timestamp;
      update_blocks_reception_set_application_timestamp_to_timestamp;
      alter_blocks_reception_drop_timestamp;
      alter_blocks_reception_add_validation_timestamp;
    ];
    [alter_nodes];
  ]

module Type = struct
  let decode_error x =
    Result.map_error
      (fun e ->
        Format.asprintf "%a@." Tezos_error_monad.Error_monad.pp_print_trace e)
      x

  let time_protocol =
    Caqti_type.custom
      ~encode:(fun t -> Result.Ok (Tezos_base.Time.Protocol.to_seconds t))
      ~decode:(fun i -> Result.Ok (Tezos_base.Time.Protocol.of_seconds i))
      Caqti_type.int64

  let block_hash =
    Caqti_type.custom
      ~encode:(fun t -> Result.Ok (Tezos_crypto.Hashed.Block_hash.to_string t))
      ~decode:(fun s ->
        decode_error (Tezos_crypto.Hashed.Block_hash.of_string s))
      Caqti_type.octets

  let operation_hash =
    Caqti_type.custom
      ~encode:(fun t ->
        Result.Ok (Tezos_crypto.Hashed.Operation_hash.to_string t))
      ~decode:(fun s ->
        decode_error (Tezos_crypto.Hashed.Operation_hash.of_string s))
      Caqti_type.octets

  let public_key_hash =
    Caqti_type.custom
      ~encode:(fun t ->
        Result.Ok (Tezos_crypto.Signature.Public_key_hash.to_string t))
      ~decode:(fun s ->
        decode_error (Tezos_crypto.Signature.Public_key_hash.of_string s))
      Caqti_type.octets

  let errors =
    Caqti_type.(
      option
        (custom
           ~encode:(fun errors ->
             Result.map_error
               (fun x ->
                 Format.asprintf "%a@." Data_encoding.Binary.pp_write_error x)
               (Data_encoding.Binary.to_string
                  (Data_encoding.list
                     Tezos_error_monad.Error_monad.error_encoding)
                  errors))
           ~decode:(fun s ->
             Result.map_error
               (fun x ->
                 Format.asprintf "%a@." Data_encoding.Binary.pp_read_error x)
               (Data_encoding.Binary.of_string
                  (Data_encoding.list
                     Tezos_error_monad.Error_monad.error_encoding)
                  s))
           Caqti_type.octets))

  let bcrypt_hash =
    Caqti_type.custom
      ~encode:(fun x -> Ok (Bcrypt.string_of_hash x))
      ~decode:(fun x -> Ok (Bcrypt.hash_of_string x))
      Caqti_type.octets
end

let maybe_insert_source =
  Caqti_request.Infix.(Caqti_type.(string ->. unit))
    "INSERT INTO nodes (name) VALUES (?) ON CONFLICT DO NOTHING"

let maybe_insert_delegate =
  Caqti_request.Infix.(Caqti_type.(Type.public_key_hash ->. unit))
    "INSERT INTO delegates (address) VALUES (?) ON CONFLICT DO NOTHING"

let maybe_insert_attesting_right =
  Caqti_request.Infix.(
    Caqti_type.(t4 int32 int int Type.public_key_hash ->. unit))
    "INSERT INTO endorsing_rights (level, delegate, first_slot, \
     endorsing_power) SELECT ?, delegates.id, ?, ? FROM delegates WHERE \
     delegates.address = ? ON CONFLICT DO NOTHING"

let maybe_insert_operation =
  Caqti_request.Infix.(
    Caqti_type.(
      t2 (t4 int32 Type.operation_hash bool (option int32)) Type.public_key_hash
      ->. unit))
    "INSERT INTO operations (level, hash, endorsement, endorser, round) SELECT \
     $1, $2, $3, delegates.id, $4 FROM delegates WHERE delegates.address = $5 \
     ON CONFLICT DO NOTHING"

let maybe_insert_block =
  Caqti_request.Infix.(
    Caqti_type.(
      t2
        (t4 int32 Type.time_protocol Type.block_hash int32)
        (t2 (option Type.block_hash) Type.public_key_hash)
      ->. unit))
    "INSERT INTO blocks (level, timestamp, hash, round, predecessor, baker) \
     SELECT ?, ?, ?, ?, blocks.id, delegates.id FROM delegates LEFT JOIN \
     blocks ON blocks.hash = ? WHERE delegates.address = ? ON CONFLICT (hash) \
     DO UPDATE SET (timestamp, level, round, predecessor, baker) = \
     (EXCLUDED.timestamp, EXCLUDED.level, EXCLUDED.round, \
     EXCLUDED.predecessor, EXCLUDED.baker) WHERE True"

let maybe_insert_cycle =
  Caqti_request.Infix.(Caqti_type.(t3 int32 int32 int32 ->. unit))
    "INSERT INTO cycles (id, level, size) VALUES (?, ?, ?) ON CONFLICT DO \
     NOTHING"

let insert_missing_block =
  Caqti_request.Infix.(
    Caqti_type.(
      t4
        (* $1 source *) string
        (* $2 level *) int32
        (* $3 round *) int32
        (* $4 baker *) Type.public_key_hash
      ->. unit))
    "\n\
     INSERT INTO missing_blocks (source, level, round, baker)\n\
    \     SELECT nodes.id AS source, $2 AS level, $3 AS round, delegates.id AS \
     baker\n\
    \     FROM nodes\n\
    \     LEFT JOIN delegates ON delegates.address = $4\n\
    \     LEFT JOIN blocks\n\
    \       ON blocks.level = $2\n\
    \       AND blocks.round = $3\n\
    \     LEFT JOIN blocks_reception\n\
    \       ON blocks_reception.block = blocks.id\n\
    \       AND blocks_reception.source = nodes.id\n\
    \     WHERE nodes.name = $1\n\
    \     AND blocks_reception.id IS NULL\n\
     ON CONFLICT DO NOTHING"

let delete_missing_block =
  Caqti_request.Infix.(
    Caqti_type.(
      t3 (* $1 source_name *) string (* $2 level *) int32 (* $3 round *) int32
      ->. unit))
    "DELETE FROM missing_blocks WHERE source = (SELECT id FROM nodes WHERE \
     name = $1) AND level = $2 AND round = $3"

let insert_received_operation =
  Caqti_request.Infix.(
    Caqti_type.(
      t2
        (t4 ptime Type.errors Type.public_key_hash bool)
        (t3 (option int32) string int32)
      ->. unit))
    "INSERT INTO operations_reception (timestamp, operation, source, errors) \
     SELECT $1, operations.id, nodes.id, $2 FROM operations, delegates, nodes \
     WHERE delegates.address = $3 AND operations.endorser = delegates.id AND \
     operations.endorsement = $4 AND ((operations.round IS NULL AND $5 IS \
     NULL) OR operations.round = $5) AND nodes.name = $6 AND operations.level \
     = $7 ON CONFLICT DO NOTHING"

let insert_included_operation =
  Caqti_request.Infix.(
    Caqti_type.(
      t2
        (t3 Type.public_key_hash bool (option int32))
        (t2 Type.block_hash int32)
      ->. unit))
    "INSERT INTO operations_inclusion (block, operation) SELECT blocks.id, \
     operations.id FROM operations, delegates, blocks WHERE delegates.address \
     = $1 AND operations.endorser = delegates.id AND operations.endorsement = \
     $2 AND ((operations.round IS NULL AND $3 IS NULL) OR operations.round = \
     $3) AND blocks.hash = $4 AND operations.level = $5 ON CONFLICT DO NOTHING"

let insert_received_block =
  Caqti_request.Infix.(
    Caqti_type.(
      t4 (option ptime) (option ptime) Type.block_hash string ->. unit))
    "INSERT INTO blocks_reception (application_timestamp, \
     validation_timestamp, block, source) SELECT ?, ?, blocks.id, nodes.id \
     FROM blocks, nodes WHERE blocks.hash = ? AND nodes.name = ? ON CONFLICT \
     (block, source) DO UPDATE SET application_timestamp = \
     COALESCE(blocks_reception.application_timestamp, \
     excluded.application_timestamp), validation_timestamp = \
     COALESCE(blocks_reception.validation_timestamp, \
     excluded.validation_timestamp)"

let maybe_with_metrics (c : Config.t) (name : string) (f : unit -> 'a Lwt.t) =
  if c.with_metrics then Metrics.sql name f else f ()
