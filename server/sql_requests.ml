(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let env driver_info s =
  match (Caqti_driver_info.dialect_tag driver_info, s) with
  | `Pgsql, "PRIMARY_INCREMENTING_INT" -> Caqti_query.L "SERIAL"
  | `Sqlite, "PRIMARY_INCREMENTING_INT" -> Caqti_query.L "INTEGER"
  | `Pgsql, "BYTES" -> Caqti_query.L "BYTEA"
  | `Sqlite, "BYTES" -> Caqti_query.L "BLOB"
  | _, _ -> raise Not_found

let create_delegates =
  "CREATE TABLE IF NOT EXISTS delegates(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  address $(BYTES) UNIQUE NOT NULL)"

let create_nodes =
  "CREATE TABLE IF NOT EXISTS nodes(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  name TEXT UNIQUE NOT NULL,\n\
  \  comment TEXT)"

let create_blocks =
  "CREATE TABLE IF NOT EXISTS blocks(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  predecessor INTEGER,\n\
  \  timestamp INTEGER NOT NULL, -- Unix time\n\
  \  hash $(BYTES) UNIQUE NOT NULL,\n\
  \  level INTEGER NOT NULL,\n\
  \  round INTEGER NOT NULL,\n\
  \  baker INTEGER NOT NULL,\n\
  \  FOREIGN KEY (baker) REFERENCES delegates(id))"

let create_blocks_reception =
  "CREATE TABLE IF NOT EXISTS blocks_reception(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  application_timestamp TEXT, -- ISO8601 string\n\
  \  validation_timestamp TEXT, -- ISO8601 string\n\
  \  block INTEGER NOT NULL,\n\
  \  source INTEGER NOT NULL,\n\
  \  FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \  FOREIGN KEY (source) REFERENCES nodes(id),\n\
  \  UNIQUE (block, source))"

let create_operations =
  "CREATE TABLE IF NOT EXISTS operations(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  hash $(BYTES) UNIQUE NOT NULL,\n\
  \  endorsement BOOLEAN NOT NULL,\n\
  \  endorser INTEGER NOT NULL,\n\
  \  level INTEGER NOT NULL,\n\
  \  round INTEGER,\n\
  \  FOREIGN KEY (endorser) REFERENCES delegates(id))"

let create_operations_reception =
  "CREATE TABLE IF NOT EXISTS operations_reception(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  timestamp TEXT NOT NULL, -- ISO8601 string\n\
  \  operation INTEGER NOT NULL,\n\
  \  source INTEGER NOT NULL,\n\
  \  errors $(BYTES),\n\
  \  FOREIGN KEY (operation) REFERENCES operations(id),\n\
  \  FOREIGN KEY (source) REFERENCES nodes(id),\n\
  \  UNIQUE (operation,source))"

let create_operations_inclusion =
  "CREATE TABLE IF NOT EXISTS operations_inclusion(\n\
  \   id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \   block INTEGER NOT NULL,\n\
  \   operation INTEGER NOT NULL,\n\
  \   FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \   FOREIGN KEY (operation) REFERENCES operations(id),\n\
  \   UNIQUE (block, operation))"

let create_endorsing_rights =
  "CREATE TABLE IF NOT EXISTS endorsing_rights(\n\
  \   id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \   level INTEGER NOT NULL,\n\
  \   delegate INTEGER NOT NULL,\n\
  \   first_slot INTEGER NOT NULL,\n\
  \   endorsing_power INTEGER NOT NULL,\n\
  \   FOREIGN KEY (delegate) REFERENCES delegates(id),\n\
  \   UNIQUE (level, delegate))"

let create_endorsing_rights_level_idx =
  "CREATE INDEX IF NOT EXISTS endorsing_rights_level_idx ON \
   endorsing_rights(level)"

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
    create_endorsing_rights_level_idx;
    create_operations_level_idx;
    create_blocks_reception_block_idx;
    create_operations_reception_operation_idx;
    create_operations_inclusion_operation_idx;
  ]

let alter_blocks = "ALTER TABLE blocks ADD COLUMN predecessor INTEGER"

let alter_tables = [alter_blocks]

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
end

let maybe_insert_source =
  Caqti_request.Infix.(Caqti_type.(string ->. unit))
    "INSERT INTO nodes (name) VALUES (?) ON CONFLICT DO NOTHING"

let maybe_insert_delegate =
  Caqti_request.Infix.(Caqti_type.(Type.public_key_hash ->. unit))
    "INSERT INTO delegates (address) VALUES (?) ON CONFLICT DO NOTHING"

let maybe_insert_endorsing_right =
  Caqti_request.Infix.(
    Caqti_type.(tup4 int32 int int Type.public_key_hash ->. unit))
    "INSERT INTO endorsing_rights (level, delegate, first_slot, \
     endorsing_power) SELECT ?, delegates.id, ?, ? FROM delegates WHERE \
     delegates.address = ? ON CONFLICT DO NOTHING"

let maybe_insert_operation =
  Caqti_request.Infix.(
    Caqti_type.(
      tup2
        (tup4 int32 Type.operation_hash bool (option int32))
        Type.public_key_hash
      ->. unit))
    "INSERT INTO operations (level, hash, endorsement, endorser, round) SELECT \
     $1, $2, $3, delegates.id, $4 FROM delegates WHERE delegates.address = $5 \
     AND NOT EXISTS ( SELECT 1 FROM operations WHERE endorsement = $3 AND \
     endorser = delegates.id AND round = $4 AND level = $1) ON CONFLICT DO \
     NOTHING"

let maybe_insert_block =
  Caqti_request.Infix.(
    Caqti_type.(
      tup2
        (tup4 int32 Type.time_protocol Type.block_hash int32)
        (tup2 (option Type.block_hash) Type.public_key_hash)
      ->. unit))
    "INSERT INTO blocks (level, timestamp, hash, round, predecessor, baker) \
     SELECT ?, ?, ?, ?, blocks.id, delegates.id FROM delegates LEFT JOIN \
     blocks ON blocks.hash = ? WHERE delegates.address = ? ON CONFLICT (hash) \
     DO UPDATE SET (timestamp, level, round, predecessor, baker) = \
     (EXCLUDED.timestamp, EXCLUDED.level, EXCLUDED.round, \
     EXCLUDED.predecessor, EXCLUDED.baker) WHERE True"

let insert_received_operation =
  Caqti_request.Infix.(
    Caqti_type.(
      tup2
        (tup4 ptime Type.errors Type.public_key_hash bool)
        (tup3 (option int32) string int32)
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
      tup2
        (tup3 Type.public_key_hash bool (option int32))
        (tup2 Type.block_hash int32)
      ->. unit))
    "INSERT INTO operations_inclusion (block, operation) SELECT blocks.id, \
     operations.id FROM operations, delegates, blocks WHERE delegates.address \
     = $1 AND operations.endorser = delegates.id AND operations.endorsement = \
     $2 AND ((operations.round IS NULL AND $3 IS NULL) OR operations.round = \
     $3) AND blocks.hash = $4 AND operations.level = $5 ON CONFLICT DO NOTHING"

let insert_received_block =
  Caqti_request.Infix.(
    Caqti_type.(
      tup4 (option ptime) (option ptime) Type.block_hash string ->. unit))
    "INSERT INTO blocks_reception (application_timestamp, \
     validation_timestamp, block, source) SELECT ?, ?, blocks.id, nodes.id \
     FROM blocks, nodes WHERE blocks.hash = ? AND nodes.name = ? ON CONFLICT \
     DO UPDATE SET application_timestamp = COALESCE(application_timestamp, \
     excluded.application_timestamp), validation_timestamp = \
     COALESCE(validation_timestamp, excluded.validation_timestamp)"
