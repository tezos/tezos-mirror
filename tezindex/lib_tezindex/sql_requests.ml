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

let create_block_balance_updates =
  "CREATE TABLE IF NOT EXISTS block_balance_updates(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  block $(PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  cycle $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  address $(BYTES) NOT NULL,\n\
  \  category TEXT NOT NULL,\n\
  \  result  TEXT NOT NULL,\n\
  \  value INTEGER NOT NULL,\n\
  \  UNIQUE (block, address, category, result))"

module Mutex = struct
  let balance_updates = Lwt_mutex.create ()
end

let create_block_balance_updates_idx =
  "CREATE INDEX IF NOT EXISTS block_balance_updates_idx ON \
   block_balance_updates(block)"

let create_block_balance_updates_cycle_address_idx =
  "CREATE INDEX IF NOT EXISTS block_balance_updates_cycle_address_idx ON \
   block_balance_updates(cycle, address)"

let create_cycle_delegators =
  "CREATE TABLE IF NOT EXISTS cycle_delegators(\n\
  \  id $(PRIMARY_INCREMENTING_INT) PRIMARY KEY,\n\
  \  baker $(BYTES) NOT NULL,\n\
  \  cycle $(SMALL_PRIMARY_INCREMENTING_INT_REF) NOT NULL,\n\
  \  delegator TEXT NOT NULL,\n\
  \  delegated_balance INTEGER NOT NULL,\n\
  \  UNIQUE (baker, cycle, delegator))"

let create_tables =
  [
    create_block_balance_updates;
    create_block_balance_updates_idx;
    create_block_balance_updates_cycle_address_idx;
    create_cycle_delegators;
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

let insert_block_balance_update =
  Caqti_request.Infix.(
    Caqti_type.(
      t2
        (t3
           (* $1 level *) int32
           (* $2 cycle *) int32
           (* $3 address *) Type.public_key_hash)
        (t3
           (* $4 category *) string
           (* $5 result *) string
           (* $6 value *) int64)
      ->. unit))
    "INSERT INTO block_balance_updates \
     (block,cycle,address,category,result,value) VALUES (?,?,?,?,?,?) ON \
     CONFLICT DO NOTHING"

let select_cycle_balance_updates =
  Caqti_request.Infix.(
    Caqti_type.(t2 Type.public_key_hash int32 ->* t3 string string int64))
    "SELECT category, result, SUM(value) FROM block_balance_updates WHERE \
     address = ? AND cycle = ? GROUP BY category, result"

let insert_cycle_delegator =
  Caqti_request.Infix.(
    Caqti_type.(
      t2
        (t2 (* $1 baker *) Type.public_key_hash (* $2 cycle *) int32)
        (t2 (* $3 delegator *) string (* $4 delegated_balance *) int64)
      ->. unit))
    "INSERT INTO cycle_delegators (baker,cycle,delegator,delegated_balance) \
     VALUES (?,?,?,?) ON CONFLICT DO NOTHING"

let select_cycle_delegators =
  Caqti_request.Infix.(
    Caqti_type.(t2 Type.public_key_hash int32 ->* t2 string int64))
    "SELECT delegator, delegated_balance FROM cycle_delegators WHERE baker = ? \
     AND cycle = ?"
