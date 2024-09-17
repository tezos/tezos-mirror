(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Type : sig
  val time_protocol : Tezos_base.Time.Protocol.t Caqti_type.t

  val block_hash : Tezos_crypto.Hashed.Block_hash.t Caqti_type.t

  val operation_hash : Tezos_crypto.Hashed.Operation_hash.t Caqti_type.t

  val public_key_hash : Tezos_crypto.Signature.public_key_hash Caqti_type.t

  val errors : Tezos_error_monad.TzCore.error list option Caqti_type.t

  val bcrypt_hash : Bcrypt.hash Caqti_type.t
end

val env : Caqti_driver_info.t -> string -> Caqti_query.t

module Mutex : sig
  val delegates : Lwt_mutex.t

  val nodes : Lwt_mutex.t

  val blocks : Lwt_mutex.t

  val blocks_reception : Lwt_mutex.t

  val operations : Lwt_mutex.t

  val operations_reception : Lwt_mutex.t

  val operations_inclusion : Lwt_mutex.t

  val attesting_rights : Lwt_mutex.t

  val cycles : Lwt_mutex.t

  val missing_blocks : Lwt_mutex.t
end

val create_tables : string list

val alter_tables : string list list

val maybe_insert_source : (string, unit, [`Zero]) Caqti_request.t

val maybe_insert_delegate :
  (Tezos_crypto.Signature.public_key_hash, unit, [`Zero]) Caqti_request.t

val maybe_insert_attesting_right :
  ( int32 * int * int * Tezos_crypto.Signature.public_key_hash,
    unit,
    [`Zero] )
  Caqti_request.t

val maybe_insert_operation :
  ( (int32 * Tezos_crypto.Hashed.Operation_hash.t * bool * int32 option)
    * Tezos_crypto.Signature.public_key_hash,
    unit,
    [`Zero] )
  Caqti_request.t

val maybe_insert_block :
  ( (int32 (* level *)
    * Tezos_base.Time.Protocol.t
    * Tezos_crypto.Hashed.Block_hash.t
    * int32 (* round *))
    * (Tezos_crypto.Hashed.Block_hash.t option
      * Tezos_crypto.Signature.public_key_hash),
    unit,
    [`Zero] )
  Caqti_request.t

val maybe_insert_cycle :
  ( int32 (* id *) * int32 (* level *) * int32 (* size *),
    unit,
    [`Zero] )
  Caqti_request.t

val insert_missing_block :
  ( string (* source_name *)
    * int32 (* level *)
    * int32 (* round *)
    * Tezos_crypto.Signature.public_key_hash (* baker *),
    unit,
    [`Zero] )
  Caqti_request.t

val delete_missing_block :
  ( string (* source_name *) * int32 (* level *) * int32 (* round *),
    unit,
    [`Zero] )
  Caqti_request.t

val insert_received_operation :
  ( (Ptime.t
    * Tezos_error_monad.TzCore.error list option
    * Tezos_crypto.Signature.public_key_hash
    * bool)
    * (int32 option * string * int32),
    unit,
    [`Zero] )
  Caqti_request.t

val insert_included_operation :
  ( (Tezos_crypto.Signature.public_key_hash * bool * int32 option)
    * (Tezos_crypto.Hashed.Block_hash.t * int32),
    unit,
    [`Zero] )
  Caqti_request.t

val insert_received_block :
  ( Ptime.t option * Ptime.t option * Tezos_crypto.Hashed.Block_hash.t * string,
    unit,
    [`Zero] )
  Caqti_request.t

val maybe_with_metrics : Config.t -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
