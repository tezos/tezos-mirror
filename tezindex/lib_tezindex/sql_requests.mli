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
  val balance_updates : Lwt_mutex.t
end

val create_tables : string list

val insert_block_balance_update :
  ( (int32 * int32 * Tezos_crypto.Signature.public_key_hash)
    * (string * string * int64),
    unit,
    [`Zero] )
  Caqti_request.t

val select_cycle_balance_updates :
  ( Tezos_crypto.Signature.public_key_hash * int32,
    string * string * int64,
    [`Many | `One | `Zero] )
  Caqti_request.t
