(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type PROTOCOL_SERVICES = sig
  val hash : Protocol_hash.t

  type wrap_full

  val wrap_full : Tezos_client_base.Client_context.full -> wrap_full

  val get_balance_updates :
    wrap_full ->
    Int32.t ->
    (int32
    * int32
    * Block_hash.t
    * Time.Protocol.t
    * Data.Balance_update.balance_update list)
    tzresult
    Lwt.t

  val get_delegators :
    Tezos_client_base.Client_context.full ->
    Int32.t ->
    Signature.Public_key_hash.t ->
    (Signature.Public_key_hash.t * int64) list tzresult Lwt.t
end
