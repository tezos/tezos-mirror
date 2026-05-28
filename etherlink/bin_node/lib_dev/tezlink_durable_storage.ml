(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

(** Thin readers on top of [Durable_storage] for the Michelson-runtime
    implicit-account subtree (balance / manager / counter), plus the
    DA-fee mutez helper. The Originated/Implicit dispatch and the
    encoding/decoding live in the resolver of the underlying typed
    constructors. *)

let balance state c =
  Durable_storage.read_or_default ~default:Tez.zero (Tezlink_balance c) state

let balance_z state c =
  let open Lwt_result_syntax in
  let* b = balance state c in
  return @@ Tez.to_mutez_z b

let manager state pkh =
  Durable_storage.read_or_default ~default:None (Tezlink_manager pkh) state

let counter state pkh =
  Durable_storage.read_or_default ~default:None (Tezlink_counter pkh) state

let da_fee_per_byte_mutez state =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty da_fee_per_byte_wei) =
    Durable_storage.read Da_fee_per_byte state
  in
  let*? da_fee_per_byte = Tezos_types.Tez.(of_wei (Wei da_fee_per_byte_wei)) in
  (* DA fee expressed in wei: converting to mutez. *)
  return da_fee_per_byte
