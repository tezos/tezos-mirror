(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

val register_tezlink_services :
  l2_chain_id:L2_types.chain_id ->
  add_operation:(bytes -> Ethereum_types.hash tzresult Lwt.t) ->
  get_da_fee_per_byte_nanotez:(unit -> Q.t tzresult Lwt.t) ->
  (module Tezlink_backend_sig.S) ->
  unit Tezos_rpc.Directory.t

(** [protocol_for_block_or_level ~allowing_mock block_result]
    returns the protocol and next protocol modules for the given block.
    When [allowing_mock] is true, levels 0 and 1 return the zero and
    genesis protocol modules respectively. *)
val protocol_for_block_or_level :
  allowing_mock:bool ->
  L2_types.Tezos_block.t tzresult ->
  (module Tezos_services.Tezlink_protocol)
  * (module Tezos_services.Tezlink_protocol)
