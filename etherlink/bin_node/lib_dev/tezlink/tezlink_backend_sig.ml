(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  type block_param = [`Head]

  val current_level :
    [`Main] -> block_param -> offset:int32 -> Tezos_types.level tzresult Lwt.t

  val constants : [`Main] -> block_param -> Tezlink_constants.t tzresult Lwt.t

  val balance :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Tezos_types.Tez.t tzresult Lwt.t

  val manager_key :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Signature.V1.Public_key.t option tzresult Lwt.t

  val counter :
    [`Main] -> block_param -> Tezos_types.Contract.t -> Z.t tzresult Lwt.t

  val header : [`Main] -> block_param -> L2_types.Tezos_block.t tzresult Lwt.t
end
