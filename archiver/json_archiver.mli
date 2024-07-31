(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Archiver.S with type t = Tezos_client_base.Client_context.full

type chunk =
  | Block of
      Int32.t
      * Block_hash.t
      * Block_hash.t option
      * Int32.t
      * Time.Protocol.t
      * Data.Block.reception list
      * Tezos_crypto.Signature.Public_key_hash.t
      * Data.cycle_info option
      * Consensus_ops.block_op list
      * Data.baking_right list
  | Mempool of bool option * Int32.t (* level *) * Consensus_ops.delegate_ops

val dump : string -> chunk -> unit Lwt.t

type level_file_content = {
  cycle_info : Data.cycle_info option;
  blocks : Data.Block.t list;
  delegate_operations : Data.Delegate_operations.t list;
  baking_rights : Data.baking_right list;
  unaccurate : bool;
}

val level_file_content_encoding : level_file_content Data_encoding.t
