(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type PROTOCOL_SERVICES = sig
  val hash : Protocol_hash.t

  type wrap_full

  val wrap_full : Tezos_client_base.Client_context.full -> wrap_full

  val attesting_rights :
    wrap_full ->
    reference_level:Int32.t ->
    Int32.t ->
    Consensus_ops.rights tzresult Lwt.t

  type block_id

  module BlockIdMap : Map.S with type key = block_id

  val consensus_operation_stream :
    wrap_full ->
    (((Operation_hash.t
      * ((block_id * Int32.t * Consensus_ops.operation_kind * Int32.t option)
        * int))
     * error trace option)
     Lwt_stream.t
    * Tezos_rpc.Context.stopper)
    tzresult
    Lwt.t

  (* baking_rights _ level round *)
  val baking_rights :
    wrap_full ->
    Int32.t ->
    int ->
    (Tezos_crypto.Signature.public_key_hash * Data.baking_right list) tzresult
    Lwt.t

  (* dal_shards_of _ level *)
  val dal_shards_of :
    wrap_full -> Int32.t -> Data.Dal.shard_assignment list tzresult Lwt.t

  val baker_and_cycle :
    wrap_full ->
    Block_hash.t ->
    (Tezos_crypto.Signature.public_key_hash * Data.cycle_info) tzresult Lwt.t

  val block_round : Block_header.t -> int tzresult

  val consensus_ops_info_of_block :
    wrap_full -> Block_hash.t -> Consensus_ops.block_op list tzresult Lwt.t

  val get_block_info :
    wrap_full ->
    Int32.t ->
    ((Tezos_crypto.Signature.public_key_hash
     * Time.Protocol.t
     * int
     * Block_hash.t
     * Block_hash.t option)
    * Data.cycle_info)
    tzresult
    Lwt.t
end
