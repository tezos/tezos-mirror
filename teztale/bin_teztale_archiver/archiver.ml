(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  type t

  val name : string

  val launch : t -> string -> unit Lwt.t

  val stop : unit -> unit

  (* [add_received ?unaccurate level ops] adds information about the list of
     received consensus operations [ops], all at level [level]. [unaccurate] is
     true iff the [level] is the same as the current head's level. [ops] is an
     association list of tuples [(delegate, ops)], where [ops] is a list of
     operations all produced by [delegate]. *)
  val add_mempool :
    ?unaccurate:bool -> level:Int32.t -> Consensus_ops.delegate_ops -> unit

  (* [add_block level block_info consensus_ops] adds information about
     a newly received block, like its level, hash, round, its
     timestamp, its reception time, and the included consensus
     operations. *)
  val add_block :
    level:Int32.t ->
    Data.Block.t
    * Data.cycle_info option
    * (Consensus_ops.block_op list * Consensus_ops.block_op list)
    * Data.baking_right list ->
    unit

  val add_rights : level:Int32.t -> Consensus_ops.rights -> unit

  val add_dal_shards : level:Int32.t -> Data.Dal.shard_assignment list -> unit
end
