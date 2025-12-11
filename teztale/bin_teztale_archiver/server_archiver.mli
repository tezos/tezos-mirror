(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** For efficiency reason, the authentification part is extracted once
    for all *)
type endpoint = {auth : string * string; endpoint : Uri.t}

type chunk =
  | Block of
      Int32.t (* level *)
      * (Data.Block.t
        * Data.cycle_info option
        * (Consensus_ops.block_op list (* endos *)
          * Consensus_ops.block_op list (* preendos *))
        * Data.baking_right list)
  | Mempool of Int32.t (* level *) * Consensus_ops.delegate_ops
  | Rights of (Int32.t (* level *) * Consensus_ops.rights)
  | Dal_shards of Int32.t (* level *) * Data.Dal.shard_assignment list

type ctx = {
  cohttp_ctx : Cohttp_lwt_unix.Net.ctx;
  endpoints : endpoint list;
  backup : chunk -> unit Lwt.t;
}

val extract_auth : Uri.t -> endpoint

include Archiver.S with type t = ctx
