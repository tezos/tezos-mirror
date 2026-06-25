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

(** [backup_post dir chunk] persists the POST that would ship [chunk] (its
    request path and JSON body) as a single record file under [dir], so it can
    be replayed later. Used as the on-failure backup when [--backup-dir] is set;
    handles every chunk kind and never raises. *)
val backup_post : string -> chunk -> unit Lwt.t

(** [set_request_timeout seconds] sets the maximum time to wait for a single
    POST to the teztale-server before giving up (default: 30s). On timeout the
    send is aborted and treated as a failure. *)
val set_request_timeout : float -> unit

(** [replay_backups_loop t dir] periodically re-sends the POST records left in
    [dir] by {!backup_post} and deletes each one once the server accepts it
    (HTTP OK), so the local backup does not grow without bound. Runs forever and
    never raises. *)
val replay_backups_loop : ctx -> string -> unit Lwt.t

include Archiver.S with type t = ctx
