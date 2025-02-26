(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Create a RPC context for calling the rollup node and retrying on connection
    errors. *)
val make_ctxt : Uri.t -> Retrier_context.t

(** Marks the outbox messages executed in [block] on L1 as being executed in the
    database. *)
val mark_executed_outbox_messages :
  Db.t ->
  l1_node_rpc:Retrier_context.t ->
  rollup_address:Octez_smart_rollup.Address.t ->
  block:int32 ->
  unit tzresult Lwt.t

(** Report all withdrawals which are overdue for execution. *)
val check_overdue : Db.t -> l1_node_rpc:Retrier_context.t -> unit tzresult Lwt.t
