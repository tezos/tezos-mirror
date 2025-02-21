(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Marks the outbox messages executed in [block] on L1 as being executed in the
    database. *)
val mark_executed_outbox_messages :
  Db.t ->
  l1_node_endpoint:Uri.t ->
  rollup_address:Octez_smart_rollup.Address.t ->
  block:int32 ->
  unit tzresult Lwt.t

(** Report all withdrawals which are overdue for execution. *)
val check_overdue : Db.t -> l1_node_endpoint:Uri.t -> unit tzresult Lwt.t
