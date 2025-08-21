(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Octez_smart_rollup

(** Create a RPC context for calling the rollup node and retrying on connection
    errors. *)
val make_ctxt : rollup_node_endpoint:Uri.t -> Retrier_context.t

type outbox_message = {
  outbox_level : int32;
  message_index : int;
  transactions : Outbox_message.transaction_summary list;
}

(** Returns the outbox messages for a given outbox level known by the rollup
    node. *)
val get_outbox_messages :
  #Tezos_rpc.Context.simple ->
  outbox_level:int32 ->
  outbox_message list tzresult Lwt.t

(** Returns last cemented commitment of the rollup from the rollup node. *)
val get_lcc : #Tezos_rpc.Context.simple -> int32 tzresult Lwt.t

(** Returns the rollup address which the rollup node handles. *)
val get_rollup_address : #Tezos_rpc.Context.simple -> Address.t tzresult Lwt.t
