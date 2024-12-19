(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A chunk of a blueprint *)
type chunk = [`External of string]

(** A chunked blueprint, ready to be executed localy. *)
type payload = chunk list

type t = {
  number : Ethereum_types.quantity;
  timestamp : Time.Protocol.t;
  payload : payload;
}

(** Blueprint with events contains: *)
type with_events = {
  delayed_transactions : Evm_events.Delayed_transaction.t list;
      (** The delayed transactions to apply before applying the blueprint. *)
  kernel_upgrade : Evm_events.Upgrade.t option;
  blueprint : t;  (** The blueprint to execute. *)
}

val encoding : t Data_encoding.t

val payload_encoding : payload Data_encoding.t

val with_events_encoding : with_events Data_encoding.t

val with_events_equal : with_events -> with_events -> bool

val events_of_blueprint_with_events : with_events -> Evm_events.t list
