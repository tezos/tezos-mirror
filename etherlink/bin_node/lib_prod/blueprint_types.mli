(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A chunked blueprint, ready to be executed localy. *)
type payload = [`External of string] list

type t = {
  number : Ethereum_types.quantity;
  timestamp : Time.Protocol.t;
  payload : payload;
}

(** Blueprint with events contains: *)
type with_events = {
  delayed_transactions : Ethereum_types.Delayed_transaction.t list;
      (** The delayed transactions to apply before applying the blueprint. *)
  blueprint : t;  (** The blueprint to execute. *)
}

val encoding : t Data_encoding.t

val payload_encoding : payload Data_encoding.t

val with_events_encoding : with_events Data_encoding.t
