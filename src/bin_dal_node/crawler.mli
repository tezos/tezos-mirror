(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** This module maintains information about the layer 1 chain.

   This module follows the evolution of the layer 1 chain by
   subscribing to the head monitoring RPC offered by the Tezos node.
*)

type t

(** [start ~name ~chain ~reconnection_delay  ?protocols
    cctxt] connects to a Tezos node and starts monitoring new
    heads. [reconnection_delay] gives an initial delay for the reconnection
    which is used in an exponential backoff. The [name] is used to differentiate
    events. If [protocols] is provided, only heads of these protocols will be
    monitored. *)
val start :
  name:string ->
  chain:Shell_services.chain ->
  reconnection_delay:float ->
  ?protocols:Protocol_hash.t list ->
  Tezos_rpc.Context.generic ->
  t Lwt.t
