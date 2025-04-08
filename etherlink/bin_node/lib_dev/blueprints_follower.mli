(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** A [handler] is a function that is called for every blueprint fetched
    from a remote EVM endpoint. *)
type handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Restart_from of quantity | `Continue] tzresult Lwt.t

(** [start ~time_between_blocks ~evm_node_endpoint ~get_next_blueprint_number k]
    is a never-returning function which iterates over blueprints streamed by
    [evm_node_endpoint], calling [k] for each blueprint in order.

    [next_blueprint_number] is the height of the current local head, while
    [time_between_blocks] is used to detect when a connection to
    [evm_node_endpoint] is stalling.

    If [ping_tx_pool] is set to [true] (as it is if omitted), then the
    blueprint follower will ping the {!Tx_pool} for every blueprint it
    receives. Conversly, setting it to [false] allows to call [start] without a
    running {!Tx_pool}. *)
val start :
  ?ping_tx_pool:bool ->
  time_between_blocks:Configuration.time_between_blocks ->
  evm_node_endpoint:Uri.t ->
  next_blueprint_number:quantity ->
  handler ->
  'a tzresult Lwt.t
