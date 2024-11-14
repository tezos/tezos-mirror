(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A [handler] is a function that is called for every blueprint fetched
    from a remote EVM endpoint. *)
type handler =
  Ethereum_types.quantity -> Blueprint_types.with_events -> unit tzresult Lwt.t

(** [start ~time_between_blocks ~evm_node_endpoint ~get_next_blueprint_number k]
    is a never-returning function which iterates over blueprints streamed by
    [evm_node_endpoint], calling [k] for each blueprint in order.

    [next_blueprint_number] is the height of the current local head, while
    [time_between_blocks] is used to detect when a connection to
    [evm_node_endpoint] is stalling. *)
val start :
  time_between_blocks:Configuration.time_between_blocks ->
  evm_node_endpoint:Uri.t ->
  next_blueprint_number:Ethereum_types.quantity ->
  handler ->
  'a tzresult Lwt.t
