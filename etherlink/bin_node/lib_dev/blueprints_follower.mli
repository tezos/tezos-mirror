(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** A [on_new_blueprint_handler] is a function that is called for
    every blueprint fetched from a remote EVM endpoint. *)
type on_new_blueprint_handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Restart_from of quantity | `Continue] tzresult Lwt.t

(** A [on_finalized_levels_handler] is a function that is called for
    every finalized l1-l2 levels association fetched from a remote EVM
    endpoint. *)
type on_finalized_levels_handler =
  l1_level:int32 ->
  start_l2_level:Ethereum_types.quantity ->
  end_l2_level:Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** [start ~multichain ~time_between_blocks ~evm_node_endpoint
    ~get_next_blueprint_number on_new_blueprint_handler
    on_finalized_levels_handler] is a never-returning function which
    iterates over blueprints streamed by [evm_node_endpoint], calling
    [on_new_blueprint_handler] for each blueprint in order and
    [on_finalized_levels_handler] for each finalized levels notified
    by the remote EVM endpoint.

    [next_blueprint_number] is the height of the current local head, while
    [time_between_blocks] is used to detect when a connection to
    [evm_node_endpoint] is stalling.
    [multichain] is used to know where to retrieve the block number.

    If [ping_tx_pool] is set to [true] (as it is if omitted), then the
    blueprint follower will ping the {!Tx_pool} for every blueprint it
    receives. Conversely, setting it to [false] allows to call [start] without a
    running {!Tx_pool}. *)
val start :
  ?ping_tx_pool:bool ->
  multichain:bool ->
  time_between_blocks:Configuration.time_between_blocks ->
  evm_node_endpoint:Uri.t ->
  next_blueprint_number:quantity ->
  on_new_blueprint:on_new_blueprint_handler ->
  on_finalized_levels:on_finalized_levels_handler ->
  unit ->
  'a tzresult Lwt.t
