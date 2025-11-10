(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** A [new_blueprint_handler] is a function that is called for
    every blueprint fetched from a remote EVM endpoint. *)
type new_blueprint_handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Restart_from of quantity | `Continue] tzresult Lwt.t

(** A [finalized_levels_handler] is a function that is called for
    every finalized l1-l2 levels association fetched from a remote EVM
    endpoint. *)
type finalized_levels_handler =
  l1_level:int32 ->
  start_l2_level:Ethereum_types.quantity ->
  end_l2_level:Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** A [next_block_timestamp_handler] is a function that is called for
    every next block timestamp fetched from a remote EVM. *)
type next_block_timestamp_handler = Time.Protocol.t -> unit tzresult Lwt.t

(** A [next_block_timestamp_handler] is a function that is called for
    every transaction included in the next block fetched from a remote EVM. *)
type inclusion_handler = Broadcast.transaction -> unit tzresult Lwt.t

(** [start ~multichain ~time_between_blocks ~evm_node_endpoint ~rpc_timeout
    ~get_next_blueprint_number on_new_blueprint_handler
    on_finalized_levels_handler]
    is a never-returning function that continuously streams blueprints from
    the given [evm_node_endpoint] and dispatches them to the appropriate handlers.

    - [on_new_blueprint] is called for each new blueprint received, in order.
    - [on_finalized_levels] is called when the remote EVM notifies finalized levels.
    - [on_next_block_timestamp] is called whenever the remote EVM provides
      the timestamp of an upcoming block.
    - [on_inclusion] is called for each transaction included in the next block.

    [next_blueprint_number] is the height of the current local head, while
    [time_between_blocks] is used to detect when a connection to
    [evm_node_endpoint] is stalling.
    [multichain] is used to know where to retrieve the block number. *)
val start :
  multichain:bool ->
  time_between_blocks:Configuration.time_between_blocks ->
  evm_node_endpoint:Uri.t ->
  rpc_timeout:float ->
  next_blueprint_number:quantity ->
  on_new_blueprint:new_blueprint_handler ->
  on_finalized_levels:finalized_levels_handler ->
  on_next_block_timestamp:next_block_timestamp_handler ->
  on_inclusion:inclusion_handler ->
  unit ->
  'a tzresult Lwt.t
