(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type sbl_callbacks_activated = {sbl_callbacks_activated : bool}

(** A [new_blueprint_handler] is a function that is called for
    every blueprint fetched from a remote EVM endpoint. *)
type new_blueprint_handler =
  quantity ->
  Blueprint_types.with_events ->
  [`Restart_from of quantity | `Continue of sbl_callbacks_activated] tzresult
  Lwt.t

(** A [finalized_levels_handler] is a function that is called for
    every finalized l1-l2 levels association fetched from a remote EVM
    endpoint. *)
type finalized_levels_handler =
  l1_level:int32 ->
  start_l2_level:Ethereum_types.quantity ->
  end_l2_level:Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** A [next_block_info_handler] is a function that is called for
    every next block informations (timestamp + number) fetched from a remote EVM. *)
type next_block_info_handler =
  Time.Protocol.t -> Ethereum_types.quantity -> unit tzresult Lwt.t

(** A [next_block_timestamp_handler] is a function that is called for
    every transaction included in the next block fetched from a remote EVM. *)
type inclusion_handler =
  Broadcast.transaction -> Ethereum_types.hash -> unit tzresult Lwt.t

(** A [dropped_handler] is a function that is called for
    every dropped transaction fetched from a remote EVM. *)
type dropped_handler = Ethereum_types.hash -> string -> unit tzresult Lwt.t

(** [start ~multichain ~time_between_blocks ~evm_node_endpoint ~rpc_timeout
    ~get_next_blueprint_number on_new_blueprint_handler
    on_finalized_levels_handler]
    is a never-returning function that continuously streams blueprints from
    the given [evm_node_endpoint] and dispatches them to the appropriate handlers.

    - [on_new_blueprint] is called for each new blueprint received, in order.
    - [on_finalized_levels] is called when the remote EVM notifies finalized levels.
    - [on_next_block_info] is called whenever the remote EVM provides
      the informations of an upcoming block.
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
  on_next_block_info:next_block_info_handler ->
  on_inclusion:inclusion_handler ->
  on_dropped:dropped_handler ->
  unit ->
  'a tzresult Lwt.t
