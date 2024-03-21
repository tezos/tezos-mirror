(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type init_status = Loaded | Created

type head = {
  current_block_hash : Ethereum_types.block_hash;
  next_blueprint_number : Ethereum_types.quantity;
}

(** [start ~data_dir ~preimages ~preimages_endpoint ~smart_rollup_address ()]
    creates a new worker to manage a local EVM context where it initializes the
    {!type-index}, and use a checkpoint mechanism to load the latest
    {!type-store} if any.

    Returns a value telling if the context was loaded from disk
    ([Loaded]) or was initialized from scratch ([Created]). *)
val start :
  ?kernel_path:string ->
  data_dir:string ->
  preimages:string ->
  preimages_endpoint:Uri.t option ->
  smart_rollup_address:string ->
  unit ->
  init_status tzresult Lwt.t

(** [init_from_rollup_node ~data_dir
    ~rollup_node_data_dir ~inspect_current_blueprint_number]
    initialises the irmin context and metadata of the evm using the
    latest known evm state of the given rollup
    node. *)
val init_from_rollup_node :
  data_dir:string -> rollup_node_data_dir:string -> unit tzresult Lwt.t

(** [apply_evm_events ~finalized_level events] applies all the
    events [events] on the local context. The events are performed in a
    transactional context. *)
val apply_evm_events :
  finalized_level:int32 ->
  Ethereum_types.Evm_events.t list ->
  unit tzresult Lwt.t

(** [inspect ctxt path] returns the value stored in [path] of the freshest EVM
    state, if it exists. *)
val inspect : string -> bytes option tzresult Lwt.t

(** [execute_and_inspect ~input ctxt] executes [input] using the freshest EVM
    state, and returns [input.insights_requests].

    If [wasm_entrypoint] is omitted, the [kernel_run] function of the kernel is
    executed. *)
val execute_and_inspect :
  ?wasm_entrypoint:string ->
  Simulation.Encodings.simulate_input ->
  bytes option list tzresult Lwt.t

(** [last_produced_blueprint ctxt] returns the pair of publishable and
    executable blueprints used to create the current head of the chain. *)
val last_produced_blueprint : unit -> Blueprint_types.t tzresult Lwt.t

(** [apply_blueprint ctxt blueprint] applies [blueprint] in the freshest EVM
    state stored under [ctxt]. It commits the result if the blueprint produces
    the expected block. *)
val apply_blueprint :
  Time.Protocol.t -> Blueprint_types.payload -> unit tzresult Lwt.t

(** Same as {!apply_blueprint}, but additionally publish the blueprint if it is
    correct. *)
val apply_sequencer_blueprint :
  Time.Protocol.t -> Sequencer_blueprint.t -> unit tzresult Lwt.t

val head_info : unit -> head tzresult Lwt.t

val blueprints_watcher :
  unit -> Blueprint_types.t Lwt_stream.t * Lwt_watcher.stopper

val executable_blueprint :
  Ethereum_types.quantity -> Blueprint_types.t option tzresult Lwt.t

val publishable_blueprints_range :
  Ethereum_types.quantity ->
  Ethereum_types.quantity ->
  (Ethereum_types.quantity * Blueprint_types.payload) list tzresult Lwt.t

val last_known_l1_level : unit -> int32 option tzresult Lwt.t

val new_last_known_l1_level : int32 -> unit tzresult Lwt.t

val shutdown : unit -> unit Lwt.t
