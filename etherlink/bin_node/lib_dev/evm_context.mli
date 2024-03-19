(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type session_state = {
  mutable context : Irmin_context.rw;  (** Irmin read and write context. *)
  mutable next_blueprint_number : Ethereum_types.quantity;
      (** Number for the next bluerpint to be produced. *)
  mutable current_block_hash : Ethereum_types.block_hash;
      (** Hash of the latest processed block *)
  mutable pending_upgrade : Ethereum_types.Upgrade.t option;
}

type t = {
  data_dir : string;  (** Data dir of the EVM node. *)
  index : Irmin_context.rw_index;
  preimages : string;  (** Path to the preimages directory. *)
  preimages_endpoint : Uri.t option;  (** URI to fetch missing pre-images. *)
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  blueprint_watcher : Blueprint_types.t Lwt_watcher.input;
  store : Store.t;
  session : session_state;
  head_lock : Lwt_mutex.t;
      (** Lock to acquire to modify the head of the chain *)
}

(** [init ~data_dir ~preimages ~preimages_endpoint ~smart_rollup_address ()]
    creates a context where it initializes the {!type-index}, and use a
    checkpoint mechanism to load the latest {!type-store} if any.

    Returns an additional boolean telling if the context was loaded from disk
    ([true]) or was initialized from scratch ([false]). *)
val init :
  ?kernel_path:string ->
  data_dir:string ->
  preimages:string ->
  preimages_endpoint:Uri.t option ->
  smart_rollup_address:string ->
  unit ->
  (t * bool) tzresult Lwt.t

(** [init_from_rollup_node ~data_dir
    ~rollup_node_data_dir ~inspect_current_blueprint_number]
    initialises the irmin context and metadata of the evm using the
    latest known evm state of the given rollup
    node. *)
val init_from_rollup_node :
  data_dir:string -> rollup_node_data_dir:string -> unit tzresult Lwt.t

(** [apply_evm_events ~finalized_level ctxt events] applies all the
    events [events] on the local context [ctxt]. The events are
    performed in a transactional context. *)
val apply_evm_events :
  finalized_level:int32 ->
  t ->
  Ethereum_types.Evm_events.t list ->
  unit tzresult Lwt.t

(** [inspect ctxt path] returns the value stored in [path] of the freshest EVM
    state, if it exists. *)
val inspect : t -> string -> bytes option Lwt.t

(** [execute_and_inspect ~input ctxt] executes [input] using the freshest EVM
    state, and returns [input.insights_requests].

    If [wasm_entrypoint] is omitted, the [kernel_run] function of the kernel is
    executed. *)
val execute_and_inspect :
  ?wasm_entrypoint:string ->
  input:Simulation.Encodings.simulate_input ->
  t ->
  bytes option list tzresult Lwt.t

(** [last_produced_blueprint ctxt] returns the pair of publishable and
    executable blueprints used to create the current head of the chain. *)
val last_produced_blueprint : t -> Blueprint_types.t tzresult Lwt.t

(** [apply_blueprint ctxt blueprint] applies [blueprint] in the freshest EVM
    state stored under [ctxt]. It commits the result if the blueprint produces
    the expected block. *)
val apply_blueprint :
  t -> Time.Protocol.t -> Blueprint_types.payload -> unit tzresult Lwt.t

(** Same as {!apply_blueprint}, but additionally publish the blueprint if it is
    correct. *)
val apply_and_publish_blueprint :
  t -> Time.Protocol.t -> Sequencer_blueprint.t -> unit tzresult Lwt.t
