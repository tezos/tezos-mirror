(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  data_dir : string;  (** Data dir of the EVM node. *)
  context : Irmin_context.rw;  (** Irmin read and write context. *)
  kernel : string;  (** Path to the kernel to execute. *)
  preimages : string;  (** Path to the preimages directory. *)
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
      (** Number for the next bluerpint to be produced. *)
  blueprint_watcher : Blueprint_types.t Lwt_watcher.input;
}

(** [init ~data_dir ~kernel ~preimages ~smart_rollup_address ()] creates
    a context where it initializes the {!type-index}, and use a
    checkpoint mechanism to load the latest {!type-store} if any.

    If the context does not already exist and if [produce_genesis_with] is set,
    this function also produces and publishes the genesis blueprint (optionally
    set to [genesis_timestamp]). *)
val init :
  ?genesis_timestamp:Time.Protocol.t ->
  ?produce_genesis_with:Signature.secret_key ->
  data_dir:string ->
  kernel:string ->
  preimages:string ->
  smart_rollup_address:string ->
  unit ->
  t tzresult Lwt.t

(** [init_from_rollup_node ~data_dir
    ~rollup_node_data_dir ~inspect_current_blueprint_number]
    initialises the irmin context and metadata of the evm using the
    latest known evm state of the given rollup
    node. *)
val init_from_rollup_node :
  data_dir:string -> rollup_node_data_dir:string -> unit tzresult Lwt.t

(** [commit ctxt evm_state] updates the [evm_state] in [ctxt], commits
    to disk the changes, and update the checkpoint. *)
val commit : t -> Evm_state.t -> t tzresult Lwt.t

(** [sync ctxt] synchronizes the [ctxt] based on on-disk information, loads the
    latest checkpoint. *)
val sync : t -> t tzresult Lwt.t

(** [evm_state ctxt] returns the freshest EVM state stored under [ctxt]. *)
val evm_state : t -> Evm_state.t Lwt.t

(** [execute ?commit ctxt messages] executes [messages] on the freshest
    EVM state stored in [ctxt].

    If [commit = true], the resulting EVM state is committed in [ctxt] (that
    is, it becomes the freshest one). *)
val execute :
  ?commit:bool ->
  t ->
  [< `Input of string] list ->
  (t * Evm_state.t) tzresult Lwt.t

(** [execute_and_inspect ~input ctxt] executes [input] using the freshest EVM
    state, and returns [input.insights_requests]. *)
val execute_and_inspect :
  input:Simulation.Encodings.simulate_input ->
  t ->
  bytes option list tzresult Lwt.t

val find_blueprint :
  t -> Ethereum_types.quantity -> Blueprint_types.payload option Lwt.t

val last_produced_blueprint : t -> Blueprint_types.t tzresult Lwt.t

(** [apply_blueprint ctxt blueprint] applies [blueprint] in the freshest EVM
    state stored under [ctxt]. It commits the result if the blueprint produces
    the expected block. *)
val apply_blueprint : t -> Sequencer_blueprint.t -> t tzresult Lwt.t
