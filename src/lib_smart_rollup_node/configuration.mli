(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Mode for the rollup node *)
type mode =
  | Observer  (** Only follows the chain and reconstructs inboxes *)
  | Accuser
      (** Only publishes commitments for conflicts and play refutation games *)
  | Bailout (* Only defends and cements, does not publish any new commitment *)
  | Batcher  (** Accept transactions in its queue and batches them on the L1 *)
  | Maintenance  (** Follows the chain and publishes commitments *)
  | Operator  (** Equivalent to maintenance + batcher  *)
  | Custom of Operation_kind.t list
      (** In this mode, the system handles only the specific operation kinds
        defined by the user, allowing for tailored control and flexibility. *)

(** Configuration for the batcher.

  Invariants:
  - 0 < [min_batch_size] <= [max_batch_size] <= [protocol_max_batch_size]
  - 0 < [min_batch_elements] <= [max_batch_elements]
*)
type batcher = {
  min_batch_elements : int;
      (** The minimum number elements in a batch for it to be produced when the
          batcher receives new messages. *)
  min_batch_size : int;
      (** The minimum size in bytes of a batch for it to be produced when the
          batcher receives new messages. *)
  max_batch_elements : int;
      (** The maximum number of elements that we can put in a batch. *)
  max_batch_size : int option;  (** The maximum size in bytes of a batch. *)
}

type injector = {
  retention_period : int;
      (** The number of blocks during which the injector will keep track of an
          operation (in addition to the confirmation period). *)
  attempts : int;
      (** The number of attempts that will be made to inject an operation. *)
  injection_ttl : int;
      (** The number of blocks after which an operation that is injected but
          never included is retried. *)
  max_batch_length : int option;
      (** The maximum number of operations to include in a single L1 batch.
          If not specified, batches are limited only by the operation size limit. *)
}

type fee_parameters = Injector_common.fee_parameter Operation_kind.Map.t

type gc_parameters = {
  frequency_in_blocks : int32 option;
      (** Frequency at which the GC is triggered. *)
  context_splitting_period : int option;
      (** Number of blocks before splitting the context. *)
}

type history_mode = Store.State.history_mode =
  | Archive
      (** The whole history of the rollup (starting at its genesis) is kept *)
  | Full
      (** Only the history necessary to play refutation games is kept
          (i.e. after the LCC only) *)

(** Filter on destination of outbox message transactions. *)
type outbox_destination_filter =
  | Any_destination  (** Accept any destination.  *)
  | Destination_among of string list
      (** Accept destination which match the given list (in base58-check). *)

(** Filter on entrypoints of outbox message transactions. *)
type outbox_entrypoint_filter =
  | Any_entrypoint  (** Accept any entrypoint. *)
  | Entrypoint_among of string list  (** Accept entrypoints which are listed. *)

(** Filter on outbox messages executed by the rollup node automatically. *)
type outbox_message_filter =
  | Transaction of {
      destination : outbox_destination_filter;
      entrypoint : outbox_entrypoint_filter;
    }
      (** Accept transactions which match the filter on their destination and
          entrypoint. *)

type t = {
  sc_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  etherlink : bool;
  boot_sector_file : string option;
  operators : Purpose.operators;
  rpc_addr : string;
  rpc_port : int;
  acl : Tezos_rpc_http_server.RPC_server.Acl.policy;
  metrics_addr : string option;
  performance_metrics : bool;
  reconnection_delay : float;
  fee_parameters : fee_parameters;
  mode : mode;
  loser_mode : Loser_mode.t;
  apply_unsafe_patches : bool;
  unsafe_pvm_patches : Pvm_patches.unsafe_patch list;
  (*DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3718
    Decide whether we want to handle connections to multiple
    Dal nodes for different slot indexes.
  *)
  execute_outbox_messages_filter : outbox_message_filter list;
  dal_node_endpoint : Uri.t option;
  pre_images_endpoint : Uri.t option;
  batcher : batcher;
  injector : injector;
  l1_blocks_cache_size : int;
  l2_blocks_cache_size : int;
  prefetch_blocks : int option;
  l1_monitor_finalized : bool;
  l1_rpc_timeout : float;
  loop_retry_delay : float;
      (** Delay in seconds to retry the main loop and the refutation loop after
          an error. *)
  dal_slot_status_max_fetch_attempts : int;
      (** Maximum number of attempts to fetch a finalized DAL slot status
          (i.e. not [Unknown] or awaiting attestation) before giving up.
          Attempts are spaced by 1 second each. Default value is 15. *)
  index_buffer_size : int option;
  irmin_cache_size : int option;
  log_kernel_debug : bool;
  log_kernel_debug_file : string option;
  unsafe_disable_wasm_kernel_checks : bool;
  no_degraded : bool;
  gc_parameters : gc_parameters;
  history_mode : history_mode option;
  cors : Resto_cohttp.Cors.t;
  bail_on_disagree : bool;
  opentelemetry : Octez_telemetry.Opentelemetry_config.t;
}

type error += Empty_operation_kinds_for_custom_mode

(** [history_mode_of_string s] parses a history_mode from the given string
    [s]. *)
val history_mode_of_string : string -> history_mode

(** [string_of_history_mode p] returns a string representation of history_mode
    [p]. *)
val string_of_history_mode : history_mode -> string

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [default_storage_dir] returns the default value of the storage dir
    given a [data_dir]. *)
val default_storage_dir : string -> string

(** [default_context_dir] returns the default value of the directory
    for persisting the context given a [data_dir]. *)
val default_context_dir : string -> string

(** [default_rpc_addr] is the default value for [rpc_addr]. *)
val default_rpc_addr : string

(** [default_rpc_port] is the default value for [rpc_port]. *)
val default_rpc_port : int

(** [default_acl] is the default value for [acl]. *)
val default_acl : Tezos_rpc_http_server.RPC_server.Acl.policy

(** [default_metrics_port] is the default port for the metrics server. *)
val default_metrics_port : int

(** [default_reconnection_delay] is the default value for [reconnection_delay]. *)
val default_reconnection_delay : float

(** [default_fee_parameter operation_kind] is the default fee parameter to inject
    operation on L1. If [operation_kind] is provided, it returns the default fee
    parameter for this kind of operation. *)
val default_fee_parameter : Operation_kind.t -> Injector_common.fee_parameter

(** [default_fee_parameters] is the default fee parameters configuration build
    with {!default_fee_parameter} for all purposes. *)
val default_fee_parameters : fee_parameters

(** [default_batcher] is the default configuration parameters for the batcher. *)
val default_batcher : batcher

(** [default_injector] is the default configuration parameters for the
    injector. *)
val default_injector : injector

(** [default_l1_blocks_cache_size] is the default number of L1 blocks that are
    cached by the rollup node *)
val default_l1_blocks_cache_size : int

(** [default_l2_blocks_cache_size] is the default number of L2 blocks that are
    cached by the rollup node *)
val default_l2_blocks_cache_size : int

(** Default timeout for RPCs to the L1 node. *)
val default_l1_rpc_timeout : float

val default_gc_parameters : gc_parameters

(** [default_history_mode] is the default history mode for the rollup node
    ({!Full}).  *)
val default_history_mode : history_mode

(** Default filter for executing outbox messages is only whitelist updates.  *)
val default_execute_outbox_filter : outbox_message_filter list

(** Default maximum number of attempts to fetch a finalized DAL slot status. *)
val default_dal_slot_status_max_fetch_attempts : int

val history_mode_encoding : history_mode Data_encoding.t

(** [max_injector_retention_period] is the maximum allowed value for
    [injector_retention_period]. *)
val max_injector_retention_period : int

(** This is the list of available modes. *)
val modes : mode list

(** [string_of_mode mode] returns a string representation of the mode
    specified by the argument [mode]. *)
val string_of_mode : mode -> string

(** [mode_of_string s] returns the mode represented by string [s] if it exists. *)
val mode_of_string : string -> mode tzresult

(** [description_of_mode m] returns a textual description of the mode [m]. *)
val description_of_mode : mode -> string

(** [config_filename ~data_dir config] returns the configration filename from
    the [data_dir] when [config] is [None]. *)
val config_filename : data_dir:string -> string option -> string

(** [purposes_of_mode mode] returns purposes associated with the provided mode. *)
val purposes_of_mode : mode -> Purpose.ex_purpose list

(** [operation_kinds_of_mode mode] returns operation kinds with the provided mode. *)
val operation_kinds_of_mode : mode -> Operation_kind.t list

(** [can_inject mode op_kind] determines if a given operation kind can
    be injected based on the configuration settings. *)
val can_inject : mode -> Operation_kind.t -> bool

(** [purposes_matches_mode mode purposes] returns true if and only if the given
    [mode] supports the given [purposes]. *)
val purposes_matches_mode : mode -> 'kind Purpose.t list -> bool

(** Number of levels the refutation player waits until trying to play
    for a game state it already played before. *)
val refutation_player_buffer_levels : int

(* To limit the number of entries stored in the Irmin's LRU cache, it can adjust
   the `lru_size` configuration. By default, it's set to 100_000 entries. Increasing
   this value will consume more memory. Copy from irmin-pack/config.ml *)
val default_irmin_cache_size : int

(** The `default_index_buffer_size` defines the maximum amount of memory
   reserved for caching index entries before they are written to disk.
   Essentially, this cache aids the efficiency of the index.
   The total cache capacity is determined by `index_buffer_size * entry`,
   with each `entry` occupying approximately 56 bytes.
   An `entry` represents a single log record which can encompass various
   details such as a timestamp, message content, severity level, etc. *)
val default_index_buffer_size : int

(** Default setting for monitoring finalized heads of L1 node. *)
val default_l1_monitor_finalized : bool

(** Encoding for configuration. *)
val encoding : t Data_encoding.t

(** Encoding for configuration without any default value. *)
val encoding_no_default : t Data_encoding.t

(** [save ~force ~config_file configuration] writes the [configuration] in file
    [config_file]. If [force] is [true], existing configurations are
    overwritten. *)
val save : force:bool -> config_file:string -> t -> unit tzresult Lwt.t

(** [load ~config_file] reads a configuration from the file [config_file]. *)
val load : config_file:string -> t tzresult Lwt.t

module Cli : sig
  val configuration_from_args :
    rpc_addr:string option ->
    rpc_port:int option ->
    acl_override:[`Allow_all | `Secure] option ->
    metrics_addr:string option ->
    disable_performance_metrics:bool ->
    loser_mode:Loser_mode.t option ->
    reconnection_delay:float option ->
    dal_node_endpoint:Uri.t option ->
    pre_images_endpoint:Uri.t option ->
    injector_retention_period:int option ->
    injector_attempts:int option ->
    injection_ttl:int option ->
    mode:mode ->
    sc_rollup_address:Hashed.Smart_rollup_address.t ->
    boot_sector_file:string option ->
    operators:
      [< `Default of Signature.public_key_hash
      | `Purpose of Purpose.ex_purpose * Signature.public_key_hash ]
      list ->
    index_buffer_size:int option ->
    irmin_cache_size:int option ->
    log_kernel_debug:bool ->
    log_kernel_debug_file:string option ->
    no_degraded:bool ->
    gc_frequency:int32 option ->
    history_mode:history_mode option ->
    allowed_origins:string list option ->
    allowed_headers:string list option ->
    apply_unsafe_patches:bool ->
    unsafe_disable_wasm_kernel_checks:bool ->
    bail_on_disagree:bool ->
    profiling:bool option ->
    force_etherlink:bool ->
    l1_monitor_finalized:bool option ->
    t tzresult Lwt.t

  val create_or_read_config :
    config_file:string ->
    rpc_addr:string option ->
    rpc_port:int option ->
    acl_override:[`Allow_all | `Secure] option ->
    metrics_addr:string option ->
    disable_performance_metrics:bool ->
    loser_mode:Loser_mode.t option ->
    reconnection_delay:float option ->
    dal_node_endpoint:Uri.t option ->
    pre_images_endpoint:Uri.t option ->
    injector_retention_period:int option ->
    injector_attempts:int option ->
    injection_ttl:int option ->
    mode:mode option ->
    sc_rollup_address:Smart_rollup_alias.Address.t option ->
    boot_sector_file:string option ->
    operators:
      [< `Default of Signature.public_key_hash
      | `Purpose of Purpose.ex_purpose * Signature.public_key_hash ]
      list ->
    index_buffer_size:int option ->
    irmin_cache_size:int option ->
    log_kernel_debug:bool ->
    log_kernel_debug_file:string option ->
    no_degraded:bool ->
    gc_frequency:int32 option ->
    history_mode:history_mode option ->
    allowed_origins:string list option ->
    allowed_headers:string list option ->
    apply_unsafe_patches:bool ->
    unsafe_disable_wasm_kernel_checks:bool ->
    bail_on_disagree:bool ->
    profiling:bool option ->
    force_etherlink:bool ->
    l1_monitor_finalized:bool option ->
    t tzresult Lwt.t
end
