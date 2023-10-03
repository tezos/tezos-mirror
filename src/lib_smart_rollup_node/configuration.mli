(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** The kind of operations that can be injected by the rollup node. *)
type operation_kind =
  | Publish
  | Add_messages
  | Cement
  | Timeout
  | Refute
  | Recover
  | Execute_outbox_message

(** Mode for the rollup node *)
type mode =
  | Observer  (** Only follows the chain and reconstructs inboxes *)
  | Accuser
      (** Only publishes commitments for conflicts and play refutation games *)
  | Bailout (* Only defends and cements, does not publish any new commitment *)
  | Batcher  (** Accept transactions in its queue and batches them on the L1 *)
  | Maintenance  (** Follows the chain and publishes commitments *)
  | Operator  (** Equivalent to maintenance + batcher  *)
  | Custom of operation_kind list
      (** In this mode, the system handles only the specific operation kinds
        defined by the user, allowing for tailored control and flexibility. *)

(** Purposes for operators, indicating their role and thus the kinds of
    operations that they sign. *)
type purpose =
  | Operating
  | Batching
  | Cementing
  | Recovering
  | Executing_outbox

module Operation_kind_map : Map.S with type key = operation_kind

module Operator_purpose_map : Map.S with type key = purpose

type operators = Signature.Public_key_hash.t Operator_purpose_map.t

type fee_parameters = Injector_sigs.fee_parameter Operation_kind_map.t

(** Configuration for the batcher.

  Invariants:
  - 0 < [min_batch_size] <= [max_batch_size] <= [protocol_max_batch_size]
  - 0 < [min_batch_elements] <= [max_batch_elements]
*)
type batcher = {
  simulate : bool;
      (** If [true], the batcher will simulate the messages it receives, in an
          incremental context, before queuing them. *)
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
}

type gc_parameters = {frequency_in_blocks : int32}

type history_mode =
  | Archive
      (** The whole history of the rollup (starting at its genesis) is kept *)
  | Full
      (** Only the history necessary to play refutation games is kept
          (i.e. after the LCC only) *)

type t = {
  sc_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  boot_sector_file : string option;
  sc_rollup_node_operators : operators;
  rpc_addr : string;
  rpc_port : int;
  metrics_addr : string option;
  reconnection_delay : float;
  fee_parameters : fee_parameters;
  mode : mode;
  loser_mode : Loser_mode.t;
  (*DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3718
    Decide whether we want to handle connections to multiple
    Dal nodes for different slot indexes.
  *)
  dal_node_endpoint : Uri.t option;
  dac_observer_endpoint : Uri.t option;
  dac_timeout : Z.t option;
  batcher : batcher;
  injector : injector;
  l1_blocks_cache_size : int;
  l2_blocks_cache_size : int;
  prefetch_blocks : int option;
  index_buffer_size : int option;
  irmin_cache_size : int option;
  log_kernel_debug : bool;
  no_degraded : bool;
  gc_parameters : gc_parameters;
  history_mode : history_mode;
  cors : Resto_cohttp.Cors.t;
}

type error +=
  | Missing_mode_operators of {mode : string; missing_operators : string list}
  | Empty_operation_kinds_for_custom_mode

(** [make_purpose_map ~default purposes] constructs a purpose map from a list of
    bindings [purposes], with a potential [default] value. *)
val make_purpose_map :
  default:'a option -> (purpose * 'a) trace -> 'a Operator_purpose_map.t

(** [operation_kind_of_string s] parses an operation kind from the given string [s]. *)
val operation_kind_of_string : string -> operation_kind option

(** [string_of_operation_kind o] returns a string representation of operation_kind [o]. *)
val string_of_operation_kind : operation_kind -> string

(** [purpose_of_string s] parses a purpose from the given string [s]. *)
val purpose_of_string : string -> purpose option

(** [string_of_purpose p] returns a string representation of purpose [p]. *)
val string_of_purpose : purpose -> string

(** [history_mode_of_string s] parses a history_mode from the given string
    [s]. *)
val history_mode_of_string : string -> history_mode

(** [string_of_history_mode p] returns a string representation of history_mode
    [p]. *)
val string_of_history_mode : history_mode -> string

(** List of possible purposes for operator specialization. *)
val purposes : purpose list

(** List of possible operations kind for operator specialization. *)
val operation_kinds : operation_kind list

(* For each purpose, it returns a list of associated operation kinds. *)
val operation_kinds_of_purpose : purpose -> operation_kind list

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

(** [default_metrics_port] is the default port for the metrics server. *)
val default_metrics_port : int

(** [default_reconnection_delay] is the default value for [reconnection_delay]. *)
val default_reconnection_delay : float

(** [default_fee_parameter ?operation_kind ()] is the default fee parameter to inject
    operation on L1. If [operation_kind] is provided, it returns the default fee
    parameter for this kind of operation. *)
val default_fee_parameter :
  ?operation_kind:operation_kind -> unit -> Injector_sigs.fee_parameter

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

val default_gc_parameters : gc_parameters

(** [default_history_mode] is the default history mode for the rollup node
    ({!Full}).  *)
val default_history_mode : history_mode

val history_mode_encoding : history_mode Data_encoding.t

(** [max_injector_retention_period] is the maximum allowed value for
    [injector_retention_period]. *)
val max_injector_retention_period : int

(** This is the list of available modes. *)
val modes : mode list

(** [string_of_mode mode] returns a string representation of the mode [mode]. *)
val string_of_mode : mode -> string

(** [mode_of_string s] returns the mode represented by string [s] if it exists. *)
val mode_of_string : string -> mode tzresult

(** [description_of_mode m] returns a textual description of the mode [m]. *)
val description_of_mode : mode -> string

(** [config_filename data_dir] returns
    the configration filename from the [data_dir] *)
val config_filename : data_dir:string -> string

(** [purposes_of_operation_kinds op_kinds] map a list of operation kinds
    to their corresponding purposes based on their presence in the input list *)
val purposes_of_operation_kinds : operation_kind list -> purpose list

(** [check_mode config] ensures the operators correspond to the chosen mode and
    removes the extra ones. *)
val check_mode : t -> t tzresult

(** [purposes_of_mode mode] returns purposes associated with the provided mode. *)
val purposes_of_mode : mode -> purpose list

(** [operation_kinds_of_mode mode] returns operation kinds with the provided mode. *)
val operation_kinds_of_mode : mode -> operation_kind list

(** [can_inject mode op_kind] determines if a given operation kind can
    be injected based on the configuration settings. *)
val can_inject : mode -> operation_kind -> bool

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

(** [save ~force ~data_dir configuration] writes the [configuration] file in
    [data_dir]. If [force] is [true], existing configurations are
    overwritten. *)
val save : force:bool -> data_dir:string -> t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t

module Cli : sig
  val configuration_from_args :
    rpc_addr:string option ->
    rpc_port:int option ->
    metrics_addr:string option ->
    loser_mode:Loser_mode.t option ->
    reconnection_delay:float option ->
    dal_node_endpoint:Uri.t option ->
    dac_observer_endpoint:Uri.t option ->
    dac_timeout:Z.t option ->
    injector_retention_period:int option ->
    injector_attempts:int option ->
    injection_ttl:int option ->
    mode:mode ->
    sc_rollup_address:Hashed.Smart_rollup_address.t ->
    boot_sector_file:string option ->
    sc_rollup_node_operators:
      [< `Default of Signature.public_key_hash
      | `Purpose of purpose * Signature.public_key_hash ]
      trace ->
    index_buffer_size:int option ->
    irmin_cache_size:int option ->
    log_kernel_debug:bool ->
    no_degraded:bool ->
    gc_frequency:int32 option ->
    history_mode:history_mode option ->
    allowed_origins:string list option ->
    allowed_headers:string list option ->
    t tzresult

  val create_or_read_config :
    data_dir:string ->
    rpc_addr:string option ->
    rpc_port:int option ->
    metrics_addr:string option ->
    loser_mode:Loser_mode.t option ->
    reconnection_delay:float option ->
    dal_node_endpoint:Uri.t option ->
    dac_observer_endpoint:Uri.t option ->
    dac_timeout:Z.t option ->
    injector_retention_period:int option ->
    injector_attempts:int option ->
    injection_ttl:int option ->
    mode:mode option ->
    sc_rollup_address:Smart_rollup_alias.Address.t option ->
    boot_sector_file:string option ->
    sc_rollup_node_operators:
      [< `Default of Signature.public_key_hash
      | `Purpose of purpose * Signature.public_key_hash ]
      list ->
    index_buffer_size:int option ->
    irmin_cache_size:int option ->
    log_kernel_debug:bool ->
    no_degraded:bool ->
    gc_frequency:int32 option ->
    history_mode:history_mode option ->
    allowed_origins:string list option ->
    allowed_headers:string list option ->
    t tzresult Lwt.t
end
