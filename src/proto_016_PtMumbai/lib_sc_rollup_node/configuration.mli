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

(** Mode for the rollup node *)
type mode =
  | Observer  (** Only follows the chain and reconstructs inboxes *)
  | Accuser
      (** Only publishes commitments for conflicts and play refutation games *)
  | Batcher  (** Accept transactions in its queue and batches them on the L1 *)
  | Maintenance  (** Follows the chain and publishes commitments *)
  | Operator  (** Equivalent to maintenance + batcher  *)
  | Custom
      (** This mode allows to tweak which operations are injected by selecting
          the signers *)

(** Purposes for operators, indicating the kind of operations that they sign. *)
type purpose = Publish | Add_messages | Cement | Timeout | Refute

module Operator_purpose_map : Map.S with type key = purpose

type operators = Tezos_crypto.Signature.Public_key_hash.t Operator_purpose_map.t

type fee_parameters = Injector_sigs.fee_parameter Operator_purpose_map.t

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

type t = {
  sc_rollup_address : Sc_rollup_address.t;
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
  batcher : batcher;
  injector : injector;
  l2_blocks_cache_size : int;
  log_kernel_debug : bool;
}

(** [make_purpose_map ~default purposes] constructs a purpose map from a list of
    bindings [purposes], with a potential [default] value. *)
val make_purpose_map :
  default:'a option -> (purpose * 'a) trace -> 'a Operator_purpose_map.t

(** [purpose_of_string s] parses a purpose from the given string [s]. *)
val purpose_of_string : string -> purpose option

(** [string_of_purpose p] returns a string representation of purpose [p]. *)
val string_of_purpose : purpose -> string

(** List of possible purposes for operator specialization. *)
val purposes : purpose list

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

(** [default_fee_parameter ?purpose ()] is the default fee parameter to inject
    operation on L1. If [purpose] is provided, it returns the default fee
    parameter for the specific purpose. *)
val default_fee_parameter :
  ?purpose:purpose -> unit -> Injector_sigs.fee_parameter

(** [default_fee_parameters] is the default fee parameters configuration build
    with {!default_fee_parameter} for all purposes. *)
val default_fee_parameters : fee_parameters

(** [default_batcher] is the default configuration parameters for the batcher. *)
val default_batcher : batcher

(** [default_injector] is the default configuration parameters for the
    injector. *)
val default_injector : injector

(** [default_l2_blocks_cache_size] is the default number of L2 blocks that are
    cached by the rollup node *)
val default_l2_blocks_cache_size : int

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

(** [check_mode config] ensures the operators correspond to the chosen mode and
    removes the extra ones. *)
val check_mode : t -> t tzresult

(** [save ~force ~data_dir configuration] writes the [configuration] file in
    [data_dir]. If [force] is [true], existing configurations are
    overwritten. *)
val save : force:bool -> data_dir:string -> t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t
