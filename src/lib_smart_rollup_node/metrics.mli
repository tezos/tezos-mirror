(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** A description of the metrics exported by the node. *)
val listing : disable_performance_metrics:bool -> string Lwt.t

(** The collector registry for the rollup node metrics. *)
val sc_rollup_node_registry : Prometheus.CollectorRegistry.t

(** Enables the wrapper variable *)
val active_metrics : Configuration.t -> unit

(** Wrap a function to be processed if a metrics address is set up in the configuration. *)
val wrap : (unit -> unit) -> unit

val wrap_lwt :
  (unit -> (unit, 'error) result Lwt.t) -> (unit, 'error) result Lwt.t

(** [metrics_server metrics_addr] runs a server for the rollup metrics on [metrics_addr].
    The metrics are accessible thanks to a [/metrics] request. *)
val metrics_serve : string option -> (unit, tztrace) result Lwt.t

(** [print_csv_metrics ppf metrics] prints the [metrics] as CSV. *)
val print_csv_metrics :
  Format.formatter -> 'a Prometheus.MetricFamilyMap.t -> unit

module Refutation : sig
  type state = OurTurn | TheirTurn | Timeout

  (** Set the number of current conflict for this rollup node *)
  val set_number_of_conflict : int -> unit

  (** Set the state of a refutation game whether it's our turn
      or the opponent *)
  val set_state_refutation_game : ?labels:string list -> state -> unit

  (** Set the number of block before the player timeout in the game *)
  val set_block_timeout : ?labels:string list -> int -> unit

  (** Clear the state of a refutation game *)
  val clear_state_refutation_game : string list -> unit
end

(** The node info metrics *)
module Info : sig
  (** Initializes the metric for rollup info
      with a the given arguments as label values *)
  val init_rollup_node_info :
    Configuration.t ->
    genesis_level:int32 ->
    genesis_hash:Commitment.Hash.t ->
    pvm_kind:string ->
    history_mode:Configuration.history_mode ->
    unit

  (** Set the last cemented commitment level available locally *)
  val set_lcc_level_local : int32 -> unit

  (** Set the last cemented commitment level seen on the network *)
  val set_lcc_level_l1 : int32 -> unit

  (** Set the last published commitment level available locally *)
  val set_lpc_level_local : int32 -> unit

  (** Set the last published commitment level seen on the network *)
  val set_lpc_level_l1 : int32 -> unit

  (** Set the protocol specific information for the current protocol *)
  val set_proto_info :
    Protocol_hash.t -> Rollup_constants.protocol_constants -> unit
end

(** The metrics related to Inboxes *)
module Inbox : sig
  (** Set the level of the head *)
  val set_head_level : int32 -> unit

  (** Set the number of messages from the head *)
  val set_messages : is_internal:('a -> bool) -> 'a list -> unit

  (** Set the time the rollup node used to process the inbox *)
  val set_process_time : Ptime.span -> unit

  (** Set the time the rollup node used to fetch the inbox *)
  val set_fetch_time : Ptime.span -> unit

  (** Set the time the rollup node used to handle the inbox *)
  val set_total_time : Ptime.span -> unit
end

module Batcher : sig
  (** Set the time to retrieve the batches *)
  val set_get_time : Ptime.span -> unit

  (** Set the time to inject the batches *)
  val set_inject_time : Ptime.span -> unit

  (** Set the number of messages in the queue *)
  val set_messages_queue_size : int -> unit

  (** Set the number of messages batched *)
  val set_messages_size : int -> unit

  (** Set the number of batches sent *)
  val set_batches_size : int -> unit

  (** Set the level of the last batch *)
  val set_last_batch_level : int32 -> unit

  (** Set the time of the last batch *)
  val set_last_batch_time : Ptime.t -> unit
end

module DAL_batcher : sig
  (** Set the number of messages in the queue waiting to be published on the DAL *)
  val set_dal_batcher_queue_length : int -> unit

  (** Set the number of DAL slots which were recently published and not yet forgotten *)
  val set_dal_injections_queue_length : int -> unit
end

module GC : sig
  (** Set the time to process a GC *)
  val set_process_time : Ptime.span -> unit

  (** Set the oldest available level after a GC *)
  val set_oldest_available_level : int32 -> unit
end

module type PERFORMANCE = sig
  val set_stats : data_dir:string -> unit Lwt.t
end

val performance_metrics : (module PERFORMANCE) Lazy.t
