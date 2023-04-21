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

(** The collector registry for the rollup node metrics. *)
val sc_rollup_node_registry : Prometheus.CollectorRegistry.t

(** [metrics_server metrics_addr] runs a server for the rollup metrics on [metrics_addr].
    The metrics are accessible thanks to a [/metrics] request. *)
val metrics_serve : string option -> (unit, tztrace) result Lwt.t

(** [print_csv_metrics ppf metrics] prints the [metrics] as CSV. *)
val print_csv_metrics :
  Format.formatter -> 'a Prometheus.MetricFamilyMap.t -> unit

(** The node info metrics *)
module Info : sig
  (** Initializes the metric for rollup info
      with a the given arguments as label values *)
  val init_rollup_node_info :
    id:Tezos_crypto.Hashed.Smart_rollup_address.t ->
    mode:Configuration.mode ->
    genesis_level:int32 ->
    pvm_kind:string ->
    unit
end

(** The metrics related to Inboxes *)
module Inbox : sig
  (** The type of an inbox metrics *)
  type t = {head_inbox_level : Prometheus.Gauge.t}

  (** The stats for the inboxes *)
  module Stats : sig
    (** Set the number of messages from the head *)
    val set : is_internal:('a -> bool) -> 'a list -> unit
  end

  (** The inboxes metrics *)
  val metrics : t
end
