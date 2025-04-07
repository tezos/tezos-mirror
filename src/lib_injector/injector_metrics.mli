(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module type P = sig
  module Tag : Injector_sigs.TAG

  val registry : Prometheus.CollectorRegistry.t
end

module Make (P : P) : sig
  (** Set a flag that will let executing the code to produce metrics
      through the wrap function *)
  val produce_metrics : bool -> unit

  (** Wrap metrics code to be executed only if the metrics server is on *)
  val wrap : (unit -> unit) -> unit

  (** Add queue, injected and included gauge for a tag *)
  val add_gauge : int -> P.Tag.t list -> unit

  (** Set the signer's balance. *)
  val set_gauge_signer_balance : P.Tag.t list -> string -> int64 -> unit

  (** Set the operations queue size *)
  val set_queue_size : P.Tag.t list -> int -> unit

  (** Set the injected operations size *)
  val set_injected_operations_size : P.Tag.t list -> int -> unit

  (** Set the included operations size *)
  val set_included_operations_size : P.Tag.t list -> int -> unit
end
