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

module Test : sig
  (** Name of the tps benchmark tezt test. *)
  val benchmark_tps : string

  (** Name of the gas tps tezt test. *)
  val gas_tps : string

  (** Name of the tezt test that performs estimation of the average block. *)
  val estimate_average_block : string
end

module Measurement : sig
  (** Name of the measurement of gas TPS. *)
  val gas_tps_evaluation : string

  (** Name of the measurement of de facto TPS of injection. *)
  val defacto_tps_of_injection : string

  (** Name of the measurement of empirical TPS. *)
  val empirical_tps : string
end

module Tag : sig
  (** The tag that tells whether protocol limited were lifted. *)
  val lifted_protocol_limits : string
end

(** Re-generate TPS evaluation dashboard definition. *)
val update_grafana_dashboard : unit -> unit
