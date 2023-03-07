(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(** How to collect and store data *)

(** container of the informations on a data point *)
type datum

(** [make_datum scenario section label ticks time] *)
val make_datum : string -> string -> string -> Z.t -> Measure.time -> datum

(** container of all data point informations collected during benchmark *)
type benchmark

(** initialize en empty benchmark with options
      - verbose: ouput info during execution (besides csv data in the end)
      - totals: add to csv data the total time / tick number for each steps
      - irmin: add data points corresponding to irmin decoding / encoding *)
val empty_benchmark :
  ?verbose:bool -> ?totals:bool -> ?irmin:bool -> unit -> benchmark

(** [init_scenario scenario_name benchmark] inits an empty benchmark
      for a given scenario *)
val init_scenario : string -> benchmark -> benchmark

(** [switch_section section_name benchmark] open a new section*)
val switch_section : string -> benchmark -> benchmark

(** [add_datum name ticks time benchmark] *)
val add_datum : string -> Z.t -> Measure.time -> benchmark -> benchmark

(** [add_tickless_datum label time benchmark]
      adds a point of data for an action consuming no tick *)
val add_tickless_datum : string -> Measure.time -> benchmark -> benchmark

(** adds final info as a data point in the benchmark *)
val add_final_info : Measure.time -> Z.t -> benchmark -> benchmark

module Csv : sig
  (** [print_benchmark filename benchmark] Output benchmark data in CSV format *)
  val pp_benchmark : out_channel -> benchmark -> unit
end

val pp_analysis : Format.formatter -> benchmark -> unit
