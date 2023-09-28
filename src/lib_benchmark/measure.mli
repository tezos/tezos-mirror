(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type options = {
  seed : int;
  nsamples : int;
  bench_number : int;
  minor_heap_size : [`words of int];
  config_file : string option;
}

val options_encoding : options Data_encoding.t

val pp_options : Format.formatter -> options -> unit

type 'workload measured_workload = {
  workload : 'workload;  (** Workload associated to the measurement *)
  measures : Maths.vector;  (** Collected measurements *)
}

type 'workload workload_data = 'workload measured_workload list

type 'workload measurement = {
  bench_opts : options;
  workload_data : 'workload workload_data;
  date : Unix.tm;
}

type packed_measurement =
  | Measurement : ('a, 't) Benchmark.poly * 't measurement -> packed_measurement

type workloads_stats = {
  max : float;
  min : float;
  mean : float;
  variance : float;
}

val save :
  filename:string ->
  options:options ->
  bench:('c, 't) Benchmark.poly ->
  workload_data:'t workload_data ->
  packed_measurement

val packed_measurement_save_json : packed_measurement -> string option -> unit

val load : filename:string -> packed_measurement

val to_csv :
  filename:string ->
  bench:('c, 't) Benchmark.poly ->
  workload_data:'t workload_data ->
  unit

val perform_benchmark : options -> ('c, 't) Benchmark.poly -> 't workload_data

val make_timing_probe :
  (module Compare.COMPARABLE with type t = 't) -> 't Generator.probe

val get_free_variable_set : packed_measurement -> Free_variable.Set.t

module Time : sig
  (** All return nano seconds *)

  val measure : (unit -> unit) -> float

  val measure_and_return : (unit -> 'a) -> float * 'a

  val measure_lwt : (unit -> 'a Lwt.t) -> (float * 'a) Lwt.t
end
