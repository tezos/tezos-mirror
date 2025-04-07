(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

module Benchmark_base = Benchmark

module Benchmark : sig
  type group = Benchmark_base.group = Standalone | Group of string | Generic

  type purpose = Benchmark_base.purpose =
    | Other_purpose of string
    | Generate_code of string

  (** The module type of benchmarks, a simplification of {!Benchmark.S} used by
      [registration_simple] below. *)

  module type S = sig
    (** Name of the benchmark *)
    val name : Namespace.t

    (** Description of the benchmark *)
    val info : string

    (** Filename of the benchmark module *)
    val module_filename : string

    (** Inference group of the benchmark *)
    val group : group

    (** Tags of the benchmark *)
    val tags : string list

    (** Configuration of the benchmark (eg sampling parameters, paths, etc) *)
    type config

    (** Default configuration of the benchmark *)
    val default_config : config

    (** Configuration encoding *)
    val config_encoding : config Data_encoding.t

    (** Benchmark workload *)
    type workload

    (** Workload encoding *)
    val workload_encoding : workload Data_encoding.t

    (** Optional conversion to vector, for report generation purposes *)
    val workload_to_vector : workload -> Sparse_vec.String.t

    (** Cost model *)
    val model : name:Namespace.t -> workload Model.t

    (** Benchmark purpose.
        The developer can submit an explanation as to what the benchmark is for when it's not generating code
        (i.e. in the case of [Other_purpose]). *)
    val purpose : Benchmark_base.purpose

    (** Creates a  benchmark, ready to be run.
            The benchmarks are thunked to prevent evaluating the workload until
            needed. *)
    val create_benchmark :
      rng_state:Random.State.t -> config -> workload Generator.benchmark
  end

  type t = (module S)
end

module Registration : sig
  val ns : Namespace.cons

  (** Registers a benchmark with a model, model names are uniformely generated
  *)
  val register : Benchmark.t -> unit

  val register_as_simple_with_num :
    group:Benchmark.group -> Benchmark_base.t -> unit
end

module Model : sig
  open Model

  type 'workload t = 'workload Model.t

  val make :
    ?takes_saturation_reprs:bool ->
    name:Namespace.t ->
    conv:('a -> 'b) ->
    (Namespace.t -> 'b model) ->
    'a t

  val unknown_const1 : ?const:Free_variable.t -> Namespace.t -> unit model

  val affine :
    ?intercept:Free_variable.t ->
    ?coeff:Free_variable.t ->
    Namespace.t ->
    (int * unit) model

  val logn : ?coeff:Free_variable.t -> Namespace.t -> (int * unit) model

  val nlogn :
    ?intercept:Free_variable.t ->
    ?coeff:Free_variable.t ->
    Namespace.t ->
    (int * unit) model

  val linear : ?coeff:Free_variable.t -> Namespace.t -> (int * unit) model
end
