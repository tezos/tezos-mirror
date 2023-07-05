(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** This module defines several signatures to create a benchmark.
    * The most generic is [S]; it provides a rather complete control on
      benchmark creation.
    * [Simple] and [Simple_with_num] can be used to ease benchmark creation,
      with simpler parameters. Registration functions in {!Registration} will
      convert these simple interfaces back to [S].
    * All the common parameters for these signatures are declared in
      [Benchmark_base]. 
      *)

(** Some benchmarks depend on others, and some are for generic parameters that
    most benchmarks depend on. We need this information in order to correctly
    infer the values of parameters after a benchmark run; the user provides it
    with a group.

    * [Standalone]: benchmarks that don't depend on others (except generic
      ones). This is the value to use if you're not sure whether you should
      group your benchmark.
    * [Group]: benchmarks that belong to the given inference group. Note that
      setting a benchmark with a group that is referenced only in this benchmark
      will produce the same inference results as with [Standalone].
    * [Generic]: for generic parameters only. *)
type group = Standalone | Group of string | Generic

(** Described the purpose of the benchmark.
      * [Generate_code of destination]: generates code at the given
        [destination] file, prefixed by "src/proto_alpha/lib_protocol/" and
        suffixed by "_costs_generated.ml".
      * [Other_purpose of purpose]: any other purpose. The goal is to explain why the function is benchmarked since it does not produce a cost function.
  *)
type purpose = Other_purpose of string | Generate_code of string

(** Benchmark parameters. *)
type 'config parameters = {bench_number : int; config : 'config}

(** Common declarations used by the different module types of benchmarks *)
module type Benchmark_base = sig
  (** Name of the benchmark *)
  val name : Namespace.t

  (** Description of the benchmark *)
  val info : string

  (** File where the benchmark module is defined *)
  val module_filename : string

  (** Described the purpose of the benchmark.
      * [Generate_code of destination]: generates code at the given [destination] file.
      * [Other_purpose of purpose]: any other purpose. The goal is to explain why the function is benchmarked since it does not produce a cost function.
  *)
  val purpose : purpose

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
end

(** A simplification of [S] below, when only one model is defined and no
    particular process is needed to generate benchmarks. *)
module type Simple = sig
  include Benchmark_base

  (** Inference group of the benchmark *)
  val group : group

  (** Model used for inference *)
  val model : workload Model.t

  (** Creates a  benchmark, ready to be run.
      The benchmarks are thunked to prevent evaluating the workload until
      needed. *)
  val create_benchmark :
    rng_state:Random.State.t -> config -> workload Generator.benchmark
end

(** A simplification of [S] below, when only one model is defined. *)
module type Simple_with_num = sig
  include Benchmark_base

  (** Inference group of the benchmark *)
  val group : group

  (** Model used for inference *)
  val model : workload Model.t

  (** Benchmark generator *)
  val create_benchmarks :
    rng_state:Random.State.t ->
    bench_num:int ->
    config ->
    (unit -> workload Generator.benchmark) trace
end

(** The module type of benchmarks *)
module type S = sig
  include Benchmark_base

  (** Cost models, with a given local name (string) for reference *)
  val models : (string * workload Model.t) list

  (** Benchmark generator *)
  val create_benchmarks :
    rng_state:Random.State.t ->
    bench_num:int ->
    config ->
    (unit -> workload Generator.benchmark) list
end

type t = (module S)

type simple = (module Simple)

type simple_with_num = (module Simple_with_num)

val pp : Format.formatter -> t -> unit

type ('cfg, 'workload) poly =
  (module S with type config = 'cfg and type workload = 'workload)

type packed = Ex : ('cfg, 'workload) poly -> packed

val ex_unpack : t -> packed

(** Get the name of a benchmark *)
val name : t -> Namespace.t

(** Get the description of a benchmark *)
val info : t -> string

(** Get the tags of a benchmark *)
val tags : t -> string list

(** Returns the free variables occur in the models of the benchmark. *)
val get_free_variable_set : (module S) -> Free_variable.Set.t
