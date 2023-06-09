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

(** Benchmark parameters. *)
type 'config parameters = {bench_number : int; config : 'config}

(** The module type of benchmarks *)
module type S = sig
  (** Name of the benchmark *)
  val name : Namespace.t

  (** Description of the benchmark *)
  val info : string

  (** File where the benchmark module is defined *)
  val module_filename : string

  (** Destination of generated code *)
  val generated_code_destination : string option

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
