(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** The [Registration] module contains functions regarding the registration
    of benchmarks, models, parameters and commands for the Snoop tool. *)

exception Benchmark_not_found of Namespace.t

exception Model_not_found of Namespace.t

exception Parameter_not_found of Free_variable.t

(** Types of the registered informations for each kind of item.
    These informations are retrieved with the query functions [all_*] and
    [find_*] defined below *)

(** The information registered for a benchmark is the benchmark itself *)
type benchmark_info = Benchmark.t

(** We only register abstract models. For each of them, we store the model
    itself, as well as each of its occurences in benchmarks *)
type model_info = {model : Model.packed_model; from : local_model_info list}

(** We only store the name of the benchmark and the local name of the model,
    as it is enough information to retrieve the associated model *)
and local_model_info = {bench_name : Namespace.t; local_model_name : string}

(** For each parameter, we register the list of models (by name) in which
    they occur *)
type parameter_info = Namespace.t list

(* -------------------------------------------------------------------------- *)
(* Registration functions *)

(** Register a benchmark. Recursively registers any relevant model and parameter
    included in it. *)
val register : Benchmark.t -> unit

(** Register a {!type:Tezos_clic.command} for the command line *)
val add_command : unit Tezos_clic.command -> unit

(* -------------------------------------------------------------------------- *)
(* Listing functions *)

(** Returns the list of all registered benchmarks *)
val all_benchmarks : unit -> (Namespace.t * benchmark_info) list

(** Returns the list of all registered models *)
val all_models : unit -> (Namespace.t * model_info) list

(** Returns the list of all registered parameters *)
val all_parameters : unit -> (Free_variable.t * parameter_info) list

(** Returns the list of all registered commands *)
val all_custom_commands : unit -> unit Tezos_clic.command list

(** Returns the list of all the tags from registered benchmarks *)
val all_tags : unit -> string list

(** Returns the list of all the model names
    Same as [all_models () |> List.map fst] *)
val all_model_names : unit -> Namespace.t list

(** Returns the list of all local model names as they appear in registered
    benchmarks *)
val all_local_model_names : unit -> string list

(* -------------------------------------------------------------------------- *)
(* Search functions *)

(** [find_benchmarks_with_tags ~mode tag_list] returns all the benchmarks which tags
    match the given [tag_list] with the given mode.
    [`All] means all tags in [tag_list] appear in the tags of the returned benchmarks
    [`Exact] means the returned benchmarks' tags match exactly with [tag_list]
    [`Any] means the returned benchmarks have at least one tag also present in [tag_list]*)
val find_benchmarks_with_tags :
  mode:[< `All | `Exact | `Any] ->
  string list ->
  (Namespace.t * benchmark_info) list

(** The following query functions take a [string] as input. This [string] is converted
    into a [Namespace.t] with {!Namespace.of_string}. This conversion is then returned
    as output. This ensures that the returned [Namespace.t] values correspond to
    registered items. *)

(** [find_benchmark s] returns [Some (name,b)] with [name = s] if the benchmark [b] is
    named [name] and has been registered, else returns [None] *)
val find_benchmark : Namespace.t -> benchmark_info option

(** [find_benchmark_exn s] returns [(name,b)] with [name = s] if the benchmark [b] is
    named [name] and has been registered, else raises the exception {! Benchmark_not_found s} *)
val find_benchmark_exn : Namespace.t -> benchmark_info

(** [find_benchmarks_in_namespace s] returns all registered benchmarks which name
    has [s] as a parent namespace.
    For instance, if two benchmarks [a/b/bench1] and [a/b/bench2] are registered, then
    - if [s = a] or [s = a/b], then the function returns both benchmarks
    - if [s = a/b/bench], the empty list is returned
    - if [s = a/b/bench1], then [a/b/bench1] is returned *)
val find_benchmarks_in_namespace :
  Namespace.t -> (Namespace.t * benchmark_info) list

(** [find_model s] returns [Some (name,m)] with [name = s] if the model of [m] is
    named [name] and has been registered, else returns [None] *)
val find_model : Namespace.t -> model_info option

(** [find_model_exn s] returns [(name,m)] with [name = s] if the model of [m] is
    named [name] and has been registered, else raises the exception {! Model_not_found s} *)
val find_model_exn : Namespace.t -> model_info

(** [find_models_in_namespace s] returns all registered models which name
    has [s] as a parent namespace. *)
val find_models_in_namespace : Namespace.t -> (Namespace.t * model_info) list

(** [find_parameter s] returns [Some (name,p)] if the parameter [name = s] has been registered,
    and [p] is the registered info on [s], else returns [None] *)
val find_parameter : Free_variable.t -> parameter_info option

(** [find_parameter_exn s] returns [(name,p)] if the parameter [name = s] has been registered,
    and [p] is the registered info on [s], else raises the exception
    {! Parameter_not_found s} *)
val find_parameter_exn : Free_variable.t -> parameter_info

(** [find_parameters_in_namespace s] returns all registered info for parameters which name
    has [s] as a parent namespace. *)
val find_parameters_in_namespace :
  Namespace.t -> (Free_variable.t * parameter_info) list
