(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(** Models associated to benchmarks have free variables. Some of these
    variables are to be inferred from the empirical data, but some others
    must be provided by other models and correspond to _dependencies_
    of a model upon the result of another one.

    The goal of this module is to take as input a set of models seen as
    sets of free variables and infer back a partial dependency ordering
    among them. In particular, a topological sort of this partial ordering
    yields a scheduling for the inference process that respects cross-model
    dependencies.

    Such a problem does not always have a solution, or can have several
    solutions (ie it is in general ill-posed). When there's more than
    one possible solution, we use a simple heuristic to pick one.
*)

(** Decide dependencies/provides of free variables *)
module Solver : sig
  module Unsolved : sig
    type t

    (** [build name ~fvs_unapplied fvs_applied] builds an unsolved node
        of a benchmark of name [name]. [fvs_unapplied] is the set of
        free variables of the models of the benchmark without workload
        application. [fvs_applied] is the set of free variables of
        the models of the benchmark applied with workloads.
    *)
    val build :
      Namespace.t ->
      fvs_unapplied:Free_variable.Set.t ->
      Free_variable.Set.t ->
      t
  end

  module Solved : sig
    type t = {
      dependencies : Free_variable.Set.t;
      provides : Free_variable.Set.t;
      name : Namespace.t;
    }

    val pp : Format.formatter -> t -> unit
  end

  val solve : Unsolved.t list -> Solved.t list
end

(** Visualization of solutions using Graphviz *)
module Graphviz : sig
  val save : string -> Solver.Solved.t list -> unit
end

(** Dependency graph of benchmarks using dependencies/provides *)
module Graph : sig
  type t

  val is_empty : t -> bool

  (** Which benchmarks provide each variable *)
  type providers_map = Solver.Solved.t list Free_variable.Map.t

  val is_ambiguous : providers_map -> bool

  (** Print out the ambiguity information to stderr *)
  val warn_ambiguities : providers_map -> unit

  type result = {
    (* Graph without ambiguities. The ambiguities are resolved by heuristics *)
    resolved : t;
    (* Graph with possible ambiguity *)
    with_ambiguities : t;
    (* Which benchmarks provide each variable *)
    providers_map : providers_map;
  }

  (** Build dependency graphs *)
  val build : Solver.Solved.t list -> result

  (** Dependency topological folding *)
  val fold : (Solver.Solved.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** Dependency topological iteration *)
  val iter : (Solver.Solved.t -> unit) -> t -> unit

  (** Visualize the graph using Graphviz *)
  val save_graphviz : t -> string -> unit
end

(** [find_model_or_generic model_name model_list] returns the model matching
    the local name [model_name] from a [model_list] of a benchmark. If none
    match, then searches for models named ["*"]. *)
val find_model_or_generic : string -> (string * 'model) list -> 'model option

(** [load_workload_files ~model_name files] loads [.workload] files given in [files],
    looks for the model [model_name], and if found, adds it to a dependency
    graph. Returns [(G, H)] where [G] is the final graph obtained this way,
    and [H] is a table that maps the name of a benchmark with its contents. *)
val load_workload_files :
  model_name:string ->
  string list ->
  Graph.t * Measure.packed_measurement Namespace.Hashtbl.t
