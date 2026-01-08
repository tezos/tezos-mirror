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

module Fv_map = Free_variable.Map
module Fv_set = Free_variable.Set

let pp_sep s ppf () = Format.fprintf ppf "%s@;" s

(* Decide dependencies/provides of free variables *)
module Solver = struct
  (* We proceed iteratively on a set of _nodes_.

     Nodes correspond to sets of free variables.

     A node is _solved_ when its variables can be partitioned in two subsets:
     - the set of _dependencies_
     - the set of _provided_ variables

     A node is _unsolved_ when this partition cannot be decided yet:
     - some free variables are kwown to be _dependencies_
     - some other are still undecided.

     A node is _redundant_ when it is solved and its set of _provided_ variables
     is empty. *)

  module Unsolved = struct
    type t = {
      dependencies : Fv_set.t;
      undecided_variables : Fv_set.t;
      name : Namespace.t;
    }

    (* [build name ~fvs_unapplied fvs] makes an initial [unsolved]:
       - [fvs_unapplied]: free variables occur in the models without workload
                          application.
       - [fvs]: free variables occur in the applied models.
    *)
    let build name ~fvs_unapplied fvs =
      (* Free variables in an applied model which do not in the unapplied
         model must be provided by other benchmarks, therefore they are
         dependencies. *)
      let dependencies = Fv_set.diff fvs fvs_unapplied in
      let undecided_variables = Fv_set.diff fvs dependencies in
      {name; dependencies; undecided_variables}
  end

  module Solved = struct
    type t = {dependencies : Fv_set.t; provides : Fv_set.t; name : Namespace.t}

    (* Comparison only by names for graph building *)
    module ForGraph : Graph.Sig.COMPARABLE with type t = t = struct
      type nonrec t = t

      let equal s1 s2 = Namespace.equal s1.name s2.name

      let compare s1 s2 = Namespace.compare s1.name s2.name

      let hash s = Namespace.hash s.name
    end

    let pp ppf ({name; dependencies; provides} : t) =
      Format.fprintf
        ppf
        "@[<v2>name: %a;@ dep: @[%a@]@ prv: @[%a@]@]"
        Namespace.pp
        name
        Fv_set.pp
        dependencies
        Fv_set.pp
        provides
  end

  type node =
    | Solved of Solved.t
    | Redundant of Solved.t
    | Unsolved of Unsolved.t

  type state = {solved : Solved.t list; unsolved : Unsolved.t list}

  let force_solved Unsolved.{dependencies; undecided_variables; name} =
    Solved.{dependencies; provides = undecided_variables; name}

  (* Sets free variable [v] to be 'solved' in node [n] *)
  let set_variable_as_solved (n : Unsolved.t) v =
    if not (Fv_set.mem v n.undecided_variables) then Unsolved n
    else
      let undecided = Fv_set.remove v n.undecided_variables in
      let deps = Fv_set.add v n.dependencies in
      let card = Fv_set.cardinal undecided in
      if card = 0 then
        Redundant {dependencies = deps; provides = Fv_set.empty; name = n.name}
      else if card = 1 then
        (* If there's only one variable left in [undecided], it must
           in fact be constrained by the model and becomes [provided]. *)
        Solved {dependencies = deps; provides = undecided; name = n.name}
      else
        Unsolved
          {dependencies = deps; undecided_variables = undecided; name = n.name}

  let empty_state = {solved = []; unsolved = []}

  let rec propagate_solved state (n : Solved.t) solved_but_not_propagated =
    let solved_but_not_propagated, unsolved =
      List.fold_left
        (fun (solved_acc, unsolved_acc) unsolved ->
          Fv_set.fold
            (fun provided_var (solved_acc, unsolved_acc) ->
              let node = set_variable_as_solved unsolved provided_var in
              match node with
              | Redundant node | Solved node ->
                  (node :: solved_acc, unsolved_acc)
              | Unsolved node -> (solved_acc, node :: unsolved_acc))
            n.provides
            (solved_acc, unsolved_acc))
        (solved_but_not_propagated, [])
        state.unsolved
    in
    let state = {solved = n :: state.solved; unsolved} in
    propagate_solved_loop state solved_but_not_propagated

  and propagate_solved_loop state solved_but_not_propagated =
    match solved_but_not_propagated with
    | [] -> state
    | solved :: solved_list -> propagate_solved state solved solved_list

  let solve unsolved_list =
    let roots, others =
      List.partition
        (fun (node : Unsolved.t) ->
          Fv_set.cardinal node.undecided_variables = 1)
        unsolved_list
    in
    (* Set the roots as solved. *)
    let roots =
      List.map
        (fun root ->
          Solved.
            {
              dependencies = root.Unsolved.dependencies;
              provides = root.Unsolved.undecided_variables;
              name = root.name;
            })
        roots
    in
    (* Propagate iteratively. *)
    let state = {empty_state with unsolved = others} in
    propagate_solved_loop state roots

  let solve unsolved_list =
    let least_constrained = solve unsolved_list in
    match least_constrained.unsolved with
    | [] -> least_constrained.solved
    | _ ->
        let set_solved = List.map force_solved least_constrained.unsolved in
        least_constrained.solved @ set_solved
end

(* Visualization of dependencies of benchmarks and free variables *)
module Graphviz = struct
  module G = Graph.Imperative.Digraph.Concrete (Namespace)

  module D () = struct
    let vattrs = Namespace.Hashtbl.create 1023

    include Graph.Graphviz.Dot (struct
      include G

      let edge_attributes _ = []

      let default_edge_attributes _ = []

      let vertex_attributes ns =
        Option.value ~default:[`Label (String.escaped @@ Namespace.basename ns)]
        @@ Namespace.Hashtbl.find_opt vattrs ns

      let default_vertex_attributes _ = []

      let graph_attributes _ = []

      let get_subgraph _ = None

      (* Node name including '.' and other symbols must be double-quoted *)
      let vertex_name ns = Printf.sprintf "%S" @@ Namespace.to_filename ns
    end)
  end

  let add_solved vattrs g solved =
    let data_name = solved.Solver.Solved.name in
    (* Some free variables have the same name as the benchmark.
       We must suffix "fv" for distinction.
    *)
    let fv_node fv = Namespace.cons (Free_variable.to_namespace fv) "fv" in
    let add_vertex name shape =
      (* We cannot always use [Namespace.basename] here because of
         ".../intercept" *)
      G.add_vertex g name ;
      let label = String.escaped @@ Namespace.to_string name in
      Namespace.Hashtbl.replace vattrs name [`Label label; `Shape shape]
    in
    let add_edges set ~inverted =
      Fv_set.iter
        (fun fv ->
          let n = fv_node fv in
          add_vertex n `Oval ;
          let from, to_ = if inverted then (data_name, n) else (n, data_name) in
          G.add_edge g from to_)
        set
    in
    add_vertex data_name `Box ;
    add_edges solved.dependencies ~inverted:false ;
    add_edges solved.provides ~inverted:true

  let visualize vattrs solution =
    let g = G.create () in
    List.iter (add_solved vattrs g) solution ;
    g

  let save fn solution =
    let oc = open_out fn in
    let module D = D () in
    D.output_graph oc @@ visualize D.vattrs solution ;
    close_out oc
end

(* Dependency graph of benchmarks using dependencies/provides *)
module Graph : sig
  type t

  val is_empty : t -> bool

  type providers_map = Solver.Solved.t list Fv_map.t

  val is_ambiguous : providers_map -> bool

  val warn_ambiguities : providers_map -> unit

  type result = {
    (* Graph without ambiguities.  The ambiguities are resolved by heuristics *)
    resolved : t;
    (* Graph with possible ambiguities *)
    with_ambiguities : t;
    (* Which benchmarks provide each variable *)
    providers_map : providers_map;
  }

  val build : Solver.Solved.t list -> result

  (** Topological ordered fold *)
  val fold : (Solver.Solved.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** Topological ordered iter *)
  val iter : (Solver.Solved.t -> unit) -> t -> unit

  (** Returns the topological ordered list of [Solver.Sovled.t] *)
  val to_sorted_list : t -> Solver.Solved.t list

  val save_graphviz : t -> string -> unit
end = struct
  open Solver
  open Solved (* This module is for the graph of [Solved.t] *)

  module G = struct
    module G = Graph.Imperative.Digraph.Concrete (Solver.Solved.ForGraph)
    include G
    include Graph.Topological.Make (G)
  end

  type t = G.t

  let is_empty = G.is_empty

  exception Missing_file_for_free_variable of {free_var : Free_variable.t}

  let () =
    Printexc.register_printer (function
      | Missing_file_for_free_variable {free_var} ->
          let error =
            Format.asprintf
              "Bug found: variable %a is not associated to any dataset. Please \
               report.\n"
              Free_variable.pp
              free_var
          in
          Some error
      | _ -> None)

  type providers_map = Solver.Solved.t list Fv_map.t

  let is_ambiguous =
    Fv_map.exists (fun _ -> function
      | [] -> assert false (* impossible *)
      | [_] -> false
      | _ -> true)

  let warn_ambiguities =
    let open Format in
    Fv_map.iter (fun fv -> function
      | [] -> assert false (* impossible *)
      | [_] -> () (* not ambiguous *)
      | solved_list ->
          eprintf
            "@[<v2>Warning: A variable is provided by multiple benchmarks. \
             Choosing the first one:@ " ;
          eprintf "Variable: %a@ " Free_variable.pp fv ;
          eprintf
            "@[<2>Benchmarks@ @[<v>%a@]@]"
            (pp_print_list ~pp_sep:(pp_sep " ") Solver.Solved.pp)
            solved_list ;
          eprintf "@]@.")

  type result = {
    resolved : t;
    with_ambiguities : t;
    providers_map : providers_map;
  }

  let build solved_list =
    (* Which benchmarks provide each variable? *)
    let providers_map : providers_map =
      List.fold_left
        (fun map ({provides; _} as solved) ->
          Fv_set.fold
            (fun free_var map ->
              Fv_map.update
                free_var
                (function
                  | None -> Some [solved]
                  | Some others -> Some (solved :: others))
                map)
            provides
            map)
        Fv_map.empty
        solved_list
    in
    (* Sort the benchmarks in the order of the numbers of dependencies.
       If ambiguous, the one with the least dependencies is chosen.
    *)
    let providers_map : providers_map =
      Fv_map.map
        (fun solved_list ->
          let compare a b =
            Int.compare
              (Fv_set.cardinal a.dependencies)
              (Fv_set.cardinal b.dependencies)
          in
          List.sort compare solved_list)
        providers_map
    in

    (* Resolve ambiguities by heuristics *)
    let provider_map_without_ambiguities : Solved.t Fv_map.t =
      (* Choose the provider with the least dependencies *)
      let m =
        Fv_map.map
          (fun providers ->
            match providers with
            | [] -> assert false (* impossible *)
            | s :: _ -> s)
          providers_map
      in
      (* Move the dropped provided variables to dependencies *)
      Fv_map.map
        (fun provider ->
          let provides, dropped =
            Fv_set.partition
              (fun fv ->
                match Fv_map.find fv m with
                | None -> assert false (* impossible *)
                | Some s -> s.name = provider.name)
              provider.provides
          in
          let dependencies = Fv_set.union provider.dependencies dropped in
          {provider with provides; dependencies})
        m
    in
    let solved_list_without_ambiguities =
      List.sort_uniq (fun s1 s2 ->
          Namespace.compare s1.Solved.name s2.Solved.name)
      @@ List.map snd @@ List.of_seq
      @@ Fv_map.to_seq provider_map_without_ambiguities
    in
    let build_graph iter_fun pmap solved_list =
      let len = List.length solved_list in
      let g = G.create ~size:len () in
      List.iter
        (fun ({dependencies; _} as s) ->
          G.add_vertex g s ;
          Fv_set.iter
            (fun fv_dep ->
              match Fv_map.find fv_dep pmap with
              | None ->
                  raise (Missing_file_for_free_variable {free_var = fv_dep})
              | Some d -> iter_fun (fun dep -> G.add_edge g dep s) d)
            dependencies)
        solved_list ;
      g
    in
    (* Make a graph, keeping ambiguities *)
    let g_with_ambiguities =
      let iter_fun add_edge deps = List.iter add_edge deps in
      build_graph iter_fun providers_map solved_list
    in
    (* Make a graph, resolving ambiguities by heuristics *)
    let g_without_ambiguities =
      let iter_fun add_edge dep = add_edge dep in
      build_graph
        iter_fun
        provider_map_without_ambiguities
        solved_list_without_ambiguities
    in
    {
      resolved = g_without_ambiguities;
      with_ambiguities = g_with_ambiguities;
      providers_map;
    }

  let fold = G.fold

  let iter = G.iter

  let to_sorted_list t = List.rev @@ fold List.cons t []

  let save_graphviz g fn =
    Graphviz.save fn @@ G.fold_vertex (fun s acc -> s :: acc) g []
end

(* Generic models, named "*", are models used to infer generic parameters used in
   many other benchmarks, namely the timer overhead, and the Lwt_main.run call *)
let find_model_or_generic local_model_name local_model_list =
  match
    List.assoc_opt ~equal:String.equal local_model_name local_model_list
  with
  | None -> List.assoc_opt ~equal:String.equal "*" local_model_list
  | res -> res

let load_workload_files ~local_model_name files =
  (* Use a table to store loaded measurements *)
  let table = Namespace.Hashtbl.create 51 in
  let unsolved =
    List.fold_left
      (fun unsolved filename ->
        let measurement = Measure.load ~filename in
        match measurement with
        | Measure.Measurement ((module Bench), m) -> (
            match find_model_or_generic local_model_name Bench.models with
            | None -> unsolved
            | Some model ->
                Namespace.Hashtbl.add table Bench.name measurement ;
                let fvs =
                  List.fold_left
                    (fun acc {Measure.workload; _} ->
                      let fvs =
                        Model.get_free_variable_set_applied model workload
                      in
                      Free_variable.Set.union fvs acc)
                    Free_variable.Set.empty
                    m.Measure.workload_data
                in
                let fvs_unapplied =
                  Benchmark.get_free_variable_set (module Bench)
                in
                Solver.Unsolved.build Bench.name ~fvs_unapplied fvs :: unsolved))
      []
      files
  in
  let solved = Solver.solve unsolved in
  let Graph.{resolved; providers_map; _} = Graph.build solved in
  Graph.warn_ambiguities providers_map ;
  (resolved, table)
