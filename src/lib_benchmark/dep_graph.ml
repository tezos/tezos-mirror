(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Fv_map = Free_variable.Map
module Fv_set = Free_variable.Set

let pp_sep s ppf () = Format.fprintf ppf "%s@;" s

let pp_print_set fmtr (set : Free_variable.Set.t) =
  let elts = Free_variable.Set.elements set in
  Format.fprintf fmtr "{ @[" ;
  Format.pp_print_list ~pp_sep:(pp_sep "; ") Free_variable.pp fmtr elts ;
  Format.fprintf fmtr "@] }"

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

  type unsolved = {
    dependencies : Fv_set.t;
    undecided_variables : Fv_set.t;
    name : Namespace.t;
  }

  type solved = {
    dependencies : Fv_set.t;
    provides : Fv_set.t;
    name : Namespace.t;
  }

  module Solved = struct
    type t = solved

    let equal s1 s2 = Namespace.equal s1.name s2.name

    let compare s1 s2 = Namespace.compare s1.name s2.name

    let hash s = Namespace.hash s.name
  end

  type problem = unsolved list

  type node = Solved of solved | Redundant of solved | Unsolved of unsolved

  type state = {solved : solved list; unsolved : unsolved list}

  let empty_problem : problem = []

  let make_unsolved name fvs =
    {dependencies = Fv_set.empty; undecided_variables = fvs; name}

  let add_to_problem (name, fvs) problem = make_unsolved name fvs :: problem

  let force_solved {dependencies; undecided_variables; name} =
    {dependencies; provides = undecided_variables; name}

  (* Sets free variable [v] to be 'solved' in node [n] *)
  let set_variable_as_solved (n : unsolved) (v : Free_variable.t) =
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

  let rec propagate_solved state (n : solved) solved_but_not_propagated =
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

  let solve unsolved =
    let roots, others =
      List.partition
        (fun (node : unsolved) ->
          Fv_set.is_empty node.dependencies
          && Fv_set.cardinal node.undecided_variables = 1)
        unsolved
    in
    (* Set the roots as solved. *)
    let roots =
      List.map
        (fun root ->
          {
            dependencies = Fv_set.empty;
            provides = root.undecided_variables;
            name = root.name;
          })
        roots
    in
    (* Propagate iteratively. *)
    let state = {empty_state with unsolved = others} in
    propagate_solved_loop state roots

  let solve unsolved =
    let least_constrained = solve unsolved in
    match least_constrained.unsolved with
    | [] -> least_constrained.solved
    | _ ->
        let set_solved = List.map force_solved least_constrained.unsolved in
        least_constrained.solved @ set_solved
end

(* Named Graph for now to avoid the name collision with OCamlGraph's Graph *)
module Graphviz = struct
  open Solver
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

      let vertex_name ns = Namespace.to_filename ns
    end)
  end

  let add_solved vattrs g solved =
    let data_name = solved.name in
    let fv_node fv = Free_variable.to_namespace fv in
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

  val build : Solver.solved list -> t

  val fold : (Solver.solved -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (Solver.solved -> unit) -> t -> unit

  val dependencies : t -> Namespace.t list -> Solver.solved list

  val save_graphviz : t -> string -> unit
end = struct
  open Solver

  module G = struct
    module G = Graph.Imperative.Digraph.Concrete (Solver.Solved)
    include G
    include Graph.Topological.Make (G)
  end

  type t = G.t

  let is_empty g = G.is_empty g

  exception Missing_file_for_free_variable of {free_var : Free_variable.t}

  let () =
    Printexc.register_printer (function
        | Missing_file_for_free_variable {free_var} ->
            let error =
              Format.asprintf
                "Bug found: variable %a is not associated to any dataset. \
                 Please report.\n"
                Free_variable.pp
                free_var
            in
            Some error
        | _ -> None)

  let warn_ambiguity solved_to_benchmark solved_list ambiguity =
    let open Format in
    Fv_map.iter
      (fun fv ({name; _}, _) ->
        Option.iter (fun ns ->
            let ns = Namespace.Set.remove name ns in
            let ns = name :: List.of_seq (Namespace.Set.to_seq ns) in
            eprintf
              "@[<v2>Warning: A variable is provided by multiple benchmarks. \
               Choosing the first one:@ " ;
            eprintf "Variable: %a@ " Free_variable.pp fv ;
            eprintf
              "@[<2>Benchmarks@ @[<v>%a@]@]"
              (pp_print_list ~pp_sep:(pp_sep " ") (fun ppf n ->
                   let x =
                     match
                       List.find
                         (fun solved -> Namespace.equal solved.name n)
                         solved_list
                     with
                     | None -> assert false
                     | Some x -> x
                   in
                   fprintf
                     ppf
                     "@[<2>%a@ @[dep: @[%a@]@ prv: @[%a@]"
                     Namespace.pp
                     n
                     pp_print_set
                     x.dependencies
                     pp_print_set
                     x.provides ;
                   fprintf ppf "@]@]"))
              ns ;
            eprintf "@]@.")
        @@ Fv_map.find fv ambiguity)
      solved_to_benchmark

  let build_with_ambiguity solved_list =
    let solved_to_benchmark, ambiguity =
      List.fold_left
        (fun (map, ambiguity) ({provides; name; dependencies} as solved) ->
          Fv_set.fold
            (fun free_var (map, ambiguity) ->
              match Fv_map.find free_var map with
              | None ->
                  (Fv_map.add free_var (solved, dependencies) map, ambiguity)
              | Some (other, other_deps) ->
                  let ambiguity =
                    let ns =
                      Option.value ~default:Namespace.Set.empty
                      @@ Fv_map.find free_var ambiguity
                    in
                    Fv_map.add
                      free_var
                      Namespace.Set.(add name (add other.name ns))
                      ambiguity
                  in
                  (* Ambiguity resolved by some heuristics.  Not good. *)
                  let this_card = Fv_set.cardinal dependencies in
                  let other_card = Fv_set.cardinal other_deps in
                  let map =
                    if this_card < other_card then
                      Fv_map.add free_var (solved, dependencies) map
                    else map
                  in
                  (map, ambiguity))
            provides
            (map, ambiguity))
        (Fv_map.empty, Fv_map.empty)
        solved_list
    in
    warn_ambiguity solved_to_benchmark solved_list ambiguity ;
    let len = List.length solved_list in
    let g = G.create ~size:len () in
    List.iter
      (fun ({dependencies; _} as s) ->
        if Fv_set.is_empty dependencies then G.add_vertex g s
        else
          Fv_set.iter
            (fun dep ->
              match Fv_map.find dep solved_to_benchmark with
              | None -> raise (Missing_file_for_free_variable {free_var = dep})
              | Some (dep, _) -> G.add_edge g dep s)
            dependencies)
      solved_list ;
    (g, ambiguity)

  let build solved_list = fst @@ build_with_ambiguity solved_list

  let fold = G.fold

  let iter = G.iter

  module Reachability =
    Graph.Fixpoint.Make
      (G)
      (struct
        type vertex = G.E.vertex

        type edge = G.E.t

        type g = G.t

        type data = bool

        let direction = Graph.Fixpoint.Backward

        let equal = ( = )

        let join = ( || )

        let analyze _ x = x
      end)

  let dependencies g init =
    let init =
      G.fold_vertex
        (fun solved acc ->
          if List.mem ~equal:Namespace.equal solved.name init then solved :: acc
          else acc)
        g
        []
    in
    let p =
      Reachability.analyze
        (fun n -> List.mem ~equal:Solver.Solved.equal n init)
        g
    in
    List.rev
    @@ G.fold (fun solved acc -> if p solved then solved :: acc else acc) g []

  let save_graphviz g fn =
    Graphviz.save fn @@ G.fold_vertex (fun s acc -> s :: acc) g []
end

(* Generic models, named "*", are models used to infer generic parameters used in
   many other benchmarks, namely the timer overhead, and the Lwt_main.run call *)
let find_model_or_generic model_name model_list =
  match List.assoc_opt ~equal:String.equal model_name model_list with
  | None -> List.assoc_opt ~equal:String.equal "*" model_list
  | res -> res

let load_workload_files ~local_model_name:model_name files =
  (* Use a table to store loaded measurements *)
  let table = Namespace.Hashtbl.create 51 in
  let unsolved =
    List.fold_left
      (fun unsolved filename ->
        let measurement = Measure.load ~filename in
        match measurement with
        | Measure.Measurement ((module Bench), m) -> (
            match find_model_or_generic model_name Bench.models with
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
                Solver.add_to_problem (Bench.name, fvs) unsolved))
      Solver.empty_problem
      files
  in
  let solved = Solver.solve unsolved in
  (Graph.build solved, table)
