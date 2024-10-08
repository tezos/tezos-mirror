(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type NODE = sig
  type t

  val compare : t -> t -> int

  val id : t -> string

  val attributes : t -> (string * string) list
end

module type S = sig
  type node

  module Nodes : Set.S with type elt = node

  type t

  val empty : t

  val add : node -> node -> t -> t

  val map : t -> (node -> Nodes.t -> Nodes.t) -> t

  val filter : t -> (node -> Nodes.t -> bool) -> t

  val nodes : t -> Nodes.t

  val output_dot_file : Format.formatter -> t -> unit

  val simplify : t -> t

  val sourced_at : Nodes.t -> t -> t
end

module Make (Node : NODE) : S with type node = Node.t = struct
  type node = Node.t

  module Nodes = Set.Make (Node)
  module Node_map = Map.Make (Node)

  type t = Nodes.t Node_map.t

  let empty = Node_map.empty

  let add a b graph =
    (* Ensure [b] is in the graph even if it has no outgoing edge.
       [output_dot_file] relies on all nodes being in the map. *)
    let graph =
      Fun.flip (Node_map.update b) graph @@ function
      | None -> Some Nodes.empty
      | Some _ as x -> x
    in
    (* Add the edge. *)
    Fun.flip (Node_map.update a) graph @@ function
    | None -> Some (Nodes.singleton b)
    | Some old -> Some (Nodes.add b old)

  let get node graph =
    match Node_map.find_opt node graph with
    | None -> Nodes.empty
    | Some nodes -> nodes

  let mem a b graph = Nodes.mem b (get a graph)

  let iter graph f = Node_map.iter f graph

  let map graph f = Node_map.mapi f graph

  let fold graph acc f = Node_map.fold f graph acc

  let filter graph f = Node_map.filter f graph

  let nodes graph =
    fold graph Nodes.empty @@ fun node _ acc -> Nodes.add node acc

  let output_dot_file fmt graph =
    let quote_id id =
      let name = String.map (function '"' -> '_' | c -> c) id in
      "\"" ^ name ^ "\""
    in
    let node_id node = quote_id (Node.id node) in
    Format.fprintf fmt "@[<v 2>digraph {" ;
    ( iter graph @@ fun source targets ->
      Format.fprintf
        fmt
        "@ @[%s[%s]@]"
        (node_id source)
        (String.concat
           ","
           (List.map
              (fun (k, v) -> quote_id k ^ "=" ^ quote_id v)
              (Node.attributes source))) ;
      Fun.flip Nodes.iter targets @@ fun target ->
      Format.fprintf fmt "@ @[%s -> %s@]" (node_id source) (node_id target) ) ;
    Format.fprintf fmt "@]@ }@."

  (* Return the set of nodes reachable from [node]. *)
  let reachable_set node graph =
    let rec gather node acc =
      if Nodes.mem node acc then acc
      else Nodes.fold gather (get node graph) (Nodes.add node acc)
    in
    gather node Nodes.empty

  (* [transitive_closure] returns the same as
     [map graph @@ fun node _ -> reachable_set node graph]
     but hopefully more efficiently thanks to memoization. *)
  (* There is probably a more efficient implementation of this.
     Fast matrix exponentiation is O(n^2.37) for non-sparse graphs,
     Ffloyd-Warshall is O(n^3). *)
  let transitive_closure graph =
    (* [reachable_set] computes the set of nodes reachable from [node],
       and its results are memoized in [memoized]. *)
    let memoized = ref Node_map.empty in
    (* [seen] contains the path that we took to reach [node];
       it is used to detect cycles. It would be more efficient to use a [Nodes.t],
       but a list allows to display the path in error messages.
       As long as the depth of the graph is not too big it's ok. *)
    let rec reachable_set seen node =
      if List.exists (fun other_node -> Node.compare node other_node = 0) seen
      then
        let seen = List.rev (node :: seen) in
        failwith
          (Node.id node ^ " is involved in a cycle: "
          ^ String.concat " -> " (List.map Node.id seen))
      else
        let seen = node :: seen in
        match Node_map.find_opt node !memoized with
        | Some result -> result
        | None ->
            (* Recursively compute the set of reachable nodes for each
               directly reachable node, and take the union. *)
            let directly_reachable = get node graph in
            let acc = Nodes.add node directly_reachable in
            let result =
              Nodes.fold
                (fun node acc -> Nodes.union acc (reachable_set seen node))
                directly_reachable
                acc
            in
            (* Save result for later (memoize). *)
            memoized := Node_map.add node result !memoized ;
            result
    in
    map graph (fun node _ -> reachable_set [] node)

  let simplify direct_graph =
    let transitive_closure = transitive_closure direct_graph in
    map direct_graph @@ fun _ edges ->
    Fun.flip Nodes.filter edges @@ fun edge ->
    (* Only keep [edge] if there is no other edge from which one can reach [edge]. *)
    Fun.flip Nodes.for_all (Nodes.remove edge edges) @@ fun other_edge ->
    not (mem other_edge edge transitive_closure)

  let sourced_at sources graph =
    let nodes_to_keep =
      let add_reachable source acc =
        Nodes.union acc (reachable_set source graph)
      in
      Nodes.fold add_reachable sources Nodes.empty
    in
    filter graph @@ fun node _ -> Nodes.mem node nodes_to_keep
end
