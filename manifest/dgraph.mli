(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Mini graph library that we use to visualize our dependency graph. *)

(** Note: we don't use an external library such as OCamlgraph for the main reason
    that the manifest must build without the dune and opam environment being set up. *)

module type NODE = sig
  (** Arguments of the [Dgraph.Make] functor. *)

  (** Graph nodes. *)
  type t

  (** Compare two nodes. *)
  val compare : t -> t -> int

  (** Get a string that uniquely identifies a node.

      Node identifiers are used in DOT outputs. *)
  val id : t -> string

  (** Return the attributes to assign to a node in a [.dot] file. *)
  val attributes : t -> (string * string) list

  (** Return which cluster to assign the node to in a [.dot] file. *)
  val cluster : t -> string option
end

module type S = sig
  (** Directed graphs. *)

  (** Graph nodes. *)
  type node

  (** Sets of nodes. *)
  module Nodes : Set.S with type elt = node

  (** Maps of nodes. *)
  module Node_map : Map.S with type key = node

  (** Graphs. *)
  type t

  (** The empty graph. *)
  val empty : t

  (** Add a directed edge to a graph. *)
  val add : node -> node -> t -> t

  (** Apply a function to modify the edges from each node. *)
  val map : t -> (node -> Nodes.t -> Nodes.t) -> t

  (** Apply a function to all nodes of a graph to make a node map. *)
  val map_nodes : t -> (node -> Nodes.t -> 'a) -> 'a Node_map.t

  (** Fold over all nodes. *)
  val fold : t -> 'a -> (node -> Nodes.t -> 'a -> 'a) -> 'a

  (** Remove nodes that do not match a predicate. *)
  val filter : t -> (node -> Nodes.t -> bool) -> t

  (** Get the set of nodes in a graph. *)
  val nodes : t -> Nodes.t

  (** Output a DOT file with a [digraph] containing all edges from a graph.

      Usage: [output_dot_file fmt graph]

      The result can be compiled to e.g. an SVG using: [dot -O -Tsvg filename]
      where [dot] can be installed on Debian with [apt install graphviz]. *)
  val output_dot_file : Format.formatter -> t -> unit

  (** Compute the transitive closure of a graph. *)
  val transitive_closure : t -> t

  (** Remove redundant edges.

      Removes edges [A -> C] such that there is another path [A -> B -> ... -> C] already. *)
  val simplify : t -> t

  (** Take the subgraph of nodes reachable from some specific nodes. *)
  val sourced_at : Nodes.t -> t -> t

  (** Reverse all edges of a graph. *)
  val reverse : t -> t
end

(** Make a directed graph module from a node module. *)
module Make (Node : NODE) : S with type node = Node.t
