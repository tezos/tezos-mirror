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
end

module type S = sig
  (** Directed graphs. *)

  (** Graph nodes. *)
  type node

  (** Sets of nodes. *)
  module Nodes : Set.S with type elt = node

  (** Graphs. *)
  type t

  (** The empty graph. *)
  val empty : t

  (** Add a directed edge to a graph. *)
  val add : node -> node -> t -> t

  (** Output a DOT file with a [digraph] containing all edges from a graph.

      Usage: [output_dot_file fmt graph]

      The result can be compiled to e.g. an SVG using: [dot -O -Tsvg filename]
      where [dot] can be installed on Debian with [apt install graphviz]. *)
  val output_dot_file : Format.formatter -> t -> unit
end

(** Make a directed graph module from a node module. *)
module Make (Node : NODE) : S with type node = Node.t
