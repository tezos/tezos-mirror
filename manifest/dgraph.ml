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
end

module type S = sig
  type node

  module Nodes : Set.S with type elt = node

  type t

  val empty : t

  val add : node -> node -> t -> t

  val output_dot_file : Format.formatter -> t -> unit
end

module Make (Node : NODE) : S with type node = Node.t = struct
  type node = Node.t

  module Nodes = Set.Make (Node)
  module Map = Map.Make (Node)

  type t = Nodes.t Map.t

  let empty = Map.empty

  let add a b graph =
    Fun.flip (Map.update a) graph @@ function
    | None -> Some (Nodes.singleton b)
    | Some old -> Some (Nodes.add b old)

  let iter graph f = Map.iter f graph

  let output_dot_file fmt graph =
    Format.fprintf fmt "@[<v 2>digraph {" ;
    ( iter graph @@ fun source targets ->
      Fun.flip Nodes.iter targets @@ fun target ->
      let dot_id node =
        let name = String.map (function '"' -> '_' | c -> c) (Node.id node) in
        "\"" ^ name ^ "\""
      in
      Format.fprintf fmt "@ @[%s -> %s@]" (dot_id source) (dot_id target) ) ;
    Format.fprintf fmt "@]@ }@."
end
