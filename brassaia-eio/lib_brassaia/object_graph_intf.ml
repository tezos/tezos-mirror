(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  (** Directed graph *)
  include Graph.Sig.I

  (** Basic operations. *)
  include Graph.Oper.S with type g := t

  (** Topological traversal *)
  module Topological : sig
    val fold : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** Get all the vertices. *)
  val vertex : t -> vertex list

  (** Get all the relations. *)
  val edges : t -> (vertex * vertex) list

  (** [closure depth pred min max ()] creates the transitive closure graph of
      [max] using the predecessor relation [pred]. The graph is bounded by the
      [min] nodes and by [depth].

      {b Note:} Both [min] and [max] are subsets of [n]. *)
  val closure :
    ?depth:int ->
    pred:(vertex -> vertex list) ->
    min:vertex list ->
    max:vertex list ->
    unit ->
    t

  (** [iter depth min max node edge skip rev ()] iterates in topological order
      over the closure graph starting with the [max] nodes and bounded by the
      [min] nodes and by [depth].

      It applies three functions while traversing the graph: [node] on the
      nodes; [edge n predecessor_of_n] on the directed edges and [skip n] to not
      include a node [n], its predecessors and the outgoing edges of [n].

      If [rev] is true (the default) then the graph is traversed in the reverse
      order: [node n] is applied only after it was applied on all its
      predecessors; [edge n p] is applied after [node n]. Note that [edge n p]
      is applied even if [p] is skipped.

      [cache_size] is the size of the LRU cache used to store nodes already
      seen. If [None] (by default) every traversed nodes is stored (and thus no
      entries are never removed from the LRU). *)
  val iter :
    ?cache_size:int ->
    ?depth:int ->
    pred:(vertex -> vertex list) ->
    min:vertex list ->
    max:vertex list ->
    node:(vertex -> unit) ->
    ?edge:(vertex -> vertex -> unit) ->
    skip:(vertex -> bool) ->
    rev:bool ->
    unit ->
    unit

  (** [breadth_first_traversal ?cache_size pred max node ()] traverses the
      closure graph in breadth-first order starting with the [max] nodes. It
      applies [node] on the nodes of the graph while traversing it. *)
  val breadth_first_traversal :
    ?cache_size:int ->
    pred:(vertex -> vertex list) ->
    max:vertex list ->
    node:(vertex -> unit) ->
    unit ->
    unit

  (** [output ppf vertex edges name] create aand dumps the graph contents on
      [ppf]. The graph is defined by its [vertex] and [edges]. [name] is the
      name of the output graph.*)
  val output :
    Format.formatter ->
    (vertex * Graph.Graphviz.DotAttributes.vertex list) list ->
    (vertex * Graph.Graphviz.DotAttributes.edge list * vertex) list ->
    string ->
    unit

  (** Compute the minimum vertex. *)
  val min : t -> vertex list

  (** Compute the maximun vertex. *)
  val max : t -> vertex list

  (** Expose the graph internals. *)
  type dump = vertex list * (vertex * vertex) list

  (** Expose the graph as a pair of vertices and edges. *)
  val export : t -> dump

  (** Import a graph. *)
  val import : dump -> t

  (** The base functions over graph internals. *)
  module Dump : Type.S with type t = dump
end

module type HASH = sig
  include Type.S

  val short_hash : t -> int
end

module type Sigs = sig
  module type S = S

  module type HASH = HASH

  (** Build a graph. *)
  module Make
      (Contents_key : Type.S)
      (Node_key : Type.S)
      (Commit_key : Type.S)
      (Branch : Type.S) :
    S
      with type V.t =
        [ `Contents of Contents_key.t
        | `Node of Node_key.t
        | `Commit of Commit_key.t
        | `Branch of Branch.t ]
end
