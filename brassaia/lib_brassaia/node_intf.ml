(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

open! Import

module type Core = sig
  (** {1 Node values} *)

  type t [@@deriving brassaia]
  (** The type for node values. *)

  val encoding : t Data_encoding.t
  (** [encoding] is the data_encoding for {!type-t}. *)

  type contents_key [@@deriving brassaia]
  (** The type for contents keys. *)

  val contents_key_encoding : contents_key Data_encoding.t
  (** [contents_key_encoding] is the data_encoding for {!type-contents_key}. *)

  type node_key [@@deriving brassaia]
  (** The type for node keys. *)

  val node_key_encoding : node_key Data_encoding.t
  (** [node_key_encoding] is the data_encoding for {!type-node_key}. *)

  type value = [ `Node of node_key | `Contents of contents_key ]
  [@@deriving brassaia]
  (** The type for either (node) keys or (contents) keys combined *)

  val value_encoding : value Data_encoding.t
  (** [value_encoding] is the data_encoding for {!type-value}. *)

  type hash [@@deriving brassaia]
  (** The type of hashes of values. *)

  val hash_encoding : hash Data_encoding.t
  (** [hash_encoding] is the data_encoding for {!type-hash}. *)

  val of_list : (Path.step * value) list -> t
  (** [of_list l] is the node [n] such that [list n = l]. *)

  val list :
    ?offset:int -> ?length:int -> ?cache:bool -> t -> (Path.step * value) list
  (** [list t] is the contents of [t]. [offset] and [length] are used to
      paginate results. *)

  val of_seq : (Path.step * value) Seq.t -> t
  (** [of_seq s] is the node [n] such that [seq n = s]. *)

  val seq :
    ?offset:int -> ?length:int -> ?cache:bool -> t -> (Path.step * value) Seq.t
  (** [seq t] is the contents of [t]. [offset] and [length] are used to paginate
      results.

      See {!caching} for an explanation of the [cache] parameter *)

  val empty : unit -> t
  (** [empty ()] is the empty node. *)

  val is_empty : t -> bool
  (** [is_empty t] is true iff [t] is {!val-empty}. *)

  val length : t -> int
  (** [length t] is the number of entries in [t]. *)

  val hash_exn : ?force:bool -> t -> hash
  (** [hash_exn t] is the hash of [t].

      Another way of computing it is [Hash.Typed(Hash)(Node).hash t] which
      computes the pre-hash of [t] before hashing it using [Hash]. [hash_exn]
      might be faster because the it may be optimised (e.g. it may use caching).

      [hash_exn t] is [hash_exn ~force:true t] which is not expected to raise an
      exception. [hash_exn ~force:false t] will raise [Not_found] if the hash
      requires IOs to be computed. *)

  val clear : t -> unit
  (** Cleanup internal caches. *)

  val find : ?cache:bool -> t -> Path.step -> value option
  (** [find t s] is the value associated with [s] in [t].

      A node can point to user-defined {{!contents_key} contents}. The edge
      between the node and the contents is labeled by a {!Path.step}.

      See {!caching} for an explanation of the [cache] parameter *)

  val add : t -> Path.step -> value -> t
  (** [add t s v] is the node where [find t v] is [Some s] but is similar to [t]
      otherwise. *)

  val remove : t -> Path.step -> t
  (** [remove t s] is the node where [find t s] is [None] but is similar to [t]
      otherwise. *)

  (** {2:caching caching}

      [cache] regulates the caching behaviour regarding the node's internal data
      which may be lazily loaded from the backend, depending on the node
      implementation.

      [cache] defaults to [true] which may greatly reduce the IOs and the
      runtime but may also increase the memory consumption.

      [cache = false] doesn't replace a call to [clear], it only prevents the
      storing of new data, it doesn't discard the existing one. *)

  (** {1 Recursive Nodes} *)

  (** Some [Node] implementations (like [brassaia-pack]'s inodes) can represent a
      node as a set of nodes. One operation on such "high-level" node
      corresponds to a sequence of recursive calls to the underlying
      "lower-level" nodes. Note: theses [effects] are not in the Lwt monad on
      purpose (so [Tree.hash] and [Tree.equal] are not in the Lwt monad as
      well). *)

  type effect := expected_depth:int -> node_key -> t option
  (** The type for read effects. *)

  val with_handler : (effect -> effect) -> t -> t
  (** [with_handler f] replace the current effect handler [h] by [f h]. [f h]
      will be called for all the recursive read effects that are required by
      recursive operations on nodes. .*)

  type head :=
    [ `Node of (Path.step * value) list | `Inode of int * (int * hash) list ]
  [@@deriving brassaia]

  val head : t -> head
  (** Reveal the shallow internal structure of the node.

      Only hashes and not keys are revealed in the [`Inode] case, this is
      because these inodes might not be keyed yet. *)
end

module type S_generic_key = sig
  include Core
  (** @inline *)

  (** {2 merging} *)

  val merge :
    contents:contents_key option Merge.t ->
    node:node_key option Merge.t ->
    t Merge.t
  (** [merge] is the merge function for nodes. *)

  exception Dangling_hash of { context : string; hash : hash }
end

module type S = sig
  type hash

  (** @inline *)
  include
    S_generic_key
      with type hash := hash
       and type contents_key = hash
       and type node_key = hash
end

module type Portable = sig
  type hash

  (** @inline *)
  include
    Core
      with type hash := hash
       and type contents_key = hash
       and type node_key = hash

  type node

  val of_node : node -> t

  (** {2 merging} *)

  val merge :
    contents:contents_key option Merge.t ->
    node:node_key option Merge.t ->
    t Merge.t
  (** [merge] is the merge function for nodes. *)

  (** {1 Proofs} *)

  type proof =
    [ `Blinded of hash
    | `Values of (Path.step * value) list
    | `Inode of int * (int * proof) list ]
  [@@deriving brassaia]
  (** The type for proof trees. *)

  val to_proof : t -> proof

  val of_proof : depth:int -> proof -> t option
  (** [of_proof ~depth p] is [None] if [p] is corrupted or incompatible with
      [depth]. It is [Some t] when [t] is a node if the operation succeeded.

      [hash_exn t] never raises [Not_found] *)
end

open struct
  module S_is_a_generic_key (X : S) : S_generic_key = X
end

module type Maker_generic_key = functor
  (Hash : Hash.S)
  (Contents_key : Key.S with type hash = Hash.t)
  (Node_key : Key.S with type hash = Hash.t)
  -> sig
  include
    S_generic_key
      with type hash = Hash.t
       and type contents_key = Contents_key.t
       and type node_key = Node_key.t

  module Portable : Portable with type node := t and type hash := hash
end

module type Store = sig
  include Indexable.S

  (** [Path] provides base functions on node paths. *)

  val merge : [> read_write ] t -> key option Merge.t
  (** [merge] is the 3-way merge function for nodes keys. *)

  (** [Val] provides base functions for node values. *)
  module Val :
    S_generic_key
      with type t = value
       and type hash = hash
       and type node_key = key

  module Hash : Hash.Typed with type t = hash and type value = value

  module Contents : Contents.Store with type key = Val.contents_key
  (** [Contents] is the underlying contents store. *)
end

module type Graph = sig
  (** {1 Node Graphs} *)

  type 'a t
  (** The type for store handles. *)

  type contents_key [@@deriving brassaia]
  (** The type of user-defined contents. *)

  type node_key [@@deriving brassaia]
  (** The type for node values. *)

  type value = [ `Node of node_key | `Contents of contents_key ]
  [@@deriving brassaia]
  (** The type for store values. *)

  val empty : [> write ] t -> node_key Lwt.t
  (** The empty node. *)

  val init : [> write ] t -> (Path.step * value) list -> node_key Lwt.t
  (** [init t n] is a new node containing [n]. *)

  val list : [> read ] t -> node_key -> (Path.step * value) list Lwt.t
  (** [list t n] is the contents of the node [n]. *)

  val find : [> read ] t -> node_key -> Path.t -> value option Lwt.t
  (** [find t n p] is the contents of the path [p] starting form [n]. *)

  val add : [> read_write ] t -> node_key -> Path.t -> value -> node_key Lwt.t
  (** [add t n p v] is the node [x] such that [find t x p] is [Some v] and it
      behaves the same [n] for other operations. *)

  val remove : [> read_write ] t -> node_key -> Path.t -> node_key Lwt.t
  (** [remove t n path] is the node [x] such that [find t x] is [None] and it
      behhaves then same as [n] for other operations. *)

  val closure :
    [> read ] t -> min:node_key list -> max:node_key list -> node_key list Lwt.t
  (** [closure t min max] is the unordered list of nodes [n] reachable from a
      node of [max] along a path which: (i) either contains no [min] or (ii) it
      ends with a [min].

      {b Note:} Both [min] and [max] are subsets of [n]. *)

  val iter :
    [> read ] t ->
    min:node_key list ->
    max:node_key list ->
    ?node:(node_key -> unit Lwt.t) ->
    ?contents:(contents_key -> unit Lwt.t) ->
    ?edge:(node_key -> node_key -> unit Lwt.t) ->
    ?skip_node:(node_key -> bool Lwt.t) ->
    ?skip_contents:(contents_key -> bool Lwt.t) ->
    ?rev:bool ->
    unit ->
    unit Lwt.t
  (** [iter t min max node edge skip rev ()] iterates in topological order over
      the closure of [t].

      It applies the following functions while traversing the graph: [node] on
      the nodes; [edge n predecessor_of_n] on the directed edges; [skip_node n]
      to not include a node [n], its predecessors and the outgoing edges of [n]
      and [skip_contents c] to not include content [c].

      If [rev] is true (the default) then the graph is traversed in the reverse
      order: [node n] is applied only after it was applied on all its
      predecessors; [edge n p] is applied after [node n]. Note that [edge n p]
      is applied even if [p] is skipped. *)
end

module type Sigs = sig
  module type S = S

  (** [Make] provides a simple node implementation, parameterized by hash and
      path implementations. The contents and node values are addressed
      directly by their hash. *)
  module Make (Hash : Hash.S) : S with type hash = Hash.t

  (** [Generic_key] generalises the concept of "node" to one that supports
      object keys that are not strictly equal to hashes. *)
  module Generic_key : sig
    module type S = S_generic_key
    module type Maker = Maker_generic_key
    module type Core = Core

    module Make : Maker

    module Make_v2 : Maker
    (** [Make_v2] provides a similar implementation as [Make] but the hash
        computation is compatible with versions older than brassaia.3.0 *)

    module Store
        (C : Contents.Store)
        (S : Indexable.S)
        (H : Hash.S with type t = S.hash)
        (V : S
               with type t = S.value
                and type hash = H.t
                and type contents_key = C.key
                and type node_key = S.key) :
      Store
        with type 'a t = 'a C.t * 'a S.t
         and type key = S.key
         and type hash = S.hash
         and type value = S.value
         and module Val = V
  end

  (** v1 serialisation *)
  module V1 (N : Generic_key.S) : sig
    include
      Generic_key.S
        with type contents_key = N.contents_key
         and type node_key = N.node_key

    val import : N.t -> t
    val export : t -> N.t
  end

  module Portable : sig
    (** Portable form of a node implementation that can be constructed from a
        concrete representation and used in computing hashes. Conceptually, a
        [Node.Portable.t] is a [Node.t] in which all internal keys have been
        replaced with the hashes of the values they point to.

        Computations over [Portable.t] values must commute with those over [t]s,
        as in the following diagram:

        {[
           ┌────────┐       ┌─────────┐  of_node   ┌────────────────┐
           │  Key   │       │  Node   │ ─────────> │ Node.Portable  │
           └────────┘       └─────────┘            └────────────────┘
             │    │  add/remove  │                         │
          to_hash └───────────> (+)     add/remove         │
             │    ┌──────────────┼──────────────────────> (+)
             v    │              v                         v
           ┌────────┐       ┌─────────┐            ┌────────────────┐
           │  Hash  │       │  Node'  │ ─────────> │ Node.Portable' │
           └────────┘       └─────────┘  of_node   └────────────────┘
        ]} *)

    (** A node implementation with hashes for keys is trivially portable: *)
    module Of_node (S : S) :
      Portable with type node := S.t and type t = S.t and type hash = S.hash

    module type S = Portable
  end

  module type Store = Store
  (** [Store] specifies the signature for node stores. *)

  (** [Store] creates node stores. *)
  module Store
      (C : Contents.Store)
      (S : Content_addressable.S with type key = C.key)
      (H : Hash.S with type t = S.key)
      (V : S with type t = S.value and type hash = S.key) :
    Store
      with type 'a t = 'a C.t * 'a S.t
       and type key = S.key
       and type value = S.value
       and type hash = H.t
       and module Val = V

  module type Graph = Graph
  (** [Graph] specifies the signature for node graphs. A node graph is a
      deterministic DAG, labeled by steps. *)

  module Graph (N : Store) :
    Graph
      with type 'a t = 'a N.t
       and type contents_key = N.Contents.key
       and type node_key = N.key
end
