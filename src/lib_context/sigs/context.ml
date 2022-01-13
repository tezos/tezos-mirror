(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

(** The tree depth of a fold. See the [View.fold] function for more
    information. *)
type depth = [`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int]

module type VIEW = sig
  (** The type for context views. *)
  type t

  (** The type for context keys. *)
  type key

  (** The type for context values. *)
  type value

  (** The type for context trees. *)
  type tree

  (** {2 Getters} *)

  (** [mem t k] is an Lwt promise that resolves to [true] iff [k] is bound
      to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [find t k] is an Lwt promise that resolves to [Some v] if [k] is
      bound to the value [v] in [t] and [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** [find_tree t k] is like {!find} but for trees. *)
  val find_tree : t -> key -> tree option Lwt.t

  (** [list t key] is the list of files and sub-nodes stored under [k] in [t].
      The result order is not specified but is stable.

      [offset] and [length] are used for pagination. *)
  val list :
    t -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t

  (** [length t key] is an Lwt promise that resolve to the number of
      files and sub-nodes stored under [k] in [t].

      It is equivalent to [list t k >|= List.length] but has a
      constant-time complexity. *)
  val length : t -> key -> int Lwt.t

  (** {2 Setters} *)

  (** [add t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is bound to [v] in [c];
    - and [c] is similar to [t] otherwise.

    If [k] was already bound in [t] to a value that is physically equal
    to [v], the result of the function is a promise that resolves to
    [t]. Otherwise, the previous binding of [k] in [t] disappears. *)
  val add : t -> key -> value -> t Lwt.t

  (** [add_tree] is like {!add} but for trees. *)
  val add_tree : t -> key -> tree -> t Lwt.t

  (** [remove t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is unbound in [c];
    - and [c] is similar to [t] otherwise. *)
  val remove : t -> key -> t Lwt.t

  (** {2 Folding} *)

  (** [fold ?depth t root ~order ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Le d] folds over nodes and contents of depth less than or equal to [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].
      - [Ge d] folds over nodes and contents of depth more than or equal to [d].

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. For large nodes, it is memory-consuming,
      use [`Undefined] for a more memory efficient [fold]. *)
  val fold :
    ?depth:depth ->
    t ->
    key ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(key -> tree -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
end

module Kind = struct
  type t = [`Value | `Tree]
end

module type TREE = sig
  (** [Tree] provides immutable, in-memory partial mirror of the
      context, with lazy reads and delayed writes. The trees are Merkle
      trees that carry the same hash as the part of the context they
      mirror.

      Trees are immutable and non-persistent (they disappear if the
      host crash), held in memory for efficiency, where reads are done
      lazily and writes are done only when needed, e.g. on
      [Context.commit]. If a key is modified twice, only the last
      value will be written to disk on commit. *)

  (** The type for context views. *)
  type t

  (** The type for context trees. *)
  type tree

  include VIEW with type t := tree and type tree := tree

  (** [empty _] is the empty tree. *)
  val empty : t -> tree

  (** [is_empty t] is true iff [t] is [empty _]. *)
  val is_empty : tree -> bool

  (** [kind t] is [t]'s kind. It's either a tree node or a leaf
      value. *)
  val kind : tree -> Kind.t

  (** [to_value t] is an Lwt promise that resolves to [Some v] if [t]
      is a leaf tree and [None] otherwise. It is equivalent to [find t
      []]. *)
  val to_value : tree -> value option Lwt.t

  (** [of_value _ v] is an Lwt promise that resolves to the leaf tree
      [v]. Is is equivalent to [add (empty _) [] v]. *)
  val of_value : t -> value -> tree Lwt.t

  (** [hash t] is [t]'s Merkle hash. *)
  val hash : tree -> Context_hash.t

  (** [equal x y] is true iff [x] and [y] have the same Merkle hash. *)
  val equal : tree -> tree -> bool

  (** {2 Caches} *)

  (** [clear ?depth t] clears all caches in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all of the subtrees are
      cleared. *)
  val clear : ?depth:int -> tree -> unit
end

module type HASH_VERSION = sig
  (** The type for context views. *)
  type t

  val get_hash_version : t -> Context_hash.Version.t

  val set_hash_version : t -> Context_hash.Version.t -> t Lwt.t
end

module Proof_types = struct
  (** The type for node segments. *)
  type segment = string

  (** The type for contents. *)
  type contents = bytes

  (** The type for hashes. *)
  type hash = Context_hash.t

  (** The type for (internal) inode proofs.

    These proofs encode large directories into a more efficient tree-like
    structure.

    Invariant are dependent on the backend.

    [length] is the total number of entries in the chidren of the inode.
    E.g. the size of the "flattened" version of that inode. This is used
    to efficiently implements paginated lists.

    [proofs] have a length of at most [Conf.entries] entries. This list can
    be sparse so every proof is indexed by their position between
    [0 ... (Conf.entries-1)]. For binary trees, this boolean
    index is a segment of the left-right decision proof corresponding
    to the path in that binary tree. *)
  type 'a inode = {length : int; proofs : (int * 'a) list}

  (** The type for inode extenders. *)
  type 'a inode_extender = {length : int; segments : int list; proof : 'a}
  [@@deriving irmin]

  (** The type for inode trees.

    Inodes are optimized representations of trees. Pointers in that trees
    would refer to blinded nodes, nodes or to other inodes. E.g.
    Blinded content nor contents are not expected to appear directly in
    an inode tree. *)
  type 'tree inode_tree =
    | Blinded_inode of hash
    | Inode_values of (segment * 'tree) list
    | Inode_tree of 'tree inode_tree inode
    | Inode_extender of 'tree inode_tree inode_extender
  [@@deriving irmin]

  (** The type for compressed and partial Merkle tree proofs.

    [Blinded_contents h] is a shallow pointer to contents having hash [h].
    [Contents c] is the contents [c].

    Tree proofs do not provide any guarantee with the ordering of
    computations. For instance, if two effects commute, they won't be
    distinguishable by this kind of proofs.

    [Blinded_node h] is a shallow pointer to a node having hash [h].

    [Node ls] is a "flat" node containing the list of files [ls]. The length
    of [ls]  is at most [Conf.stable_hash].

    [Inode i] is an optimized representation of a node as a tree.

 *)
  type tree =
    | Contents of contents
    | Blinded_contents of hash
    | Node of (segment * tree) list
    | Blinded_node of hash
    | Inode of tree inode_tree inode
    | Extender of tree inode_tree inode_extender
  [@@deriving irmin]

  (** The type for kinded hashes. *)
  type kinded_hash = [`Contents of Context_hash.t | `Node of Context_hash.t]

  (** The type for elements of stream proofs. *)
  type elt =
    | Contents of contents
    | Node of (segment * kinded_hash) list
    | Inode of hash inode
    | Inode_extender of hash inode_extender
  [@@deriving irmin]

  (** The type for stream proofs. Stream poofs provides stronger ordering
      guarantees as the read effects have to happen in the exact same order and
      they are easier to verify. *)
  type stream = elt Seq.t [@@deriving irmin]

  type 'a t = {before : kinded_hash; after : kinded_hash; state : 'a}
end

module type PROOF = sig
  (** Proofs are compact representations of trees which can be shared
    between a node and a client.

    The protocol is the following:

    - The node runs a function [f] over a tree [t]. While performing
      this computation, the node records: the hash of [t] (called [before]
      below), the hash of [f t] (called [after] below) and a subset of [t]
      which is needed to replay [f] without any access to the node's storage.
      Once done, the node packs this into a proof [p] and sends this to the
      client.

    - The client generates an initial tree [t'] from [p] and computes [f t'].
      Once done, it compares [t']'s hash and [f t']'s hash to [before] and
      [after]. If they match, they know that the result state [f t'] is a
      valid context state, without having to have access to the full node's
      storage. *)

  include
    module type of Proof_types
      with type 'a inode = 'a Proof_types.inode
       and type 'a inode_extender = 'a Proof_types.inode_extender
       and type 'a inode_tree = 'a Proof_types.inode_tree
       and type tree = Proof_types.tree
       and type elt = Proof_types.elt
       and type stream = Proof_types.stream
       and type 'a t = 'a Proof_types.t

  (** [t] proves that the state advanced from [before t] to [after t].
      [state t]'s hash is [before], and [state t] contains the minimal
      information for the computation to reach [after t]. *)

  (** [before t] it the state's hash at the beginning of the computation. *)
  val before : 'a t -> kinded_hash

  (** [after t] is the state's hash at the end of the computation. *)
  val after : 'a t -> kinded_hash

  (** [proof t] is a subset of the initial state needed to prove that the proven
      computation could run without performing any I/O. *)
  val state : 'a t -> 'a

  val v : before:kinded_hash -> after:kinded_hash -> 'a -> 'a t
end

module type S = sig
  include VIEW with type key = string list and type value = bytes

  module Proof : PROOF

  module Tree : sig
    include
      TREE
        with type t := t
         and type key := key
         and type value := value
         and type tree := tree

    (** [pp] is the pretty-printer for trees. *)
    val pp : Format.formatter -> tree -> unit

    (** {2 Data Encoding} *)

    (** The type for in-memory, raw contexts. *)
    type raw = [`Value of bytes | `Tree of raw TzString.Map.t]

    (** [raw_encoding] is the data encoding for raw trees. *)
    val raw_encoding : raw Data_encoding.t

    (** [to_raw t] is an Lwt promise that resolves to a raw tree
        equivalent to [t]. *)
    val to_raw : tree -> raw Lwt.t

    (** [of_raw t] is the tree equivalent to the raw tree [t]. *)
    val of_raw : raw -> tree

    (** The type of tree for which to build a shallow tree with [shallow] *)
    type kinded_hash := [`Contents of Context_hash.t | `Node of Context_hash.t]

    type repo

    val make_repo : unit -> repo Lwt.t

    val shallow : repo -> kinded_hash -> tree

    (** [produce r h f] runs [f] on top of a real store [r], producing a proof
        and a reulst using the initial root hash [h].

        The trees produced during [f]'s computation will carry the full history
        of reads. This history will be reset when [f] is complete so subtrees
        escaping the scope of [f] will not cause memory leaks.

        It is possible to call [produce_proof] recursively. In that case, each
        input trees will have their own history of reads and will contain only
        the reads needed to unshallow that corresponding trees. Proof trees
        proof should then interact as if they were all unshallowed (note: in the
        case of nested proofs, it's unclear what [verify_proof] should do...). *)
    type ('proof, 'result) producer :=
      repo ->
      kinded_hash ->
      (tree -> (tree * 'result) Lwt.t) ->
      ('proof * 'result) Lwt.t

    (** [verify t f] runs [f] in checking mode, loading data from the proof as
        needed.

        The generated tree is the tree after [f] has completed. More operations
        can be run on that tree, but it won't be able to access the underlying
        storage.

        Raise [Proof.Bad_proof] when the proof is rejected. *)
    type ('proof, 'result) verifier :=
      'proof ->
      (tree -> (tree * 'result) Lwt.t) ->
      (tree * 'result, [`Msg of string]) result Lwt.t

    (** The type for tree proofs.

        Guarantee that the given computation performs exactly the same state
        operations as the generating computation, *in some order*. *)
    type tree_proof := Proof.tree Proof.t

    (** [produce_proof] is the producer of tree proofs. *)
    val produce_proof : (tree_proof, 'a) producer

    (** [verify_proof] is the verifier of tree proofs. *)
    val verify_proof : (tree_proof, 'a) verifier

    (** The type for stream proofs.

        Guarantee that the given computation performs exactly the same state
        operations as the generating computation, in the exact same order.*in
        some order*. *)
    type stream_proof := Proof.stream Proof.t

    (** [produce_stream] is the producer of stream proofs. *)
    val produce_stream : (stream_proof, 'a) producer

    (** [verify_stream] is the verifier of stream proofs. *)
    val verify_stream : (stream_proof, 'a) verifier
  end
end
