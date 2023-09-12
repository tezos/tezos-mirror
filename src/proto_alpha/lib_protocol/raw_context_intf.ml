(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** All context manipulation functions. This signature is included
    as-is for direct context accesses, and used in {!Storage_functors}
    to provide restricted views to the context. *)

(** The tree depth of a fold. See the [fold] function for more information. *)
type depth = [`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int]

(** The type for context configuration. If two trees or stores have the
    same configuration, they will generate the same context hash. *)
type config = Context.config

module type VIEW = sig
  (* Same as [Environment_context.VIEW] but with extra getters and
     setters functions. *)

  (** The type for context views. *)
  type t

  (** The type for context keys. *)
  type key = string list

  (** The type for context values. *)
  type value = bytes

  (** The type for context trees. *)
  type tree

  (** {2 Getters} *)

  (** [mem t k] is an Lwt promise that resolves to [true] iff [k] is bound
      to a value in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [mem_tree t k] is like {!mem} but for trees. *)
  val mem_tree : t -> key -> bool Lwt.t

  (** [get t k] is an Lwt promise that resolves to [Ok v] if [k] is
      bound to the value [v] in [t] and {!Storage_Error Missing_key}
      otherwise. *)
  val get : t -> key -> value tzresult Lwt.t

  (** [get_tree] is like {!get} but for trees. *)
  val get_tree : t -> key -> tree tzresult Lwt.t

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

  (** {2 Setters} *)

  (** [init t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is unbound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Existing_key} if [k] is already bound in [t]. *)
  val init : t -> key -> value -> t tzresult Lwt.t

  (** [init_tree] is like {!init} but for trees. *)
  val init_tree : t -> key -> tree -> t tzresult Lwt.t

  (** [update t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is bound in [t];
      - [k] is bound to [v] in [c];
      - and [c] is similar to [t] otherwise.

      It is {!Storage_error Missing_key} if [k] is not already bound in [t]. *)
  val update : t -> key -> value -> t tzresult Lwt.t

  (** [update_tree] is like {!update} but for trees. *)
  val update_tree : t -> key -> tree -> t tzresult Lwt.t

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

  (** [remove_existing t k v] is an Lwt promise that resolves to [Ok c] if:

      - [k] is bound in [t] to a value;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing : t -> key -> t tzresult Lwt.t

  (** [remove_existing_tree t k v] is an Lwt promise that reolves to [Ok c] if:

      - [k] is bound in [t] to a tree;
      - [k] is unbound in [c];
      - and [c] is similar to [t] otherwise.*)
  val remove_existing_tree : t -> key -> t tzresult Lwt.t

  (** [add_or_remove t k v] is:

      - [add t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove : t -> key -> value option -> t Lwt.t

  (** [add_or_remove_tree t k v] is:

      - [add_tree t k x] if [v] is [Some x];
      - [remove t k] otherwise. *)
  val add_or_remove_tree : t -> key -> tree option -> t Lwt.t

  (** {2 Folds} *)

  (** [fold ?depth t root ~order ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and values of depth exactly [d].
      - [Lt d] folds over nodes and values of depth strictly less than [d].
      - [Le d] folds over nodes and values of depth less than or equal to [d].
      - [Gt d] folds over nodes and values of depth strictly more than [d].
      - [Ge d] folds over nodes and values of depth more than or equal to [d].

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

  (** {2 Hash configurations} *)

  (** [config t] is [t]'s hash configuration. *)
  val config : t -> config

  (** [length t key] is an Lwt promise that resolves to the number of files and
      sub-nodes stored under [k] in [t].

      It is equivalent to [let+ l = list t k in List.length l] but has a constant-time
      complexity.

      Most of the time, this function does not perform any I/O as the length is
      cached in the tree. It may perform one read to load the root node of the
      tree in case it has not been loaded already. The initial constant is the
      same between [list] and [length]. They both perform the same kind of I/O
      reads. While [list] usually performs a linear number of reads, [length]
      does at most one. *)
  val length : t -> key -> int Lwt.t
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

module type PROOF = sig
  (** Proofs are compact representations of trees which can be shared
      between peers.

      This is expected to be used as follows:

      - A first peer runs a function [f] over a tree [t]. While performing
        this computation, it records: the hash of [t] (called [before]
        below), the hash of [f t] (called [after] below) and a subset of [t]
        which is needed to replay [f] without any access to the first peer's
        storage. Once done, all these informations are packed into a proof of
        type [t] that is sent to the second peer.

      - The second peer generates an initial tree [t'] from [p] and computes
        [f t']. Once done, it compares [t']'s hash and [f t']'s hash to [before]
        and [after]. If they match, they know that the result state [f t'] is a
        valid context state, without having to have access to the full storage
        of the first peer. *)

  (** The type for file and directory names. *)
  type step = string

  (** The type for values. *)
  type value = bytes

  (** The type of indices for inodes' children. *)
  type index = int

  (** The type for hashes. *)
  type hash = Context_hash.t

  (** The type for (internal) inode proofs.

      These proofs encode large directories into a tree-like structure. This
      reflects irmin-pack's way of representing nodes and computing
      hashes (tree-like representations for nodes scales better than flat
      representations).

      [length] is the total number of entries in the children of the inode.
      It's the size of the "flattened" version of that inode. [length] can be
      used to prove the correctness of operations such [Tree.length] and
      [Tree.list ~offset ~length] in an efficient way.

      In proofs with [version.is_binary = false], an inode at depth 0 has a
      [length] of at least [257]. Below that threshold a [Node] tag is used in
      [tree]. That threshold is [3] when [version.is_binary = true].

      [proofs] contains the children proofs. It is a sparse list of ['a] values.
      These values are associated to their index in the list, and the list is
      kept sorted in increasing order of indices. ['a] can be a concrete proof
      or a hash of that proof.

      In proofs with [version.is_binary = true], inodes have at most 2 proofs
      (indexed 0 or 1).

      In proofs with [version.is_binary = false], inodes have at most 32 proofs
      (indexed from 0 to 31). *)
  type 'a inode = {length : int; proofs : (index * 'a) list}

  (** The type for inode extenders.

      An extender is a compact representation of a sequence of [inode] which
      contain only one child. As for inodes, The ['a] parameter can be a
      concrete proof or a hash of that proof.

      If an inode proof contains singleton children [i_0, ..., i_n] such as:
      [{length=l; proofs = [ (i_0, {proofs = ... { proofs = [ (i_n, p) ] }})]}],
      then it is compressed into the inode extender
      [{length=l; segment = [i_0;..;i_n]; proof=p}] sharing the same lenght [l]
      and final proof [p]. *)
  type 'a inode_extender = {length : int; segment : index list; proof : 'a}

  (** The type for compressed and partial Merkle tree proofs.

      Tree proofs do not provide any guarantee with the ordering of
      computations. For instance, if two effects commute, they won't be
      distinguishable by this kind of proofs.

      [Value v] proves that a value [v] exists in the store.

      [Blinded_value h] proves a value with hash [h] exists in the store.

      [Node ls] proves that a a "flat" node containing the list of files [ls]
      exists in the store.

      In proofs with [version.is_binary = true], the length of [ls] is at most
      2.

      In proofs with [version.is_binary = false], the length of [ls] is at most
      256.

      [Blinded_node h] proves that a node with hash [h] exists in the store.

      [Inode i] proves that an inode [i] exists in the store.

      [Extender e] proves that an inode extender [e] exist in the store. *)
  type tree =
    | Value of value
    | Blinded_value of hash
    | Node of (step * tree) list
    | Blinded_node of hash
    | Inode of inode_tree inode
    | Extender of inode_tree inode_extender

  (** The type for inode trees. It is a subset of [tree], limited to nodes.

      [Blinded_inode h] proves that an inode with hash [h] exists in the store.

      [Inode_values ls] is similar to trees' [Node].

      [Inode_tree i] is similar to tree's [Inode].

      [Inode_extender e] is similar to trees' [Extender].  *)
  and inode_tree =
    | Blinded_inode of hash
    | Inode_values of (step * tree) list
    | Inode_tree of inode_tree inode
    | Inode_extender of inode_tree inode_extender

  (** The type for kinded hashes. *)
  type kinded_hash = [`Value of hash | `Node of hash]

  module Stream : sig
    (** Stream proofs represent an explicit traversal of a Merle tree proof.
        Every element (a node, a value, or a shallow pointer) met is first
        "compressed" by shallowing its children and then recorded in the proof.

        As stream proofs directly encode the recursive construction of the
        Merkle root hash is slightly simpler to implement: verifier simply
        need to hash the compressed elements lazily, without any memory or
        choice.

        Moreover, the minimality of stream proofs is trivial to check.
        Once the computation has consumed the compressed elements required,
        it is sufficient to check that no more compressed elements remain
        in the proof.

        However, as the compressed elements contain all the hashes of their
        shallow children, the size of stream proofs is larger
        (at least double in size in practice) than tree proofs, which only
        contains the hash for intermediate shallow pointers. *)

    (** The type for elements of stream proofs.

        [Value v] is a proof that the next element read in the store is the
        value [v].

        [Node n] is a proof that the next element read in the store is the
        node [n].

        [Inode i] is a proof that the next element read in the store is the
        inode [i].

        [Inode_extender e] is a proof that the next element read in the store
        is the node extender [e]. *)
    type elt =
      | Value of value
      | Node of (step * kinded_hash) list
      | Inode of hash inode
      | Inode_extender of hash inode_extender

    (** The type for stream proofs.

        The sequence [e_1 ... e_n] proves that the [e_1], ..., [e_n] are
        read in the store in sequence. *)
    type t = elt Seq.t
  end

  type stream = Stream.t

  (** The type for proofs of kind ['a].

      A proof [p] proves that the state advanced from [before p] to
      [after p]. [state p]'s hash is [before p], and [state p] contains
      the minimal information for the computation to reach [after p].

      [version p] is the proof version, it packs several informations.

      [is_stream] discriminates between the stream proofs and the tree proofs.

      [is_binary] discriminates between proofs emitted from
      [Tezos_context(_memory).Context_binary] and
      [Tezos_context(_memory).Context].

      It will also help discriminate between the data encoding techniques used.

      The version is meant to be decoded and encoded using the
      {!Tezos_context_helpers.Context.decode_proof_version} and
      {!Tezos_context_helpers.Context.encode_proof_version}. *)
  type 'a t = {
    version : int;
    before : kinded_hash;
    after : kinded_hash;
    state : 'a;
  }
end

module type T = sig
  (** The type for root contexts. *)
  type root

  include VIEW

  module Tree :
    TREE
      with type t := t
       and type key := key
       and type value := value
       and type tree := tree

  module Proof : PROOF

  (** [verify p f] runs [f] in checking mode. [f] is a function that takes a
      tree as input and returns a new version of the tree and a result. [p] is a
      proof, that is a minimal representation of the tree that contains what [f]
      should be expecting.

      Therefore, contrary to trees found in a storage, the contents of the trees
      passed to [f] may not be available. For this reason, looking up a value at
      some [path] can now produce three distinct outcomes:
      - A value [v] is present in the proof [p] and returned : [find tree path]
        is a promise returning [Some v];
      - [path] is known to have no value in [tree] : [find tree path] is a
        promise returning [None]; and
      - [path] is known to have a value in [tree] but [p] does not provide it
        because [f] should not need it: [verify] returns an error classifying
        [path] as an invalid path (see below).

      The same semantics apply to all operations on the tree [t] passed to [f]
      and on all operations on the trees built from [f].

      The generated tree is the tree after [f] has completed. That tree is
      disconnected from any storage (i.e. [index]). It is possible to run
      operations on it as long as they don't require loading shallowed subtrees.

      The result is [Error (`Msg _)] if the proof is rejected:
      - For tree proofs: when [p.before] is different from the hash of
        [p.state];
      - For tree and stream proofs: when [p.after] is different from the hash
        of [f p.state];
      - For tree proofs: when [f p.state] tries to access invalid paths in
        [p.state];
      - For stream proofs: when the proof is not consumed in the exact same
        order it was produced;
      - For stream proofs: when the proof is too short or not empty once [f] is
        done.

      @raise Failure if the proof version is invalid or incompatible with the
      verifier. *)
  type ('proof, 'result) verifier :=
    'proof ->
    (tree -> (tree * 'result) Lwt.t) ->
    ( tree * 'result,
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ] )
    result
    Lwt.t

  (** The type for tree proofs.

      Guarantee that the given computation performs exactly the same state
      operations as the generating computation, *in some order*. *)
  type tree_proof := Proof.tree Proof.t

  (** [verify_tree_proof] is the verifier of tree proofs. *)
  val verify_tree_proof : (tree_proof, 'a) verifier

  (** The type for stream proofs.

      Guarantee that the given computation performs exactly the same state
      operations as the generating computation, in the exact same order. *)
  type stream_proof := Proof.stream Proof.t

  (** [verify_stream] is the verifier of stream proofs. *)
  val verify_stream_proof : (stream_proof, 'a) verifier

  (** The equality function for context configurations. If two context have the
      same configuration, they will generate the same context hashes. *)
  val equal_config : config -> config -> bool

  (** Internally used in {!Storage_functors} to escape from a view. *)
  val project : t -> root

  (** Internally used in {!Storage_functors} to retrieve a full key
      from partial key relative a view. *)
  val absolute_key : t -> key -> key

  (** Raised if block gas quota is exhausted during gas
     consumption. *)
  type error += Block_quota_exceeded

  (** Raised if operation gas quota is exhausted during gas
     consumption. *)
  type error += Operation_quota_exceeded

  (** Internally used in {!Storage_functors} to consume gas from
     within a view. May raise {!Block_quota_exceeded} or
     {!Operation_quota_exceeded}. *)
  val consume_gas : t -> Gas_limit_repr.cost -> t tzresult

  (** Check if consume_gas will fail *)
  val check_enough_gas : t -> Gas_limit_repr.cost -> unit tzresult

  val description : t Storage_description.t

  (** The type for local context accesses instead from the root. In order for
      the carbonated storage functions to consume the gas, this has gas
      infomation *)
  type local_context

  (**
     [with_local_context ctxt key f] runs function [f] over the local
     context at path [key] of the global [ctxt].  Using the local context [f]
     can perform faster context accesses under [key].
  *)
  val with_local_context :
    t ->
    key ->
    (local_context -> (local_context * 'a) tzresult Lwt.t) ->
    (t * 'a) tzresult Lwt.t

  (** [Local_context] provides functions for local access from a specific
      directory. *)
  module Local_context : sig
    include
      VIEW
        with type t = local_context
         and type tree := tree
         and type key := key
         and type value := value

    (** Internally used in {!Storage_functors} to consume gas from
        within a view. May raise {!Block_quota_exceeded} or
        {!Operation_quota_exceeded}. *)
    val consume_gas :
      local_context -> Gas_limit_repr.cost -> local_context tzresult

    (** Internally used in {!Storage_functors} to retrieve the full key of a
        partial key relative to the [local_context]. *)
    val absolute_key : local_context -> key -> key
  end
end
