(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

module type S = sig
  type contents [@@deriving brassaia]

  type contents_key [@@deriving brassaia]

  type node [@@deriving brassaia]

  type hash [@@deriving brassaia]

  (** [Tree] provides immutable, in-memory partial mirror of the store, with
      lazy reads and delayed writes.

      Trees are like staging area in Git: they are immutable temporary
      non-persistent areas (they disappear if the host crash), held in memory
      for efficiency, where reads are done lazily and writes are done only when
      needed on commit: if you modify a key twice, only the last change will be
      written to the store when you commit. *)

  (** The type of trees. *)
  type t [@@deriving brassaia]

  (** {1 Constructors} *)

  (** [empty ()] is the empty tree. The empty tree does not have associated
      backend configuration values, as they can perform in-memory operation,
      independently of any given backend. *)
  val empty : unit -> t

  (** [singleton k c] is the tree with a single binding mapping the key [k] to
      the contents [c]. *)
  val singleton : Path.t -> contents -> t

  (** [of_contents c] is the subtree built from the contents [c]. *)
  val of_contents : contents -> t

  (** [of_node n] is the subtree built from the node [n]. *)
  val of_node : node -> t

  (** The type for tree elements. *)
  type elt = [`Node of node | `Contents of contents]

  (** General-purpose constructor for trees. *)
  val init : elt -> t

  type kinded_hash = [`Contents of hash | `Node of hash] [@@deriving brassaia]

  (** [pruned h] is a purely in-memory tree with the hash [h]. Such trees can be
      used as children of other in-memory tree nodes, for instance in order to
      compute the hash of the parent, but they cannot be dereferenced.

      Any operation that would require loading the contents of a pruned node
      (e.g. calling {!find} on one of its children) will instead raise a
      {!Pruned_hash} exception. Attempting to export a tree containing pruned
      sub-trees to a repository will fail similarly. *)
  val pruned : kinded_hash -> t

  (** [kind t k] is the type of [s] in [t]. It could either be a tree node or
      some file contents. It is [None] if [k] is not present in [t]. *)
  val kind : t -> Path.t -> [`Contents | `Node] option

  (** [is_empty t] is true iff [t] is {!val-empty} (i.e. a tree node with no
      children). Trees with {!kind} = [`Contents] are never considered empty. *)
  val is_empty : t -> bool

  (** {1 Diffs} *)

  (** [diff x y] is the difference of contents between [x] and [y]. *)
  val diff : t -> t -> (Path.t * contents Diff.t) list

  (** {1 Manipulating Contents} *)

  (** The exception raised by functions that can force lazy tree nodes but do
      not return an explicit {!or_error}. *)
  exception Dangling_hash of {context : string; hash : hash}

  (** The exception raised by functions that attempts to load {!pruned} tree
      nodes. *)
  exception Pruned_hash of {context : string; hash : hash}

  (** The exception raised by functions that attemps to perform IO on a portable
      tree. *)
  exception Portable_value of {context : string}

  type error = [`Dangling_hash of hash | `Pruned_hash of hash | `Portable_value]

  type 'a or_error = ('a, error) result

  (** Operations on lazy tree contents. *)
  module Contents : sig
    (** The type of lazy tree contents. *)
    type t

    (** [hash t] is the hash of the {!contents} value returned when [t] is
        {!val-force}d successfully. See {!caching} for an explanation of the
        [cache] parameter. *)
    val hash : ?cache:bool -> t -> hash

    (** [key t] is the key of the {!contents} value returned when [t] is
        {!val-force}d successfully. *)
    val key : t -> contents_key option

    (** [force t] forces evaluation of the lazy content value [t], or returns an
        error if no such value exists in the underlying repository. *)
    val force : t -> contents or_error

    (** Equivalent to {!val-force}, but raises an exception if the lazy content
        value is not present in the underlying repository. *)
    val force_exn : t -> contents

    (** [clear t] clears [t]'s cache. *)
    val clear : t -> unit

    (** {2:caching caching}

        [cache] regulates the caching behaviour regarding the node's internal
        data which are be lazily loaded from the backend.

        [cache] defaults to [true] which may greatly reduce the IOs and the
        runtime but may also grealy increase the memory consumption.

        [cache = false] doesn't replace a call to [clear], it only prevents the
        storing of new data, it doesn't discard the existing one. *)
  end

  (** [mem t k] is true iff [k] is associated to some contents in [t]. *)
  val mem : t -> Path.t -> bool

  (** [find_all t k] is [Some (b, m)] if [k] is associated to the contents [b]
      in [t] and [None] if [k] is not present in [t]. *)
  val find_all : t -> Path.t -> contents option

  (** [length t key] is the number of files and sub-nodes stored under [key] in
      [t].

      It is equivalent to [List.length (list t k)] but backends might optimise
      this call: for instance it's a constant time operation in [brassaia-pack].

      [cache] defaults to [true], see {!caching} for an explanation of the
      parameter.*)
  val length : t -> ?cache:bool -> Path.t -> int

  (** [find] is similar to {!find_all}. *)
  val find : t -> Path.t -> contents option

  (** Same as {!find_all} but raise [Invalid_arg] if [k] is not present in [t]. *)
  val get_all : t -> Path.t -> contents

  (** [list t key] is the list of files and sub-nodes stored under [k] in [t].
      The result order is not specified but is stable.

      [offset] and [length] are used for pagination.

      [cache] defaults to [true], see {!Contents.caching} for an explanation of
      the parameter. *)
  val list :
    t ->
    ?offset:int ->
    ?length:int ->
    ?cache:bool ->
    Path.t ->
    (Path.step * t) list

  (** [seq t key] follows the same behavior as {!list} but returns a sequence. *)
  val seq :
    t ->
    ?offset:int ->
    ?length:int ->
    ?cache:bool ->
    Path.t ->
    (Path.step * t) Seq.t

  (** Same as {!get_all} *)
  val get : t -> Path.t -> contents

  (** [add t k c] is the tree where the key [k] is bound to the contents [c] but
      is similar to [t] for other bindings. *)
  val add : t -> Path.t -> contents -> t

  (** [update t k f] is the tree [t'] that is the same as [t] for all keys
      except [k], and whose binding for [k] is determined by [f (find t k)].

      If [k] refers to an internal node of [t], [f] is called with [None] to
      determine the value with which to replace it. *)
  val update : t -> Path.t -> (contents option -> contents option) -> t

  (** [remove t k] is the tree where [k] bindings has been removed but is
      similar to [t] for other bindings. *)
  val remove : t -> Path.t -> t

  (** {1 Manipulating Subtrees} *)

  (** [mem_tree t k] is false iff [find_tree k = None]. *)
  val mem_tree : t -> Path.t -> bool

  (** [find_tree t k] is [Some v] if [k] is associated to [v] in [t]. It is
      [None] if [k] is not present in [t]. *)
  val find_tree : t -> Path.t -> t option

  (** [get_tree t k] is [v] if [k] is associated to [v] in [t]. Raise
      [Invalid_arg] if [k] is not present in [t].*)
  val get_tree : t -> Path.t -> t

  (** [add_tree t k v] is the tree where the key [k] is bound to the non-empty
      tree [v] but is similar to [t] for other bindings.

      If [v] is empty, this is equivalent to [remove t k]. *)
  val add_tree : t -> Path.t -> t -> t

  (** [update_tree t k f] is the tree [t'] that is the same as [t] for all
      subtrees except under [k], and whose subtree at [k] is determined by
      [f (find_tree t k)].

      [f] returning either [None] or [Some empty] causes the subtree at [k] to
      be unbound (i.e. it is equivalent to [remove t k]). *)
  val update_tree : t -> Path.t -> (t option -> t option) -> t

  (** [merge] is the 3-way merge function for trees. *)
  val merge : t Merge.t

  (** {1 Folds} *)

  (** General-purpose destructor for trees. *)
  val destruct : t -> [`Node of node | `Contents of Contents.t]

  (** The type for fold marks. *)
  type marks

  (** [empty_marks ()] is an empty collection of marks. *)
  val empty_marks : unit -> marks

  (** The type for {!fold}'s [force] parameter. [`True] forces the fold to read
      the objects of the lazy nodes and contents. [`False f] is applying [f] on
      every lazy node and content value instead. *)
  type 'a force = [`True | `False of Path.t -> 'a -> 'a]

  (** The type for {!fold}'s [uniq] parameters. [`False] folds over all the
      nodes. [`True] does not recurse on nodes already seen. [`Marks m] uses the
      collection of marks [m] to store the cache of keys: the fold will modify
      [m]. This can be used for incremental folds. *)
  type uniq = [`False | `True | `Marks of marks]

  (** The type for {!fold}'s folders: [pre], [post], [contents], [node], and
      [tree], where ['a] is the accumulator and ['b] is the item folded. *)
  type ('a, 'b) folder = Path.t -> 'b -> 'a -> 'a

  (** The type for fold depths.

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].

      [Le d] is [Eq d] and [Lt d]. [Ge d] is [Eq d] and [Gt d]. *)
  type depth = [`Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int]
  [@@deriving brassaia]

  (** [fold t acc] folds over [t]'s nodes with node-specific folders:
      [contents], [node], and [tree], based on a node's {!kind}.

      The default for all folders is identity.

      For every node [n] of [t], including itself:

      - If [n] is a [`Contents] kind, call [contents path c] where [c] is the
        {!contents} of [n].
      - If [n] is a [`Node] kind, (1) call [pre path steps]; (2) call
        [node path n]; (3) recursively fold on each child; (4) call
        [post path steps].
      - If [n] is any kind, call [tree path t'] where [t'] is the tree of [n].

      See
      {{:https://github.com/mirage/brassaia/blob/main/examples/fold.ml}
        examples/fold.ml} for a demo of the different {!folder}s.

      See {!force} for details about the [force] parameters. By default it is
      [`True].

      See {!uniq} for details about the [uniq] parameters. By default it is
      [`False].

      The fold depth is controlled by the [depth] parameter.

      [cache] defaults to [false], see {!Contents.caching} for an explanation of
      the parameter.

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. If [`Random state], they are traversed
      in a random order. For large nodes, these two modes are memory-consuming,
      use [`Undefined] for a more memory efficient [fold]. *)
  val fold :
    ?order:[`Sorted | `Undefined | `Random of Random.State.t] ->
    ?force:'a force ->
    ?cache:bool ->
    ?uniq:uniq ->
    ?pre:('a, Path.step list) folder ->
    ?post:('a, Path.step list) folder ->
    ?depth:depth ->
    ?contents:('a, contents) folder ->
    ?node:('a, node) folder ->
    ?tree:('a, t) folder ->
    t ->
    'a ->
    'a

  (** {1 Stats} *)

  (** The type for tree stats. *)
  type stats = {
    nodes : int;  (** Number of node. *)
    leafs : int;  (** Number of leafs. *)
    skips : int;  (** Number of lazy nodes. *)
    depth : int;  (** Maximal depth. *)
    width : int;  (** Maximal width. *)
  }
  [@@deriving brassaia]

  (** [stats ~force t] are [t]'s statistics. If [force] is true, this will force
      the reading of lazy nodes. By default it is [false]. *)
  val stats : ?force:bool -> t -> stats

  (** {1 Concrete Trees} *)

  (** The type for concrete trees. *)
  type concrete = [`Tree of (Path.step * concrete) list | `Contents of contents]
  [@@deriving brassaia]

  (** [of_concrete c] is the subtree equivalent of the concrete tree [c].

      @raise Invalid_argument
        if [c] contains duplicate bindings for a given path. *)
  val of_concrete : concrete -> t

  (** [to_concrete t] is the concrete tree equivalent of the subtree [t]. *)
  val to_concrete : t -> concrete

  (** {1 Proofs} *)

  module Proof : sig
    include Proof.S with type contents := contents and type hash := hash

    type brassaia_tree

    (** [to_tree p] is the tree [t] representing the tree proof [p]. Blinded
        parts of the proof will raise [Dangling_hash] when traversed. *)
    val to_tree : t -> brassaia_tree
  end
  with type brassaia_tree := t

  (** {1 Caches} *)

  (** [clear ?depth t] clears all caches in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all of the subtrees are
      cleared.

      A call to [clear] doesn't discard the subtrees of [t], only their cache
      are discarded. Even the lazily loaded and unmodified subtrees remain. *)
  val clear : ?depth:int -> t -> unit

  (** {1 Performance counters} *)

  type counters = {
    contents_hash : int;
    contents_find : int;
    contents_add : int;
    contents_mem : int;
    node_hash : int;
    node_mem : int;
    node_index : int;
    node_add : int;
    node_find : int;
    node_val_v : int;
    node_val_find : int;
    node_val_list : int;
  }

  val counters : unit -> counters

  val dump_counters : unit Fmt.t

  val reset_counters : unit -> unit

  (** [inspect t] is similar to {!kind}, with additional state information for
      nodes. It is primarily useful for debugging and testing.

      If [t] holds a node, additional information about its state is included:

      - [`Map], if [t] is from {!of_concrete}.
      - [`Value], if [t]'s node has modifications that have not been persisted
        to a store.
      - [`Portable_dirty], if [t]'s node has modifications and is
        {!Node.Portable}. Currently only used with {!Proof}.
      - [`Pruned], if [t] is from {!pruned}.
      - Otherwise [`Key], the default state for a node loaded from a store. *)
  val inspect :
    t ->
    [`Contents | `Node of [`Map | `Key | `Value | `Portable_dirty | `Pruned]]

  module Private : sig
    module Env : sig
      type t [@@deriving brassaia]

      val is_empty : t -> bool
    end

    val get_env : t -> Env.t
  end
end

module type Sigs = sig
  module type S = sig
    (** @inline *)
    include S
  end

  module Make (B : Backend.S) : sig
    include
      S
        with type contents = B.Contents.value
         and type contents_key = B.Contents.Key.t
         and type hash = B.Hash.t

    type kinded_key = [`Contents of B.Contents.Key.t | `Node of B.Node.Key.t]
    [@@deriving brassaia]

    val import : B.Repo.t -> kinded_key -> t option

    val import_no_check : B.Repo.t -> kinded_key -> t

    val export :
      ?clear:bool ->
      B.Repo.t ->
      [> write] B.Contents.t ->
      [> read_write] B.Node.t ->
      node ->
      B.Node.key

    val dump : t Fmt.t

    val equal : t -> t -> bool

    val key : t -> kinded_key option

    val hash : ?cache:bool -> t -> kinded_hash

    val to_backend_node : node -> B.Node.Val.t

    val to_backend_portable_node : node -> B.Node_portable.t

    val of_backend_node : B.Repo.t -> B.Node.value -> node

    type 'result producer :=
      B.Repo.t -> kinded_key -> (t -> t * 'result) -> Proof.t * 'result

    type verifier_error = [`Proof_mismatch of string] [@@deriving brassaia]

    type 'result verifier :=
      Proof.t -> (t -> t * 'result) -> (t * 'result, verifier_error) result

    val produce_proof : 'a producer

    val verify_proof : 'a verifier

    val hash_of_proof_state : Proof.tree -> kinded_hash
  end
end
