(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

(* Copy/paste of Environment_context_sigs.Context.S *)

(** The tree depth of a fold. See the [fold] function for more information. *)
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

  (** [fold ?depth t root ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      Elements are traversed in lexical order of keys.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Le d] folds over nodes and contents of depth less than or equal to [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].
      - [Ge d] folds over nodes and contents of depth more than or equal to [d].

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. For large nodes, these two modes are memory-consuming,
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

module Kind : sig
  type t = [`Value | `Tree]
end

module type TREE = sig
  (** [Tree] provides immutable, in-memory partial mirror of the
      context, with lazy reads and delayed writes.

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

include VIEW with type key = string list and type value = bytes

module Tree :
  TREE
    with type t := t
     and type key := key
     and type value := value
     and type tree := tree

val register_resolver :
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete : t -> string -> string list Lwt.t

(** Get the hash version used for the context *)
val get_hash_version : t -> Context_hash.Version.t

(** Set the hash version used for the context.  It may recalculate the hashes
    of the whole context, which can be a long process.
    Returns an Error if the hash version is unsupported. *)
val set_hash_version :
  t -> Context_hash.Version.t -> t Error_monad.shell_tzresult Lwt.t

type cache_key

type cache_value = ..

module type CACHE = sig
  (** Type for context view. A context contains a cache. A cache is
     made of subcaches. Each subcache has its own size limit. The
     limit of its subcache is called a layout and can be initialized
     via the [set_cache_layout] function. *)
  type t

  (** Size for subcaches and values of the cache. Units are not
     specified and left to the economic protocol. *)
  type size

  (** Index type to index caches. *)
  type index

  (** Identifier type for keys. *)
  type identifier

  (** A key uniquely identifies a cached [value] in some subcache. *)
  type key

  (** Cached values inhabit an extensible type. *)
  type value = ..

  (** [key_of_identifier ~cache_index identifier] builds a key from the
      [cache_index] and the [identifier].

      No check are made to ensure the validity of the index.  *)
  val key_of_identifier : cache_index:index -> identifier -> key

  (** [identifier_of_key key] returns the identifier associated to the
      [key]. *)
  val identifier_of_key : key -> identifier

  (** [pp fmt cache] is a pretty printter for a [cache]. *)
  val pp : Format.formatter -> t -> unit

  (** [find ctxt k = Some v] if [v] is the value associated to [k] in
     in the cache where [k] is. Returns [None] if there is no such
     value in the cache of [k].  This function is in the Lwt monad
     because if the value has not been constructed, it is constructed
     on the fly. *)
  val find : t -> key -> value option Lwt.t

  (** [set_cache_layout ctxt layout] sets the caches of [ctxt] to
     comply with given [layout]. If there was already a cache in
     [ctxt], it is erased by the new layout.

     Otherwise, a fresh collection of empty caches is reconstructed
     from the new [layout]. Notice that cache [key]s are invalidated
     in that case, i.e., [get t k] will return [None]. *)
  val set_cache_layout : t -> size list -> t Lwt.t

  (** [update ctxt k (Some (e, size))] returns a cache where the value
      [e] of [size] is associated to key [k]. If [k] is already in the
      cache, the cache entry is updated.

      [update ctxt k None] removes [k] from the cache. *)
  val update : t -> key -> (value * size) option -> t

  (** [sync ctxt ~cache_nonce] updates the context with the domain of
     the cache computed so far. Such function is expected to be called
     at the end of the validation of a block, when there is no more
     accesses to the cache.

     [cache_nonce] identifies the block that introduced new cache
     entries. The nonce should identify uniquely the block which
     modifies this value. It cannot be the block hash for circularity
     reasons: The value of the nonce is stored onto the context and
     consequently influences the context hash of the very same
     block. Such nonce cannot be determined by the shell and its
     computation is delegated to the economic protocol.
  *)
  val sync : t -> cache_nonce:Bytes.t -> t Lwt.t

  (** [clear ctxt] removes all cache entries. *)
  val clear : t -> t

  (** {3 Cache introspection} *)

  (** [list_keys ctxt ~cache_index] returns the list of cached keys in
     cache numbered [cache_index] along with their respective
     [size]. The returned list is sorted in terms of their age in the
     cache, the oldest coming first. If [cache_index] is invalid,
     then this function returns [None]. *)
  val list_keys : t -> cache_index:index -> (key * size) list option

  (** [key_rank index ctxt key] returns the number of cached value older
       than the given [key]; or, [None] if the [key] is not a cache key. *)
  val key_rank : t -> key -> int option

  (** {3 Cache helpers for RPCs} *)

  (** [future_cache_expectation ctxt ~time_in_blocks] returns [ctxt] except
      that the entries of the caches that are presumably too old to
      still be in the caches in [n_blocks] are removed.

      This function is based on a heuristic. The context maintains
      the median of the number of removed entries: this number is
      multipled by `n_blocks` to determine the entries that are
      likely to be removed in `n_blocks`. *)
  val future_cache_expectation : t -> time_in_blocks:int -> t

  (** [cache_size ctxt ~cache_index] returns an overapproximation of
      the size of the cache. Returns [None] if [cache_index] is not a
      valid cache index. *)
  val cache_size : t -> cache_index:index -> size option

  (** [cache_size_limit ctxt ~cache_index] returns the maximal size of
      the cache indexed by [cache_index]. Returns [None] if
      [cache_index] is not a valid cache index. *)
  val cache_size_limit : t -> cache_index:index -> size option
end

module Cache :
  CACHE
    with type t := t
     and type size := int
     and type index := int
     and type identifier := string
     and type key = cache_key
     and type value = cache_value
