(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** {2:lru-generic An LRU inter-blocks cache for the economic protocol} *)

(** {b This is for use *within* the {!module:Environment_context} only.} *)

(**

   Frequently used data should be kept in memory and persist along a
   chain of blocks. The caching mechanism allows the economic protocol
   to declare such data and to rely on an Least Recently Used strategy
   to keep the cache size under a fixed limit.

   One important difficulty is to correctly maintain this in-memory
   cache. There are three issues related to this mechanism from the
   shell perspective:

   1. The in-memory cache must take chain reorganization into account.
   (Chain reorganizations occur when the node's preferred chain is not
   the one that has been chosen by the consensus algorithm.)

   2. The in-memory cache must be completely determined by the
   storage.

   3. The loading of the cache should introduce minimal latency during
   node reboots, RPC processing, or other operations. The cache should
   also have similar reactivity time on every node, independently of
   the fact that the node has just been rebooted or not.

   This module implements the core logic of the cache mechanism (not
   the storage) part. The implementation provided to the protocol is
   implemented in {!module:Environment_context.Cache}.

   The type of a cache is parameterized by the type of the values
   stored.

   When a value is cached, we also stored metadata about the status of
   this value in the cache. The value's key and its metadata are the
   only data stored in the context. The set of all the keys and
   metadatas of the cache is called the *domain*.

   The actual cached values are introduced at runtime during cache
   lifetime.  When the node needs to load a cache in memory for a
   specific context, it uses a *builder*, i.e., a function which
   reconstructs a cached value from a key. We can reconstruct all the
   values in a cache given its domain. In practice, such builder is
   provided by the cache mechanism client, i.e., the economic
   protocol.

   Finally, notice that the cache is divided into sub-caches which
   have their own size limit. Each sub-cache is referenced by an index
   (as a positive integer). This layout is defined by the economic
   protocol.

*)

(** Abstract type for a cache parameterized by the value type. *)
type 'value t

(** {2 Cache layout} *)

(**

   A layout determines the number of sub-caches as well as the size limit
   of those sub-caches. No assumption is made on the unit of each of
   those sub-cache sizes. All we use is the natural ordering of [int].
   In particular, the size units for different sub-caches need not be the same.

   There are only two constructors for a cache:

    - {!val:uninitialised} which returns an uninitialised cache ;

    - {!val:from_layout} which initialises a cache with a layout.

   Moreover, the layout of the cache cannot be resized. This way, the
   cache's layout is determined uniquely by the one given to
   [from_layout].

   The only way to change the [layout] of a cache is to create a new
   cache and explicitly copy all the values from the previous cache to
   the new one. Such constraint may be relaxed in the future if
   necessary.

*)

(** Size of sub-caches or values. *)
type size = int

(** Index of the subcache in the cache. Values of this type should
    stay between [0] and [16383]. *)
type index = int

(** [uninitialised] is a special value which identify cache without a
   layout. Most functions of this interface will raise
   [Invalid_argument] is they are called on this value. *)
val uninitialised : 'value t

(** [from_layout layout] initializes a cache with the [layout]. Such
   function is intended to be called by the [init] function of the
   economic protocol. In particular, this means that the cache's values
   will be reset after stitching of context. *)
val from_layout : size list -> 'value t

(** [compatible_layout cache layout] returns [true] if the layout is
   compatible with the one [cache] was initialised with, [false]
   otherwise. By compatible, we mean that the layouts are actually
   precisely the same. *)
val compatible_layout : 'value t -> size list -> bool

(** [clear cache] resets the [cache] except the layout. It is
   equivalent to [from_layout layout] where [layout] was the
   [layout] provided to initialise the cache. *)
val clear : 'value t -> 'value t

(** {3 Keys} *)

(** A key is built from an identifier and an index of the corresponding cache. *)

(** Abstract type for a cache key. *)
type key

(** Identifier of a key. *)
type identifier = string

(** [key_of_identifier ~cache_index identifier] builds a key from the
   [cache_index] and the [identifier].

    No checks are made to ensure the validity of the index. *)
val key_of_identifier : cache_index:index -> identifier -> key

(** [identifier_of_key key] returns the identifier associated to the
   [key]. *)
val identifier_of_key : key -> identifier

(** Metadata associated to a value in the cache. *)
type value_metadata = {
  size : int;
      (** [size] must be strictly positive. This attribute is used to
          computed the cache size. The unit of the size is not specified at
          this stage. It is up to the economic protocol to give a measure
          of it. *)
  birth : int64;
      (** [birth] must be greater than 1. This number corresponds to the
          number of entries inserted in the cache after the insertion of
          this entry. The [birth] is used to determine which entry is the
          oldest one. The risk of overflow is minor. *)
  cache_nonce : Bytes.t;
      (** [cache_nonce] identifies the block that introduced this cache
          entry. The nonce must uniquely identify the block which
          modifies this value. It cannot be the block hash for circularity
          reasons: The value of the nonce is stored onto the context and
          consequently influences the context hash of the very same
          block. Such nonce cannot be determined by the shell and its
          computation is delegated to the economic protocol.

          [cache_nonce]s are used by the shell to properly relate
          the cached entries with the current block and its
          predecessors. In particular in case of reorganizations,
          that is a context which is not on the same branch as the
          current branch, [cache_nonce]s are used to filter out
          entries that do not exist in the target branch. *)
}

(** {3 Cache Getters/Setters} *)

(** [find cache key] is [Some v] if [key] has an associated cached
   value [v] in [cache], or [None] otherwise. *)
val find : 'value t -> key -> 'value option

(** [lookup cache key] is [Some (v, m)] where [v] is the value
   associated to [key] and [m] is the corresponding metadata. This
   function returns [None] if [key] is not in the cache domain. *)
val lookup : 'value t -> key -> ('value * value_metadata) option

(** [update cache key request] returns a new version of [cache]
    where a [request]ed change has been applied to the [key].

   More specifically:

   - [update cache key None] removes [key] from the [cache].

   - [update cache key (Some (value, size))] is [cache] where [key] is
     now associated to [value].

     The metadata of the [key] is also updated in the returned cache:
     - the [size] field is modified in the metadata ;
     - the [birth] is the higher birth, so that the key is the most
       recently used key.

     The [cache_nonce] of the entry is preserved.

    If the cache is full, the insertion of a new entry provokes the
    removal of the least recently used entries.

*)
val update : 'value t -> key -> ('value * size) option -> 'value t

(** [future_cache_expectation cache ~time_in_blocks] returns a
   predicted cache that tries to anticipate the state of [cache]
   in [time_in_blocks]. This function is using an heuristic. *)
val future_cache_expectation : 'value t -> time_in_blocks:int -> 'value t

(** {2 Cache synchronisation} *)

(**

   Synchronisation of a cache aims to be done once all the accesses to
   the caches have been done by the economic protocol.

   For each value added or updated since the last synchronisation,
   they are updated with the [nonce] provided by the economic
   protocol. *)

(** The domain is the on-disk representation of the cache. Notice
    that in-memory values must be constructed from a domain to get
    a cache. *)
type domain

val domain_encoding : domain Data_encoding.t

(** [empty_domain d] returns [true] iff [d] is the domain of an
    uninitialized cache. *)
val empty_domain : domain -> bool

(** [from_cache initial domain ~value_of_key] initializes a cache with
   the given [domain] by reusing values from [initial] if nonces
   match, or by calling [value_of_key] otherwise.

   [domain] and [initial] must share the same layout.

   This function is typically used when the cache is loaded from the
   context. See {!Environment_context}. *)
val from_cache :
  'value t ->
  domain ->
  value_of_key:(key -> ('value, 'trace) result Lwt.t) ->
  ('value t, 'trace) result Lwt.t

(** [sync cache ~cache_nonce] computes a new cache with a new domain. *)
val sync : 'value t -> cache_nonce:Bytes.t -> 'value t * domain

(** Various functions used to introspect the content of the cache. *)

(** [number_of_caches cache] returns the number of sub-caches in the
   cache. *)
val number_of_caches : 'value t -> int

(** [cache_size cache ~cache_index] returns an overapproximation of
    the size of the cache. Returns [None] if [cache_index] is
    an invalid index. *)
val cache_size : 'value t -> cache_index:int -> size option

(** [cache_size_limit cache ~cache_index] returns the maximal size of
    the cache indexed by [cache_index]. Returns [None] if
    [cache_index] is an invalid index. *)
val cache_size_limit : 'value t -> cache_index:int -> size option

(** [list_keys cache ~cache_index] returns the list of keys along with
   their size recorded into the subcache with index [cache_index]. *)
val list_keys : 'value t -> cache_index:index -> (key * size) list option

(** [key_rank cache key] returns the rank of the value associated to
   the given [key]. The rank is defined as the number of values older
   than the current one. Returns [None] if the [key] is not in the
   cache. *)
val key_rank : 'value t -> key -> int option

(** [pp fmt cache] is a pretty printter for a [cache]. *)
val pp : Format.formatter -> 'value t -> unit

(**/**)

module Internal_for_tests : sig
  val equal_domain : domain -> domain -> bool
end
