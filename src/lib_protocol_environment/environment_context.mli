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

include Environment_context_intf.Sigs

module Equality_witness : sig
  type (_, _) eq = Refl : ('a, 'a) eq

  type 'a t

  val make : unit -> 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option

  val hash : 'a t -> int
end

module Context : sig
  type ('ctxt, 'tree) ops = (module S with type t = 'ctxt and type tree = 'tree)

  type _ kind = private ..

  type ('a, 'b) equality_witness

  (** Abstract type of a cache. A cache is made of subcaches. Each
     subcache has its own size limit. The limit of its subcache is
     called a layout and can be initialized via the [set_cache_layout]
     function. *)
  type cache

  (** A context is purely functional description of a state. This
      state is used to interpret operations, and more generally,
      to validate blocks.

      This type is private because a context must be constructed
      using [make], which is a smart constructor.
  *)
  type t = private
    | Context : {
        kind : 'a kind;
        impl_name : string;
        ctxt : 'a;
        ops : ('a, 'b) ops;
        equality_witness : ('a, 'b) equality_witness;
        cache : cache;
      }
        -> t

  include S with type t := t

  (** [make kind impl_name ctxt ops equality_witness] builds a context
     value. In this context, the cache is uninitialized: one must call
     [load_cache] to obtain a context with a valid cache. Otherwise,
     the context is not usable for all protocol-level features based
     on the cache, e.g., smart contract execution. *)
  val make :
    kind:'a kind ->
    impl_name:string ->
    ctxt:'a ->
    ops:('a, 'b) ops ->
    equality_witness:('a, 'b) equality_witness ->
    t

  (** A key uniquely identifies a cached [value] in the some subcache. *)
  type cache_key

  (** Abstract type for cached values.

      This type is an extensible type since values stored in the cache
      are heterogeneous. Notice that the cache must be cleared during
      during protocol stitching because the data constructors of this
      type are incompatible between two protocols: if there remains
      values built with a data constructor of an old protocol, the new
      protocol will be confused to find that some keys it is interesting
      in have unexploitable values.

  *)

  (** Cached values inhabit an extensible type. *)
  type cache_value = ..

  (** See [Context.CACHE] in [sigs/v3/context.mli] for documentation. *)
  module Cache : sig
    include
      CACHE
        with type t := t
         and type size = int
         and type index = int
         and type identifier = string
         and type key = cache_key
         and type value = cache_value

    module Internal_for_tests : sig
      (** [same_cache_domains ctxt ctxt'] returns [true] iff the caches
          of the two contexts share the same domain. *)
      val same_cache_domains : t -> t -> (bool, 'a) result Lwt.t
    end
  end

  (** A cache is a block-dependent value: to know whether a cache can
     be reused or recycled in a given block, we need the block that
     produces it. *)
  type block_cache = {
    context_hash : Tezos_crypto.Hashed.Context_hash.t;
    cache : cache;
  }

  (** During its loading, a cache can be populated in two different
     ways:

      - values are computed immediately via the builder and inserted
     into the cache ; or,

      - the computation of the values is delayed and will be computed
     only when such value is required.

      The first mode is intended to be used after a rebooting of the
     node for example. The main benefit being that it does not impact
     the validation time of a block since the cache's values will be
     reconstructed beforehand. The second mode is intended to be used
     for RPCs where reactivity is important: we do not want to
     recompute the full cache to execute the RPC but only the values
     which are necessary. *)

  type source_of_cache =
    [ `Force_load  (** Force the cache domain to be reloaded from the context. *)
    | `Load
      (** Load a cache by iterating over the keys of its domain and by
          building a cached value for each key.

          This operation can introduce a significant slowdown
          proportional to the number of entries in the cache, and depending on
          their nature. As a consequence, loading a cache from that
          source should be done when the system has no  strict constraint
          on execution time, e.g., during startup.

      *)
    | `Lazy
      (** Same as Load except that cached values are built on demand.

          This strategy makes [load_cache] run a lot faster and the
          overall cost of loading the cache is only proportional to
          the number of entries *actually used* (and also depends on
          their nature).

          Notice that, contrary to the [`Load] source of cache, this
          loading mode may also introduce latencies when entries are
          actually used since they are reconstructed on-the-fly.

          RPCs are a typical place where this Lazy loading makes
          sense since the number of entries used is generally low,
          and the cache cannot be inherited (as in the next case).

      *)
    | `Inherited of block_cache * Tezos_crypto.Hashed.Context_hash.t
      (** When we already have some [block_cache.cache] in memory coming
          from the validation of some block [block_cache.context_hash],
          we can reuse or recycle its entries to reconstruct a cache to
          check some other block identified by a given [Context_hash.t],
          which typically comes after [block_cache.context_hash] in the
          chain.

          This source is usually the most efficient way to build a
          cache in memory since the cache entries only change
          marginally from one block to one of its close descendants.

      *)
    ]

  (** To [load_cache] in memory, we need to iterate over its domain
     and for each key found in the domain, a [builder] produces the
     associated value. *)
  type builder = cache_key -> cache_value tzresult Lwt.t

  (** [load_cache predecessor ctxt source builder] populates the
     in-memory cache values cached in the current context during the
     validation of [predecessor] block. To achieve that, the function
     uses the strategy described by [source], exploiting the [builder]
     to create cached values that are not already available in memory.

     The [builder] is assumed to never fail when evaluated on the keys
     of the cache domain. Indeed, if a key had an associated value in
     the cache at some point in the past, it should have been a valid
     key. In other words, the construction of cache should be
     reproducible. For this reason, an error in [builder] is fatal. *)
  val load_cache :
    Tezos_crypto.Hashed.Block_hash.t ->
    t ->
    source_of_cache ->
    builder ->
    t tzresult Lwt.t
end

module Register (C : S) : sig
  type _ Context.kind += Context : C.t Context.kind

  val equality_witness : (C.t, C.tree) Context.equality_witness

  val ops : (C.t, C.tree) Context.ops
end

type legacy_validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

type validation_result = {
  context : Context.t;
  fitness : Fitness.t;
  message : string option;
  max_operations_ttl : int;
  last_finalized_block_level : Int32.t;
  last_preserved_block_level : Int32.t;
}

val lift_legacy_validation_result :
  legacy_validation_result -> validation_result

type quota = {max_size : int; max_op : int option}

type rpc_context = {
  block_hash : Tezos_crypto.Hashed.Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

(** Type of semantics defining the context's hash present in a block
    header. *)
type header_context_hash_semantics =
  | Resulting_context
      (** The block header must contain the hash of the context
          resulting of the block's application. *)
  | Predecessor_resulting_context
      (** The block header must contain the hash of the context
          resulting of its predecessor block application. *)

val err_implementation_mismatch : expected:string -> got:string -> 'a
