(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**

     Frequently used data should be kept in memory and persisted along a
     chain of blocks. The caching mechanism allows the economic protocol
     to declare such data and to rely on a Least Recently Used strategy
     to keep the cache size under a fixed limit.

     Take a look at {!Environment_cache} and {!Environment_context}
     for additional implementation details about the protocol cache.

     The protocol has two main kinds of interaction with the cache:

     1. It is responsible for setting up the cache with appropriate
        parameter values and callbacks. It must also compute cache nonces
        to give the shell enough information to properly synchronize the
        in-memory cache with the block contexts and protocol upgrades.
        A typical place where this happens is {!Apply}.
        This aspect must be implemented using {!Cache.Admin}.

     2. It can exploit the cache to retrieve, to insert, and to update
        cached values from the in-memory cache. The basic idea is to
        avoid recomputing values from scratch at each block when they are
        frequently used. {!Script_cache} is an example of such usage.
        This aspect must be implemented using {!Cache.Interface}.

  *)

(** Size for subcaches and values of the cache. *)
type size = int

(** Index type to index caches. *)
type index = int

(** Type used to identifies the block that introduced new cache
     entries *)
type cache_nonce

(**

     The following module acts on the whole cache, not on a specific
     sub-cache, unlike {!Interface}. It is used to administrate the
     protocol cache, e.g., to maintain the cache in a consistent state
     with respect to the chain. This module is typically used by
     low-level layers of the protocol and by the shell.

*)
module Admin : sig
  (** A key uniquely identifies a cached [value] in some subcache. *)
  type key

  (** Cached values. *)
  type value

  (** [pp fmt ctxt] is a pretty printer for the [cache] of [ctxt]. *)
  val pp : Format.formatter -> Raw_context.t -> unit

  (** [sync ctxt cache_nonce] updates the context with the domain of
     the cache computed so far. Such function is expected to be called
     at the end of the validation of a block, when there is no more
     accesses to the cache.

     [cache_nonce] identifies the block that introduced new cache
     entries. The nonce should identify uniquely the block which
     modifies this value. It cannot be the block hash for circularity
     reasons: The value of the nonce is stored onto the context and
     consequently influences the context hash of the very same
     block. Such nonce cannot be determined by the shell and its
     computation is delegated to the economic protocol. *)
  val sync : Raw_context.t -> cache_nonce -> Raw_context.t Lwt.t

  (** {3 Cache helpers for RPCs} *)

  (** [future_cache_expectation ?blocks_before_activation ctxt
     ~time_in_blocks] returns [ctxt] except that the entries of the
     caches that are presumably too old to still be in the caches in
     [n_blocks] are removed.

      This function is based on a heuristic. The context maintains the
     median of the number of removed entries: this number is multiplied
     by `n_blocks` to determine the entries that are likely to be
     removed in `n_blocks`.

     If [blocks_before_activation] is set to [Some n],
     then the cache is considered empty if [0 <= n <= time_in_blocks].
     Otherwise, if [blocks_before_activation] is set to [None] and
     if the voting period is the adoption, the cache is considered
     empty if [blocks <= time_in_blocks remaining for adoption phase]. *)
  val future_cache_expectation :
    ?blocks_before_activation:int32 ->
    Raw_context.t ->
    time_in_blocks:int ->
    Raw_context.t tzresult Lwt.t

  (** [cache_size ctxt ~cache_index] returns an overapproximation of
       the size of the cache. Returns [None] if [cache_index] is
       greater than the number of subcaches declared by the cache
       layout. *)
  val cache_size : Raw_context.t -> cache_index:int -> size option

  (** [cache_size_limit ctxt ~cache_index] returns the maximal size of
       the cache indexed by [cache_index]. Returns [None] if
       [cache_index] is greater than the number of subcaches declared
       by the cache layout. *)
  val cache_size_limit : Raw_context.t -> cache_index:int -> size option

  (** [value_of_key ctxt k] interprets the functions introduced by
     [register] to construct a cacheable value for a key [k].

     [value_of_key] is a maintenance operation: it is typically run
     when a node reboots. For this reason, this operation is not
     carbonated. *)
  val value_of_key :
    Raw_context.t -> Context.Cache.key -> Context.Cache.value tzresult Lwt.t
end

(** A client uses a unique namespace (represented as a string
     without '@') to avoid collision with the keys of other
     clients. *)
type namespace = private string

(** [create_namespace str] creates a valid namespace from [str]

    @raise Invalid_argument if [str] contains '@'
 *)
val create_namespace : string -> namespace

(** A key is fully determined by a namespace and an identifier. *)
type identifier = string

(**
     To use the cache, a client must implement the [CLIENT]
     interface.

  *)
module type CLIENT = sig
  (** The type of value to be stored in the cache. *)
  type cached_value

  (** The client must declare the index of the subcache where its
       values shall live. [cache_index] must be between [0] and
       [List.length Constants_repr.cache_layout - 1]. *)
  val cache_index : index

  (** The client must declare a namespace. This namespace must
        be unique. Otherwise, the program stops.
        A namespace cannot contain '@'. *)
  val namespace : namespace

  (** [value_of_identifier id] builds the cached value identified by
       [id]. This function is called when the subcache is loaded into
       memory from the on-disk representation of its domain.

       An error during the execution of this function is fatal as
       witnessed by its type: an error embedded in a [tzresult] is not
       supposed to be caught by the protocol. *)
  val value_of_identifier :
    Raw_context.t -> identifier -> cached_value tzresult Lwt.t
end

(**

     An [INTERFACE] to the subcache where keys live in a given [namespace].

  *)
module type INTERFACE = sig
  (** The type of value to be stored in the cache. *)
  type cached_value

  (** [update ctxt i (Some (e, size))] returns a context where the
       value [e] of given [size] is associated to identifier [i] in
       the subcache. If [i] is already in the subcache, the cache
       entry is updated.

        [update ctxt i None] removes [i] from the subcache. *)
  val update :
    Raw_context.t ->
    identifier ->
    (cached_value * size) option ->
    Raw_context.t tzresult

  (** [find ctxt i = Some v] if [v] is the value associated to [i]
       in the subcache. Returns [None] if there is no such value in
       the subcache. This function is in the Lwt monad because if the
       value may have not been constructed (see the lazy loading
       mode in {!Environment_context}), it is constructed on the fly. *)
  val find : Raw_context.t -> identifier -> cached_value option tzresult Lwt.t

  (** [list_identifiers ctxt] returns the list of the
       identifiers of the cached values along with their respective
       size. The returned list is sorted in terms of their age in the
       cache, the oldest coming first. *)
  val list_identifiers : Raw_context.t -> (string * int) list

  (** [identifier_rank ctxt identifier] returns the number of cached values
       older than the one of [identifier]; or, [None] if the [identifier] has
       no associated value in the subcache. *)
  val identifier_rank : Raw_context.t -> string -> int option

  (** [size ctxt] returns an overapproximation of the subcache size.
      Note that the size unit is subcache specific. *)
  val size : Raw_context.t -> int

  (** [size_limit ctxt] returns the maximal size of the subcache.
      Note that the size unit is subcache specific. *)
  val size_limit : Raw_context.t -> int
end

(** [register_exn client] produces an [Interface] specific to a
     given [client]. This function can fail if [client] does not
     respect the invariant declared in the documentation of
     {!CLIENT}. *)
val register_exn :
  (module CLIENT with type cached_value = 'a) ->
  (module INTERFACE with type cached_value = 'a)

(** [cache_nonce_from_block_header shell_header contents] computes a
   {!cache_nonce} from the [shell_header] and its [contents]. *)
val cache_nonce_from_block_header :
  Block_header_repr.shell_header -> Block_header_repr.contents -> cache_nonce
